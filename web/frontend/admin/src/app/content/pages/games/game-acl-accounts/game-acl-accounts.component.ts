import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnDestroy } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { BehaviorSubject, combineLatest, Observable, Subject, Subscription } from 'rxjs';
import { debounceTime, distinctUntilChanged, filter, finalize, map, switchMap, takeUntil, tap } from 'rxjs/operators';
import { MatTableDataSource, PageEvent, Sort } from '@angular/material';
import { AlertMessage } from '../../../../shared/interfaces/alert-message';
import { Breadcrumb } from '../../../../shared/interfaces/breadcrumb';
import { SubheaderService } from '../../../../core/services/metronic/layout/subheader.service';
import { ClipboardService } from '../../../../core/services/clipboard.service';
import { NotificationService } from '../../../../core/services/notification.service';
import { HeliosAdminService } from '../../../../protocol/web-admin-protocol.service';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

interface QueryArguments {
    gameId: string;
    needle: string;
    orderBy: WebProtocol.PersonnelAccountRoleOrderBy;
    orderDir: WebProtocol.OrderDirection;
    limit: number;
    offset: number;
}

interface EffectiveRole {
    role: WebProtocol.AccessRole | null;
    groupName: string | null;
}

enum Column {
    Username = 'username',
    GroupRoles = 'group_roles',
    UserRole = 'user_role',
    GameBranches = 'game_branches',
    EffectiveRole = 'effective_role',
}

const DEFAULT_ORDER_BY = Column.UserRole,
    DEFAULT_ORDER_DIR = 'desc',
    DEFAULT_PAGE_SIZE = 10,
    DEFAULT_COLUMNS = [Column.Username, Column.GroupRoles, Column.UserRole, Column.GameBranches, Column.EffectiveRole];

@Component({
    selector: 'm-game-acl-accounts',
    templateUrl: './game-acl-accounts.component.html',
    styleUrls: ['./game-acl-accounts.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class GameAclAccountsComponent implements OnDestroy {
    public config: any;
    destroy$ = new Subject();
    loading$ = new BehaviorSubject<boolean>(false);
    game$ = new BehaviorSubject<WebProtocol.Game>(null);
    alert$ = new BehaviorSubject<AlertMessage | null>(null);
    reload$ = new BehaviorSubject<boolean>(true);
    needle$ = new BehaviorSubject<string>('');
    sort$ = new BehaviorSubject<Sort>(<Sort> { active: DEFAULT_ORDER_BY, direction: DEFAULT_ORDER_DIR });
    page$ = new BehaviorSubject<PageEvent>(<PageEvent> { pageIndex: 0, pageSize: DEFAULT_PAGE_SIZE });
    total$ = new BehaviorSubject<number>(0);
    dataSource$ = new BehaviorSubject<MatTableDataSource<WebProtocol.PersonnelAccountRole>>(new MatTableDataSource<WebProtocol.PersonnelAccountRole>());
    subData: Subscription;
    displayedColumns = [...DEFAULT_COLUMNS];
    column = Column;
    accessRole = WebProtocol.AccessRole;
    pageSizes = [10, 25, 50, 100];
    args: QueryArguments;

    constructor(
        private activatedRoute: ActivatedRoute,
        private router: Router,
        private cdr: ChangeDetectorRef,
        private clipboardService: ClipboardService,
        private subheaderService: SubheaderService,
        private heliosAdminService: HeliosAdminService,
        private notificationService: NotificationService,
    ) {
        combineLatest(
            this.activatedRoute.paramMap.pipe(
                map(params => params.get('id')),
                distinctUntilChanged(),
                switchMap(gameId => this.heliosAdminService.getGame(gameId)),
                tap(game => this.updatePageStatus(game)),
            ),
            this.reload$.asObservable(),
            this.needle$.asObservable().pipe(
                distinctUntilChanged(),
                debounceTime(450),
            ),
            this.sort$.asObservable(),
            this.page$.asObservable(),
        )
            .pipe(
                takeUntil(this.destroy$),
                map(([game, reload, needle, sort, page]) => <QueryArguments> {
                    gameId: game.id,
                    needle: needle,
                    orderBy: WebProtocol.PersonnelAccountRoleOrderBy.fromJson(sort.active || DEFAULT_ORDER_BY),
                    orderDir: WebProtocol.OrderDirection.fromJson(sort.direction || DEFAULT_ORDER_DIR),
                    limit: page.pageSize,
                    offset: page.pageIndex * page.pageSize,
                }),
                filter(args => {
                    const shouldRestart = !(this.args
                        && this.args.gameId === args.gameId
                        && this.args.needle === args.needle
                        && this.args.orderBy === args.orderBy
                        && this.args.orderDir === args.orderDir
                        && this.args.limit === args.limit);

                    this.args = args;

                    if (shouldRestart) {
                        this.page$.next(<PageEvent> { pageSize: args.limit, pageIndex: 0 });
                    }

                    return !shouldRestart;
                }),
            )
            .subscribe(args => this.loadItems(args));
    }

    ngOnDestroy(): void {
        if (this.subData instanceof Subscription) {
            this.subData.unsubscribe();
            this.subData = null;
        }

        this.destroy$.next();
        this.destroy$.complete();
        this.loading$.complete();
    }

    private getData(args: QueryArguments): Observable<WebProtocol.PersonnelAccountRole[]> {
        return this.heliosAdminService
            .getPersonnelAccountRolesForGame(args.gameId, args.needle, args.orderBy, args.orderDir, args.offset, args.limit)
            .pipe(
                takeUntil(this.destroy$),
                tap((response: WebProtocol.CollectionSlice<WebProtocol.PersonnelAccountRole>) => this.total$.next(response.total)),
                map((response: WebProtocol.CollectionSlice<WebProtocol.PersonnelAccountRole>) => response.items),
            );
    }

    loadItems(args: QueryArguments): void {
        // Show loading indicator
        this.loading$.next(true);

        if (this.subData instanceof Subscription) {
            this.subData.unsubscribe();
            this.subData = null;
        }

        // Load items
        this.subData = this.getData(args)
            .pipe(
                takeUntil(this.destroy$),
                map(response => {
                    response = response.map(item => {
                        const effective = this.effectiveRole(item.userRole, item.groupRoles),
                            isGlobalConsumer = item.userRole === WebProtocol.AccessRole.Consumer && item.isGlobal,
                            isGlobalGroupConsumer = Object.values(item.groupRoles)
                                .filter(obj => WebProtocol.AccessRole.fromJson(obj['role']) === WebProtocol.AccessRole.Consumer && obj['is_global']).length > 0;

                        item['effectiveRole'] = effective.role;
                        item['effectiveGroup'] = effective.groupName;
                        item['effectiveAllBranches'] = effective.role >= WebProtocol.AccessRole.Uploader || isGlobalGroupConsumer || isGlobalConsumer;
                        item['effectiveBranches'] = this.effectiveBranches(item);

                        return item;
                    });

                    return response;
                }),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe(response => {
                const dataSource = new MatTableDataSource<WebProtocol.PersonnelAccountRole>();
                dataSource.data = response;
                this.dataSource$.next(dataSource);
            });
    }

    roleName(role: string | WebProtocol.AccessRole | any): string {
        const actualRole = typeof role === 'string' ? WebProtocol.AccessRole.fromJson(role) : role;
        return WebProtocol.AccessRole.getDescription(actualRole);
    }

    private gameStatus(game: WebProtocol.Game): string {
        if (game.isDeleted) {
            return 'deleted';
        } else if (game.isDisabled) {
            return 'disabled';
        } else if (game.isPublished) {
            return 'published';
        } else {
            return 'internal';
        }
    }

    private gameBadgeClass(game: WebProtocol.Game): string {
        switch (this.gameStatus(game)) {
            case 'deleted': return 'm-badge--danger';
            case 'disabled': return 'm-badge--disabled';
            case 'published': return 'm-badge--success';
            case 'internal': return 'm-badge--info';
            default: return 'm-badge--warning';
        }
    }

    private mostPrivilegedGroup(groupRoles: any): string | null {
        const roles = groupRoles || {},
            groups = Object.keys(roles);

        return groups.length > 0
            ? groups.sort((a, b) => WebProtocol.AccessRole.fromJson(roles[a].role) > WebProtocol.AccessRole.fromJson(roles[b].role) ? -1 : 1)[0]
            : null;
    }

    private effectiveRole(userRole: WebProtocol.AccessRole, groupRoles: any): EffectiveRole {
        const privilegedGroup = this.mostPrivilegedGroup(groupRoles),
            groupRole = privilegedGroup ? WebProtocol.AccessRole.fromJson(groupRoles[privilegedGroup].role) : null,
            groupOverridesUser = (groupRole || 0) > (userRole || 0),
            result = <EffectiveRole> { role: null, groupName: null };

        if (!userRole && !privilegedGroup) {
            return result;
        }

        result.role = groupOverridesUser ? groupRole : userRole;
        result.groupName = groupOverridesUser ? privilegedGroup : null;

        return result;
    }

    private effectiveBranches(item: WebProtocol.PersonnelAccountRole): string[] {
        const branchIdSet = new Set<number>();

        Object.values(item.groupRoles)
            .filter(obj => WebProtocol.AccessRole.fromJson(obj['role']) === WebProtocol.AccessRole.Consumer)
            .forEach(obj => obj['branch_ids'].forEach(branchId => branchIdSet.add(branchId)));

        if (item.userRole === WebProtocol.AccessRole.Consumer) {
            item.branchIds.forEach(branchId => branchIdSet.add(branchId));
        }

        const branchMap = new Map<number, string>();
        Object.keys(item.gameBranches).forEach(branchName => branchMap.set(item.gameBranches[branchName], branchName));

        return [...branchIdSet]
            .map(branchId => branchMap.has(branchId) ? branchMap.get(branchId) : `#${branchId}`)
            .sort((a, b) => a > b ? 1 : -1);
    }

    updatePageStatus(game: WebProtocol.Game): void {
        const breadcrumbs = [
            <Breadcrumb>{ title: 'Games', page: '/games' },
            <Breadcrumb>{ title: game.title, page: `/games/${game.id}/manage` },
            <Breadcrumb>{ title: 'Access Control', page: `/games/${game.id}/acl` },
            <Breadcrumb>{ title: 'Accounts', page: `/games/${game.id}/acl/accounts` },
        ];

        this.alert$.next(null);
        this.game$.next(game);
        this.subheaderService.setTitle(game.title);
        this.subheaderService.setBreadcrumbs(breadcrumbs);
        this.subheaderService.setStatus(this.gameStatus(game), this.gameBadgeClass(game));
    }

    onNeedleChange(needle: string): void {
        this.needle$.next((needle || '').trim());
    }

    onSortChange(sort: Sort): void {
        this.sort$.next(sort);
    }

    onPageChange(page: PageEvent): void {
        this.page$.next(page);
    }

    onReload(): void {
        this.reload$.next(true);
    }

    onRoleChange(obj: WebProtocol.PersonnelAccountRole, role: WebProtocol.AccessRole): void {
        if (obj.userRole === role) {
            return;
        }

        this.setPersonnelAccountRole(obj.personnelId, obj.gameId, role)
            .subscribe(response => {
                this.reload$.next(true);
                this.notificationService.success('Role changed');
            });
    }

    onRoleReset(obj: WebProtocol.PersonnelAccountRole): void {
        if (!obj.userRole) {
            return;
        }

        this.resetPersonnelAccountRole(obj.personnelId, obj.gameId)
            .subscribe(response => {
                this.reload$.next(true);
                this.notificationService.success('Role changed');
            });
    }

    onGlobalBranchesChange(obj: WebProtocol.PersonnelAccountRole, checked: boolean): void {
        this.setPersonnelAccountRole(obj.personnelId, obj.gameId, obj.userRole, checked, [])
            .subscribe(response => {
                this.reload$.next(true);
                this.notificationService.success('Game branch access updated');
            });
    }

    onBranchChange(obj: WebProtocol.PersonnelAccountRole, branchId: number, checked: boolean): void {
        const branchIndex = obj.branchIds.indexOf(branchId);
        let branchIds: number[] = [];

        if (checked && branchIndex === -1) {
            branchIds = [...obj.branchIds, branchId];
        } else if (!checked && branchIndex !== -1) {
            branchIds = obj.branchIds.filter(id => id !== branchId);
        }

        this.setPersonnelAccountRole(obj.personnelId, obj.gameId, obj.userRole, false, branchIds)
            .subscribe(response => {
                this.reload$.next(true);
                this.notificationService.success('Game branch access updated');
            });
    }

    private setPersonnelAccountRole(personnelId: number, gameId: string, role: WebProtocol.AccessRole, isGlobal: boolean | null = null, branchIds: number[] | null = null): Observable<WebProtocol.GenericResponse> {
        const request = new WebProtocol.AccessRoleUpdateRequest();
        if (typeof isGlobal === 'boolean') request.isGlobal = isGlobal;
        if (branchIds instanceof Array) request.branchIds = branchIds;
        request.role = role;

        return this.heliosAdminService
            .setPersonnelAccountRole(request, personnelId, gameId)
            .pipe(takeUntil(this.destroy$));
    }

    private resetPersonnelAccountRole(personnelId: number, gameId: string): Observable<WebProtocol.GenericResponse> {
        return this.heliosAdminService
            .resetPersonnelAccountRole(personnelId, gameId)
            .pipe(takeUntil(this.destroy$));
    }
}
