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
    orderBy: WebProtocol.PersonnelGroupRoleOrderBy;
    orderDir: WebProtocol.OrderDirection;
    limit: number;
    offset: number;
}

enum Column {
    GroupName = 'group_name',
    GroupRole = 'group_role',
    GameBranches = 'game_branches',
}

const DEFAULT_ORDER_BY = Column.GroupRole,
    DEFAULT_ORDER_DIR = 'desc',
    DEFAULT_PAGE_SIZE = 10,
    DEFAULT_COLUMNS = [Column.GroupName, Column.GroupRole, Column.GameBranches];

@Component({
    selector: 'm-game-acl-groups',
    templateUrl: './game-acl-groups.component.html',
    styleUrls: ['./game-acl-groups.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class GameAclGroupsComponent implements OnDestroy {
    public config: any;
    destroy$ = new Subject();
    loading$ = new BehaviorSubject<boolean>(false);
    game$ = new BehaviorSubject<WebProtocol.Game>(null);
    filteredMembers$ = new BehaviorSubject<string[]>([]);
    alert$ = new BehaviorSubject<AlertMessage | null>(null);
    reload$ = new BehaviorSubject<boolean>(true);
    needle$ = new BehaviorSubject<string>('');
    memberNeedle$ = new BehaviorSubject<string>('');
    sort$ = new BehaviorSubject<Sort>(<Sort> { active: DEFAULT_ORDER_BY, direction: DEFAULT_ORDER_DIR });
    page$ = new BehaviorSubject<PageEvent>(<PageEvent> { pageIndex: 0, pageSize: DEFAULT_PAGE_SIZE });
    total$ = new BehaviorSubject<number>(0);
    dataSource$ = new BehaviorSubject<MatTableDataSource<WebProtocol.PersonnelGroupRole>>(new MatTableDataSource<WebProtocol.PersonnelGroupRole>());
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
                    orderBy: WebProtocol.PersonnelGroupRoleOrderBy.fromJson(sort.active || DEFAULT_ORDER_BY),
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

    private getData(args: QueryArguments): Observable<WebProtocol.PersonnelGroupRole[]> {
        return this.heliosAdminService
            .getPersonnelGroupRolesForGame(args.gameId, args.needle, args.orderBy, args.orderDir, args.offset, args.limit)
            .pipe(
                takeUntil(this.destroy$),
                tap((response: WebProtocol.CollectionSlice<WebProtocol.PersonnelGroupRole>) => this.total$.next(response.total)),
                map((response: WebProtocol.CollectionSlice<WebProtocol.PersonnelGroupRole>) => response.items),
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
                finalize(() => this.loading$.next(false)),
            )
            .subscribe(response => {
                const dataSource = new MatTableDataSource<WebProtocol.PersonnelGroupRole>();
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

    updatePageStatus(game: WebProtocol.Game): void {
        const breadcrumbs = [
            <Breadcrumb>{ title: 'Games', page: '/games' },
            <Breadcrumb>{ title: game.title, page: `/games/${game.id}/manage` },
            <Breadcrumb>{ title: 'Access Control', page: `/games/${game.id}/acl` },
            <Breadcrumb>{ title: 'Groups', page: `/games/${game.id}/acl/groups` },
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

    onMemberNeedleChange(needle: string): void {
        this.memberNeedle$.next((needle || '').trim());
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

    onRoleChange(obj: WebProtocol.PersonnelGroupRole, role: WebProtocol.AccessRole): void {
        if (obj.groupRole === role) {
            return;
        }

        this.setPersonnelGroupRole(obj.groupId, obj.gameId, role)
            .subscribe(response => {
                this.reload$.next(true);
                this.notificationService.success('Role changed');
            });
    }

    onRoleReset(obj: WebProtocol.PersonnelGroupRole): void {
        if (!obj.groupRole) {
            return;
        }

        this.resetPersonnelGroupRole(obj.groupId, obj.gameId)
            .subscribe(response => {
                this.reload$.next(true);
                this.notificationService.success('Role changed');
            });
    }

    onGlobalBranchesChange(obj: WebProtocol.PersonnelGroupRole, checked: boolean): void {
        this.setPersonnelGroupRole(obj.groupId, obj.gameId, obj.groupRole, checked, [])
            .subscribe(response => {
                this.reload$.next(true);
                this.notificationService.success('Game branch access updated');
            });
    }

    onBranchChange(obj: WebProtocol.PersonnelGroupRole, branchId: number, checked: boolean): void {
        const branchIndex = obj.branchIds.indexOf(branchId);
        let branchIds: number[] = [];

        if (checked && branchIndex === -1) {
            branchIds = [...obj.branchIds, branchId];
        } else if (!checked && branchIndex !== -1) {
            branchIds = obj.branchIds.filter(id => id !== branchId);
        }

        this.setPersonnelGroupRole(obj.groupId, obj.gameId, obj.groupRole, false, branchIds)
            .subscribe(response => {
                this.reload$.next(true);
                this.notificationService.success('Game branch access updated');
            });
    }

    private setPersonnelGroupRole(groupId: number, gameId: string, role: WebProtocol.AccessRole, isGlobal: boolean | null = null, branchIds: number[] | null = null): Observable<WebProtocol.GenericResponse> {
        const request = new WebProtocol.AccessRoleUpdateRequest();
        if (typeof isGlobal === 'boolean') request.isGlobal = isGlobal;
        if (branchIds instanceof Array) request.branchIds = branchIds;
        request.role = role;

        return this.heliosAdminService
            .setPersonnelGroupRole(request, groupId, gameId)
            .pipe(takeUntil(this.destroy$));
    }

    private resetPersonnelGroupRole(groupId: number, gameId: string): Observable<WebProtocol.GenericResponse> {
        return this.heliosAdminService
            .resetPersonnelGroupRole(groupId, gameId)
            .pipe(takeUntil(this.destroy$));
    }
}
