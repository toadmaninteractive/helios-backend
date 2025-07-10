import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnInit, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { MatTableDataSource, PageEvent, Sort } from '@angular/material';
import { BehaviorSubject, combineLatest, Observable, Subject, Subscription } from 'rxjs';
import { debounceTime, distinctUntilChanged, filter, finalize, map, switchMap, takeUntil, tap } from 'rxjs/operators';
import { SubheaderService } from '../../../../core/services/metronic/layout/subheader.service';
import { AccountService } from '../../../../core/services/account.service';
import { ClipboardService } from '../../../../core/services/clipboard.service';
import { HeliosAdminService } from '../../../../protocol/web-admin-protocol.service';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

interface QueryArguments {
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
    GameId = 'game_id',
    GameTitle = 'game_title',
    GroupRoles = 'group_roles',
    UserRole = 'user_role',
    GameBranches = 'game_branches',
    EffectiveRole = 'effective_role',
}

const DEFAULT_ORDER_BY = Column.GameId,
    DEFAULT_ORDER_DIR = 'asc',
    DEFAULT_PAGE_SIZE = 10,
    DEFAULT_COLUMNS = [Column.GameId, Column.GameTitle, Column.GroupRoles, Column.UserRole, Column.GameBranches, Column.EffectiveRole];

@Component({
    selector: 'm-profile-acl',
    templateUrl: './profile-acl.component.html',
    styleUrls: ['./profile-acl.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ProfileAclComponent implements OnInit, OnDestroy {
    destroy$ = new Subject<any>();
    loading$ = new BehaviorSubject<boolean>(false);
    reload$ = new BehaviorSubject<boolean>(true);
    needle$ = new BehaviorSubject<string>('');
    sort$ = new BehaviorSubject<Sort>(<Sort>{ active: DEFAULT_ORDER_BY, direction: DEFAULT_ORDER_DIR });
    page$ = new BehaviorSubject<PageEvent>(<PageEvent>{ pageIndex: 0, pageSize: DEFAULT_PAGE_SIZE });
    total$ = new BehaviorSubject<number>(0);
    dataSource$ = new BehaviorSubject<MatTableDataSource<WebProtocol.PersonnelAccountRole>>(new MatTableDataSource<WebProtocol.PersonnelAccountRole>());
    subData: Subscription;
    displayedColumns = [...DEFAULT_COLUMNS];
    column = Column;
    accessRole = WebProtocol.AccessRole;
    pageSizes = [10, 25, 50, 100];
    args: QueryArguments;

    constructor(
        private router: Router,
        private cdr: ChangeDetectorRef,
        public accountService: AccountService,
        private clipboardService: ClipboardService,
        private subheaderService: SubheaderService,
        private heliosAdminService: HeliosAdminService,
    ) { }

    ngOnInit(): void {
        this.accountService.profile$.asObservable()
            .pipe(
                takeUntil(this.destroy$),
                filter(profile => profile instanceof WebProtocol.PersonnelAccountProfile),
            )
            .subscribe(profile => {
                this.subheaderService.setTitle(profile.name);

                const role = profile.isSuperadmin ? 'Super Administrator' : profile.isGameManager ? 'Maintainer' : null,
                    badgeClass = profile.isSuperadmin ? 'm-badge--warning' : profile.isGameManager ? 'm-badge--info' : null;

                if (role && badgeClass) {
                    this.subheaderService.setStatus(role, badgeClass);
                }
            });

        combineLatest(
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
                map(([reload, needle, sort, page]) => <QueryArguments>{
                    needle: needle,
                    orderBy: WebProtocol.PersonnelAccountRoleOrderBy.fromJson(sort.active || DEFAULT_ORDER_BY),
                    orderDir: WebProtocol.OrderDirection.fromJson(sort.direction || DEFAULT_ORDER_DIR),
                    limit: page.pageSize,
                    offset: page.pageIndex * page.pageSize,
                }),
                filter(args => {
                    const shouldRestart = !(this.args
                        && this.args.needle === args.needle
                        && this.args.orderBy === args.orderBy
                        && this.args.orderDir === args.orderDir
                        && this.args.limit === args.limit);

                    this.args = args;

                    if (shouldRestart) {
                        this.page$.next(<PageEvent>{ pageSize: args.limit, pageIndex: 0 });
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
            .getMyPersonnelAccountRoles(args.needle, args.orderBy, args.orderDir, args.offset, args.limit)
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
            result = <EffectiveRole>{ role: null, groupName: null };

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
}
