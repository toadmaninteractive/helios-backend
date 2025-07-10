import { ChangeDetectionStrategy, Component, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { MatTableDataSource, PageEvent, Sort } from '@angular/material';
import { Observable, Subject, combineLatest, BehaviorSubject, Subscription } from 'rxjs';
import { debounceTime, distinctUntilChanged, filter, finalize, map, takeUntil, tap } from 'rxjs/operators';
import { HeliosAdminService } from '../../../../protocol/web-admin-protocol.service';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

interface QueryArguments {
    needle: string;
    orderBy: WebProtocol.PersonnelAccountOrderBy;
    orderDir: WebProtocol.OrderDirection;
    limit: number;
    offset: number;
}

enum Column {
    Id = 'id',
    Username = 'username',
    IsBlocked = 'is_blocked',
    IsDeleted = 'is_deleted',
    CreatedAt = 'created_at',
    UpdatedAt = 'updated_at',
}

const DEFAULT_ORDER_BY = Column.Id,
    DEFAULT_ORDER_DIR = 'asc',
    DEFAULT_PAGE_SIZE = 10,
    DEFAULT_COLUMNS = [Column.Id, Column.Username, Column.IsBlocked, Column.IsDeleted, Column.CreatedAt, Column.UpdatedAt];

@Component({
    selector: 'm-personnel-accounts',
    templateUrl: './personnel-accounts.component.html',
    styleUrls: ['./personnel-accounts.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PersonnelAccountsComponent implements OnDestroy {
    destroy$ = new Subject();
    loading$ = new BehaviorSubject<boolean>(false);
    reload$ = new BehaviorSubject<boolean>(true);
    needle$ = new BehaviorSubject<string>('');
    sort$ = new BehaviorSubject<Sort>(<Sort>{active: DEFAULT_ORDER_BY, direction: DEFAULT_ORDER_DIR});
    page$ = new BehaviorSubject<PageEvent>(<PageEvent>{pageIndex: 0, pageSize: DEFAULT_PAGE_SIZE});
    total$ = new BehaviorSubject<number>(0);
    dataSource$ = new BehaviorSubject<MatTableDataSource<WebProtocol.PersonnelAccount>>(new MatTableDataSource<WebProtocol.PersonnelAccount>());
    subData: Subscription;
    displayedColumns = [...DEFAULT_COLUMNS];
    column = Column;
    pageSizes = [10, 25, 50, 100];
    args: QueryArguments;

    constructor(
        private router: Router,
        private heliosAdminService: HeliosAdminService,
    ) {
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
                map(([reload, needle, sort, page]) => <QueryArguments> {
                    needle: needle,
                    orderBy: WebProtocol.PersonnelAccountOrderBy.fromJson(sort.active || DEFAULT_ORDER_BY),
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
    }

    private getData(args: QueryArguments): Observable<WebProtocol.PersonnelAccount[]> {
        return this.heliosAdminService
            .getPersonnelAccounts(args.needle, args.orderBy, args.orderDir, args.offset, args.limit)
            .pipe(
                takeUntil(this.destroy$),
                tap((response: WebProtocol.CollectionSlice<WebProtocol.PersonnelAccount>) => this.total$.next(response.total)),
                map((response: WebProtocol.CollectionSlice<WebProtocol.PersonnelAccount>) => response.items),
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
                const dataSource = new MatTableDataSource<WebProtocol.PersonnelAccount>();
                dataSource.data = response;
                this.dataSource$.next(dataSource);
            });
    }

    personnelAccountStatus(account: WebProtocol.PersonnelAccount): string {
        if (account.isDeleted) {
            return 'deleted';
        } else if (account.isBlocked) {
            return 'blocked';
        } else {
            return 'active';
        }
    }

    personnelAccountBadgeClass(account: WebProtocol.PersonnelAccount): string {
        switch (this.personnelAccountStatus(account)) {
            case 'deleted':
                return 'm-timeline-3__item--danger';
            case 'blocked':
                return 'm-timeline-3__item--warning';
            case 'active':
                return 'm-timeline-3__item--success';
            default:
                return 'm-timeline-3__item--metal';
        }
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
