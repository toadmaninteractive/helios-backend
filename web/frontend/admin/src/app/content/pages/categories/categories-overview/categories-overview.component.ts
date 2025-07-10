import { ChangeDetectionStrategy, Component, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { MatDialog, MatTableDataSource, PageEvent, Sort } from '@angular/material';
import { Observable, Subject, BehaviorSubject, Subscription, combineLatest } from 'rxjs';
import { debounceTime, distinctUntilChanged, filter, finalize, map, takeUntil, tap } from 'rxjs/operators';
import { compare } from '../../../../shared/functions/compare';
import { AccountService } from '../../../../core/services/account.service';
import { NotificationService } from '../../../../core/services/notification.service';
import { HeliosAdminService } from '../../../../protocol/web-admin-protocol.service';
import { CategoryEditDialogComponent } from '../category-edit-dialog/category-edit-dialog.component';
import { CategoryEditDialogData } from '../category-edit-dialog/category-edit-dialog.interface';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

interface QueryArguments {
    needle: string;
    orderBy: Column;
    orderDir: WebProtocol.OrderDirection;
    limit: number;
    offset: number;
}

enum Column {
    Id = 'id',
    Name = 'name',
    Description = 'description',
    SortOrder = 'sort_order',
    CreatedAt = 'created_at',
    UpdatedAt = 'updated_at',
    Actions = 'actions'
}

const DEFAULT_ORDER_BY = Column.Id,
    DEFAULT_ORDER_DIR = 'asc',
    DEFAULT_PAGE_SIZE = 10,
    DEFAULT_COLUMNS = [Column.Id, Column.Name, Column.Description, Column.SortOrder, Column.CreatedAt, Column.UpdatedAt, Column.Actions];

@Component({
    selector: 'm-categories-overview',
    templateUrl: './categories-overview.component.html',
    styleUrls: ['./categories-overview.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class CategoriesOverviewComponent implements OnDestroy {
    destroy$ = new Subject();
    profile$ = new BehaviorSubject<WebProtocol.PersonnelAccountProfile | null>(null);
    loading$ = new BehaviorSubject<boolean>(false);
    reload$ = new BehaviorSubject<boolean>(true);
    needle$ = new BehaviorSubject<string>('');
    sort$ = new BehaviorSubject<Sort>(<Sort> { active: DEFAULT_ORDER_BY, direction: DEFAULT_ORDER_DIR });
    page$ = new BehaviorSubject<PageEvent>(<PageEvent> { pageIndex: 0, pageSize: DEFAULT_PAGE_SIZE });
    total$ = new BehaviorSubject<number>(0);
    dataSource$ = new BehaviorSubject<MatTableDataSource<WebProtocol.GameCategory>>(new MatTableDataSource<WebProtocol.GameCategory>());
    subData: Subscription;
    displayedColumns = [...DEFAULT_COLUMNS];
    column = Column;
    pageSizes = [10, 25, 50, 100];
    args: QueryArguments;

    constructor(
        private router: Router,
        private dialog: MatDialog,
        public accountService: AccountService,
        private notificationService: NotificationService,
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
                map(([reload, needle, sort, page]) => <QueryArguments>{
                    needle: needle,
                    orderBy: sort.active || DEFAULT_ORDER_BY,
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
    }

    private getData(args: QueryArguments): Observable<WebProtocol.GameCategory[]> {
        return this.heliosAdminService
            .getGameCategories()
            .pipe(
                takeUntil(this.destroy$),
                map((response: WebProtocol.CollectionSlice<WebProtocol.GameCategory>) => response.items),
                map((items: WebProtocol.GameCategory[]) => this.sortAndFilter(args, items)),
                tap((items: WebProtocol.GameCategory[]) => this.total$.next(items.length)),
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
                const dataSource = new MatTableDataSource<WebProtocol.GameCategory>();
                dataSource.data = response;
                this.dataSource$.next(dataSource);
            });
    }

    openCategoryEditDialog(category?: WebProtocol.GameCategory): void {
        const dialogRef = this.dialog.open(CategoryEditDialogComponent, {
            width: '400px', data: <CategoryEditDialogData> { category: category }
        });

        dialogRef.afterClosed()
            .pipe(takeUntil(this.destroy$))
            .subscribe(result => {
                if (result) {
                    const message: string = category ? 'Game category updated' : 'New game category added';
                    this.notificationService.success(message);
                    this.reload$.next(true);
                }
            });
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

    onDeleteGameCategory(categoryId: number): void {
        this.heliosAdminService
            .deleteGameCategory(categoryId)
            .pipe(takeUntil(this.destroy$))
            .subscribe(result => {
                if (result) {
                    this.notificationService.warning(`Game category deleted`);
                    this.reload$.next(true);
                }
            });
    }

    sortAndFilter(args: QueryArguments, categories: WebProtocol.GameCategory[]): WebProtocol.GameCategory[] {
        const sorted = categories.sort((a: WebProtocol.GameCategory, b: WebProtocol.GameCategory) => {
            const isAsc = args.orderDir === WebProtocol.OrderDirection.Asc;

            switch (args.orderBy) {
                case Column.Id: return compare(a.id, b.id, isAsc);
                case Column.Name: return compare(a.name, b.name, isAsc);
                case Column.Description: return compare(a.description, b.description, isAsc);
                case Column.SortOrder: return compare(a.sortOrder, b.sortOrder, isAsc);
                case Column.CreatedAt: return compare(a.createdAt.getTime(), b.createdAt.getTime(), isAsc);
                case Column.UpdatedAt: return compare(a.updatedAt.getTime(), b.updatedAt.getTime(), isAsc);
                default: return 0;
            }
        });

        const trimmedNeedle = args.needle.toLocaleLowerCase().trim() || '';

        return sorted.filter(category => {
            if (!trimmedNeedle)
                return true;

            return `${category.id}`.indexOf(trimmedNeedle) !== -1
                || category.name.toLocaleLowerCase().indexOf(trimmedNeedle) !== -1
                || category.description.toLocaleLowerCase().indexOf(trimmedNeedle) !== -1;
        });
    }
}
