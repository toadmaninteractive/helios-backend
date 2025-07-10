import { ChangeDetectionStrategy, Component, OnDestroy } from '@angular/core';
import { DomSanitizer, SafeUrl } from '@angular/platform-browser';
import { Router } from '@angular/router';
import { MatDialog, MatTableDataSource, PageEvent, Sort } from '@angular/material';
import { Observable, Subject, BehaviorSubject, Subscription, combineLatest } from 'rxjs';
import { debounceTime, distinctUntilChanged, filter, finalize, map, takeUntil, tap } from 'rxjs/operators';
import { AccountService } from '../../../../core/services/account.service';
import { NotificationService } from '../../../../core/services/notification.service';
import { HeliosAdminService } from '../../../../protocol/web-admin-protocol.service';
import { Constants } from '../../../../shared/config/constants';
import { GameCreateDialogComponent } from '../game-create-dialog/game-create-dialog.component';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

interface QueryArguments {
    needle: string;
    orderBy: WebProtocol.GameOrderBy;
    orderDir: WebProtocol.OrderDirection;
    limit: number;
    offset: number;
}

enum Column {
    Id = 'id',
    Title = 'title',
    Price = 'price',
    Status = 'status',
    Available = 'available',
    JiraKey = 'jira_key',
    CiUrl = 'ci_url',
    DiscordUrl = 'discord_url',
    CreatedAt = 'created_at',
    UpdatedAt = 'updated_at',
}

const DEFAULT_ORDER_BY = Column.Title,
    DEFAULT_ORDER_DIR = 'asc',
    DEFAULT_PAGE_SIZE = 10,
    DEFAULT_COLUMNS = [Column.Id, Column.Title, Column.Price, Column.Status, Column.Available, Column.JiraKey, Column.CiUrl, Column.DiscordUrl, Column.CreatedAt, Column.UpdatedAt];

@Component({
    selector: 'm-games-overview',
    templateUrl: './games-overview.component.html',
    styleUrls: ['./games-overview.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class GamesOverviewComponent implements OnDestroy {
    destroy$ = new Subject();
    profile$ = new BehaviorSubject<WebProtocol.PersonnelAccountProfile | null>(null);
    loading$ = new BehaviorSubject<boolean>(false);
    reload$ = new BehaviorSubject<boolean>(true);
    needle$ = new BehaviorSubject<string>('');
    sort$ = new BehaviorSubject<Sort>(<Sort> { active: DEFAULT_ORDER_BY, direction: DEFAULT_ORDER_DIR });
    page$ = new BehaviorSubject<PageEvent>(<PageEvent> { pageIndex: 0, pageSize: DEFAULT_PAGE_SIZE });
    total$ = new BehaviorSubject<number>(0);
    dataSource$ = new BehaviorSubject<MatTableDataSource<WebProtocol.Game>>(new MatTableDataSource<WebProtocol.Game>());
    subData: Subscription;
    displayedColumns = [...DEFAULT_COLUMNS];
    column = Column;
    pageSizes = [10, 25, 50, 100];
    args: QueryArguments;

    constructor(
        private router: Router,
        private sanitizer: DomSanitizer,
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
                    orderBy: WebProtocol.GameOrderBy.fromJson(sort.active || DEFAULT_ORDER_BY),
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

    private getData(args: QueryArguments): Observable<WebProtocol.Game[]> {
        return this.heliosAdminService
            .getGames(args.needle, args.orderBy, args.orderDir, args.offset, args.limit)
            .pipe(
                takeUntil(this.destroy$),
                tap((response: WebProtocol.CollectionSlice<WebProtocol.Game>) => this.total$.next(response.total)),
                map((response: WebProtocol.CollectionSlice<WebProtocol.Game>) => response.items),
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
                const dataSource = new MatTableDataSource<WebProtocol.Game>();
                dataSource.data = response;
                this.dataSource$.next(dataSource);
            });
    }

    gameStatus(game: WebProtocol.Game): string {
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

    gameBadgeClass(game: WebProtocol.Game): string {
        switch (this.gameStatus(game)) {
            case 'deleted':
                return 'm-timeline-3__item--danger';
            case 'disabled':
                return 'm-timeline-3__item--metal';
            case 'published':
                return 'm-timeline-3__item--success';
            case 'internal':
                return 'm-timeline-3__item--info';
            default:
                return 'm-timeline-3__item--warning';
        }
    }

    canShowSeleneIcon(game: WebProtocol.Game): boolean {
        const gameStatus = this.gameStatus(game);
        return gameStatus === 'internal' || gameStatus === 'published';
    }

    canShowToadmanLauncherIcon(game: WebProtocol.Game): boolean {
        return this.gameStatus(game) === 'published';
    }

    jiraUrl(jiraKey: string): SafeUrl {
        return this.sanitizer.bypassSecurityTrustUrl(`${Constants.jiraUrlPrefix}${jiraKey}`);
    }

    openGameCreateDialog(): void {
        const dialogRef = this.dialog.open(GameCreateDialogComponent, { width: '400px', data: {} });

        dialogRef.afterClosed()
            .pipe(takeUntil(this.destroy$))
            .subscribe(result => {
                if (result) {
                    this.notificationService.success('New game added');
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
}
