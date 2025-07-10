import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnDestroy } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { MatDialog, MatTableDataSource, PageEvent, Sort } from '@angular/material';
import { BehaviorSubject, combineLatest, of, Subject, Subscription } from 'rxjs';
import { debounceTime, distinctUntilChanged, filter, map, switchMap, takeUntil, tap } from 'rxjs/operators';
import { compare } from '../../../../shared/functions/compare';
import { Breadcrumb } from '../../../../shared/interfaces/breadcrumb';
import { SubheaderService } from '../../../../core/services/metronic/layout/subheader.service';
import { ClipboardService } from '../../../../core/services/clipboard.service';
import { HeliosAdminService } from '../../../../protocol/web-admin-protocol.service';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

interface QueryArguments {
    filesRef: WebProtocol.GameFile[];
    needle: string;
    orderBy: Column;
    orderDir: WebProtocol.OrderDirection;
    limit: number;
    offset: number;
}

enum Column {
    RelativePath = 'relative_path',
    Size = 'size',
    CompressedSize = 'compressed_size',
    CompressionRatio = 'compression_ratio',
    SpaceSaving = 'space_saving',
}

const DEFAULT_ORDER_BY = Column.RelativePath,
    DEFAULT_ORDER_DIR = 'asc',
    DEFAULT_PAGE_SIZE = 25,
    DEFAULT_COLUMNS = [Column.RelativePath, Column.Size, Column.CompressedSize, Column.CompressionRatio, Column.SpaceSaving];

@Component({
    selector: 'm-build-overview',
    templateUrl: './build-overview.component.html',
    styleUrls: ['./build-overview.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class BuildOverviewComponent implements OnDestroy {
    public config: any;
    destroy$ = new Subject();
    loading$ = new BehaviorSubject<boolean>(false);
    loadingFiles$ = new BehaviorSubject<boolean>(false);
    role$ = new BehaviorSubject<WebProtocol.AccessRole | null>(null);
    game$ = new BehaviorSubject<WebProtocol.Game | null>(null);
    build$ = new BehaviorSubject<WebProtocol.Build | null>(null);
    files$ = new BehaviorSubject<WebProtocol.GameFile[] | null>(null);
    reload$ = new BehaviorSubject<boolean>(true);
    needle$ = new BehaviorSubject<string>('');
    sort$ = new BehaviorSubject<Sort>(<Sort>{ active: DEFAULT_ORDER_BY, direction: DEFAULT_ORDER_DIR });
    page$ = new BehaviorSubject<PageEvent>(<PageEvent>{ pageIndex: 0, pageSize: DEFAULT_PAGE_SIZE });
    total$ = new BehaviorSubject<number>(0);
    dataSource$ = new BehaviorSubject<MatTableDataSource<WebProtocol.GameFile>>(new MatTableDataSource<WebProtocol.GameFile>());
    timer: Subscription;
    accessRole = WebProtocol.AccessRole;
    buildId: number;
    args: QueryArguments;
    subData: Subscription;
    displayedColumns = [...DEFAULT_COLUMNS];
    column = Column;
    pageSizes = [10, 25, 50, 100];
    panelExpanded = false;

    constructor (
        private activatedRoute: ActivatedRoute,
        private router: Router,
        private cdr: ChangeDetectorRef,
        private dialog: MatDialog,
        private clipboardService: ClipboardService,
        private subheaderService: SubheaderService,
        private heliosAdminService: HeliosAdminService,
    ) {
        combineLatest(
            this.activatedRoute.data.pipe(
                map(data => data.role),
            ),
            this.activatedRoute.paramMap.pipe(
                distinctUntilChanged((a, b) => a.get('id') === b.get('id') && a.get('rev') === b.get('rev')),
            ),
        ).pipe(
            takeUntil(this.destroy$),
            tap(() => this.loading$.next(true)),
            switchMap(([role, paramMap]) => combineLatest(
                of(role),
                this.heliosAdminService.getGame(paramMap.get('id')),
                this.heliosAdminService.getBuildByRev(paramMap.get('id'), paramMap.get('rev')),
            )),
        ).subscribe(([role, game, build]) => {
            // Store data
            this.role$.next(role);
            this.game$.next(game);
            this.build$.next(build);
            this.buildId = build.id;

            // Update page status
            this.updatePageStatus(game, build);

            // Reset loading flag
            this.loading$.next(false);
        });

        this.build$
            .pipe(
                takeUntil(this.destroy$),
                filter(build => build instanceof WebProtocol.Build),
                tap(build => {
                    // Reset files
                    this.files$.next([]);
                }),
                filter(build => build instanceof WebProtocol.Build),
                tap(() => this.loadingFiles$.next(true)),
                switchMap(build => this.heliosAdminService.getBuildManifest(build.id)),
                tap(() => this.loadingFiles$.next(false)),
            )
            .subscribe(response => this.files$.next(response.files));

        combineLatest(
            this.build$.asObservable(),
            this.files$.asObservable(),
        )
            .pipe(
                takeUntil(this.destroy$),
                filter(([build, files]) => build instanceof WebProtocol.Build && files instanceof Array),
            )
            .subscribe(([build, files]) => {
                const exeFiles = files.filter(file => {
                    if (build.platform !== WebProtocol.Platform.Windows) {
                        return true;
                    }

                    const trimmedPath = file.relativePath.toLocaleLowerCase().trim();
                    return /\.((exe)|(cmd)|(bat))$/.test(trimmedPath);
                });
            });

        combineLatest(
            this.role$.asObservable(),
            this.files$.asObservable(),
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
                tap(([role, filesRef, reload, needle, sort, page]) => {
                    if (role < WebProtocol.AccessRole.Uploader) {
                        this.updateDataSource([]);
                    }
                }),
                map(([role, filesRef, reload, needle, sort, page]) => <QueryArguments>{
                    filesRef: filesRef,
                    needle: needle,
                    orderBy: sort.active || DEFAULT_ORDER_BY,
                    orderDir: WebProtocol.OrderDirection.fromJson(sort.direction || DEFAULT_ORDER_DIR),
                    limit: page.pageSize,
                    offset: page.pageIndex * page.pageSize,
                }),
                filter(args => {
                    const shouldRestart = !(this.args
                        && this.args.filesRef === args.filesRef
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
            .subscribe(args => {
                const sortedAndFiltered = this.sortAndFilter(args),
                    slice = sortedAndFiltered.slice(args.offset, args.offset + args.limit);

                this.total$.next(sortedAndFiltered.length);
                this.updateDataSource(slice);
            });
    }

    ngOnDestroy(): void {
        if (this.subData instanceof Subscription) {
            this.subData.unsubscribe();
            this.subData = null;
        }

        this.destroy$.next();
        this.destroy$.complete();
        this.loading$.complete();
        this.loadingFiles$.complete();
        this.role$.complete();
        this.game$.complete();
        this.build$.complete();
        this.files$.complete();
    }

    // Common
    updatePageStatus(game: WebProtocol.Game, build: WebProtocol.Build): void {
        this.subheaderService.setTitle(game.title);
        this.subheaderService.setStatus(this.gameStatus(game), this.gameBadgeClass(game));
        this.subheaderService.setBreadcrumbs([
            <Breadcrumb>{ title: 'Games', page: '/games' },
            <Breadcrumb>{ title: game.title, page: `/games/${game.id}/manage` },
            <Breadcrumb>{ title: `Build #${build.buildRev}`, page: `/games/${game.id}/builds/${build.buildRev}` },
        ]);

        this.cdr.detectChanges();
    }

    // Role-specific
    isAllowed(minRole: WebProtocol.AccessRole): boolean {
        return this.role$.getValue() >= minRole;
    }

    // Game specific
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
            case 'deleted': return 'm-badge--danger';
            case 'disabled': return 'm-badge--disabled';
            case 'published': return 'm-badge--success';
            case 'internal': return 'm-badge--info';
            default: return 'm-badge--warning';
        }
    }

    private updateDataSource(items: WebProtocol.GameFile[]): void {
        const dataSource = new MatTableDataSource<WebProtocol.GameFile>();
        dataSource.data = items;
        this.dataSource$.next(dataSource);
    }

    sortAndFilter(args: QueryArguments): WebProtocol.GameFile[] {
        const sorted = args.filesRef.sort((a: WebProtocol.GameFile, b: WebProtocol.GameFile) => {
            const isAsc = args.orderDir === WebProtocol.OrderDirection.Asc;

            switch (args.orderBy) {
                case Column.RelativePath: return compare(a.relativePath, b.relativePath, isAsc);
                case Column.Size: return compare(a.size, b.size, isAsc);
                case Column.CompressedSize: return compare(a.compressedSize, b.compressedSize, isAsc);
                default: return 0;
            }
        });

        const trimmedNeedle = args.needle.toLocaleLowerCase().trim() || '';

        return sorted.filter(file => {
            if (!trimmedNeedle)
                return true;

            return `${file.size}`.indexOf(trimmedNeedle) !== -1
                || file.relativePath.toLocaleLowerCase().indexOf(trimmedNeedle) !== -1;
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
