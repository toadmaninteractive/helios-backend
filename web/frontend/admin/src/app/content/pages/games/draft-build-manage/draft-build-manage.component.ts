import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnDestroy } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { MatDialog, MatTableDataSource, PageEvent, Sort } from '@angular/material';
import { BehaviorSubject, combineLatest, of, Subject, Subscription, timer } from 'rxjs';
import { debounceTime, distinctUntilChanged, filter, map, switchMap, takeUntil, takeWhile, tap } from 'rxjs/operators';
import { FileSystemFileEntry, NgxFileDropEntry } from 'ngx-file-drop';
import { compare } from '../../../../shared/functions/compare';
import { Breadcrumb } from '../../../../shared/interfaces/breadcrumb';
import { FileUploadDialogComponent } from '../file-upload-dialog/file-upload-dialog.component';
import { FileUploadDialogData } from '../file-upload-dialog/file-upload-dialog.interface';
import { SubheaderService } from '../../../../core/services/metronic/layout/subheader.service';
import { ClipboardService } from '../../../../core/services/clipboard.service';
import { NotificationService } from '../../../../core/services/notification.service';
import { HeliosAdminService } from '../../../../protocol/web-admin-protocol.service';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

interface QueryArguments {
    filesRef: WebProtocol.BuildFile[];
    needle: string;
    orderBy: Column;
    orderDir: WebProtocol.OrderDirection;
    limit: number;
    offset: number;
}

interface BuildPaths {
    exePath: string;
    logPath: string;
    crashReportPath: string;
    configPath: string;
    optionalFileMasks: string;
}

enum Column {
    Id = 'id',
    FilePath = 'file_path',
    FileSize = 'file_size',
    CompressedFilePath = 'compressed_file_path',
    CompressedFileSize = 'compressed_file_size',
    MD5 = 'md5',
    CreatedAt = 'created_at',
    UpdatedAt = 'updated_at',
    Actions = 'actions',
}

const DEFAULT_ORDER_BY = Column.FilePath,
    DEFAULT_ORDER_DIR = 'asc',
    DEFAULT_PAGE_SIZE = 10,
    DEFAULT_COLUMNS = [Column.Id, Column.FilePath, Column.FileSize, Column.CompressedFileSize, Column.CreatedAt, Column.Actions];

@Component({
    selector: 'm-draft-build-manage',
    templateUrl: './draft-build-manage.component.html',
    styleUrls: ['./draft-build-manage.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DraftBuildManageComponent implements OnDestroy {
    public config: any;
    destroy$ = new Subject();
    loading$ = new BehaviorSubject<boolean>(false);
    loadingFiles$ = new BehaviorSubject<boolean>(false);
    role$ = new BehaviorSubject<WebProtocol.AccessRole | null>(null);
    game$ = new BehaviorSubject<WebProtocol.Game | null>(null);
    build$ = new BehaviorSubject<WebProtocol.Build | null>(null);
    files$ = new BehaviorSubject<WebProtocol.BuildFile[] | null>(null);
    exeFiles$ = new BehaviorSubject<WebProtocol.BuildFile[]>([]);
    reload$ = new BehaviorSubject<boolean>(true);
    needle$ = new BehaviorSubject<string>('');
    sort$ = new BehaviorSubject<Sort>(<Sort>{ active: DEFAULT_ORDER_BY, direction: DEFAULT_ORDER_DIR });
    page$ = new BehaviorSubject<PageEvent>(<PageEvent>{ pageIndex: 0, pageSize: DEFAULT_PAGE_SIZE });
    total$ = new BehaviorSubject<number>(0);
    dataSource$ = new BehaviorSubject<MatTableDataSource<WebProtocol.BuildFile>>(new MatTableDataSource<WebProtocol.BuildFile>());
    timer: Subscription;
    accessRole = WebProtocol.AccessRole;
    buildId: number;
    buildPaths: BuildPaths;
    pristineBuildPaths: BuildPaths;
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
        private notificationService: NotificationService,
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

            // Set build paths
            this.updateBuildPaths(build.exePath, build.logPath, build.crashReportPath, build.configPath, build.optionalFileMasks);

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
                tap(build => {
                    if (build.isProcessing && !(this.timer instanceof Subscription)) {
                        this.monitorArchiveProcessingProgress(build.id);
                    } else if (!build.isProcessing && this.timer instanceof Subscription) {
                        this.timer.unsubscribe();
                        this.timer = null;
                    }
                }),
                filter(build => !build.isProcessing),
                tap(() => this.loadingFiles$.next(true)),
                switchMap(build => this.heliosAdminService.getBuildFiles(build.id)),
                tap(() => this.loadingFiles$.next(false)),
            )
            .subscribe(response => this.files$.next(response.items));

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

                const trimmedPath = file.filePath.toLocaleLowerCase().trim();
                return /\.((exe)|(cmd)|(bat))$/.test(trimmedPath);
            });

            this.exeFiles$.next(exeFiles);
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
        this.exeFiles$.complete();
    }

    // Common
    updatePageStatus(game: WebProtocol.Game, build: WebProtocol.Build): void {
        this.subheaderService.setTitle(game.title);
        this.subheaderService.setStatus(this.gameStatus(game), this.gameBadgeClass(game));
        this.subheaderService.setBreadcrumbs([
            <Breadcrumb>{ title: 'Games', page: '/games' },
            <Breadcrumb>{ title: game.title, page: `/games/${game.id}/manage` },
            <Breadcrumb>{ title: `Draft Build #${build.buildRev}`, page: `/games/${game.id}/builds/${build.buildRev}/draft` },
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
            case 'deleted':
                return 'm-badge--danger';
            case 'disabled':
                return 'm-badge--disabled';
            case 'published':
                return 'm-badge--success';
            case 'internal':
                return 'm-badge--info';
            default:
                return 'm-badge--warning';
        }
    }

    private updateDataSource(items: WebProtocol.BuildFile[]): void {
        const dataSource = new MatTableDataSource<WebProtocol.BuildFile>();
        dataSource.data = items;
        this.dataSource$.next(dataSource);
    }

    sortAndFilter(args: QueryArguments): WebProtocol.BuildFile[] {
        const sorted = args.filesRef.sort((a: WebProtocol.BuildFile, b: WebProtocol.BuildFile) => {
            const isAsc = args.orderDir === WebProtocol.OrderDirection.Asc;

            switch (args.orderBy) {
                case Column.Id: return compare(a.id, b.id, isAsc);
                case Column.FilePath: return compare(a.filePath, b.filePath, isAsc);
                case Column.FileSize: return compare(a.fileSize, b.fileSize, isAsc);
                case Column.CompressedFilePath: return compare(a.compressedFilePath, b.compressedFilePath, isAsc);
                case Column.CompressedFileSize: return compare(a.compressedFileSize, b.compressedFileSize, isAsc);
                case Column.MD5: return compare(a.md5, b.md5, isAsc);
                case Column.CreatedAt: return compare(a.createdAt.getTime(), b.createdAt.getTime(), isAsc);
                case Column.UpdatedAt: return compare(a.updatedAt.getTime(), b.updatedAt.getTime(), isAsc);
                default: return 0;
            }
        });

        const trimmedNeedle = args.needle.toLocaleLowerCase().trim() || '';

        return sorted.filter(file => {
            if (!trimmedNeedle)
                return true;

            return `${file.id}`.indexOf(trimmedNeedle) !== -1
                || `${file.fileSize}`.indexOf(trimmedNeedle) !== -1
                || file.filePath.toLocaleLowerCase().indexOf(trimmedNeedle) !== -1;
        });
    }

    isExePathValid(exePath: string): boolean {
        const files = this.files$.getValue();

        if (!(files && files.length > 0)) {
            return true;
        }

        let result = false;

        for (let index = 0; index < files.length; index++) {
            if (files[index].filePath === exePath) {
                result = true;
                break;
            }
        }

        return result;
    }

    buildPathsChanged(): boolean {
        if (!(this.buildPaths && this.pristineBuildPaths))
            return false;

        return this.buildPaths.exePath.trim() !== this.pristineBuildPaths.exePath.trim()
            || this.buildPaths.logPath.trim() !== this.pristineBuildPaths.logPath.trim()
            || this.buildPaths.crashReportPath.trim() !== this.pristineBuildPaths.crashReportPath.trim()
            || this.buildPaths.configPath.trim() !== this.pristineBuildPaths.configPath.trim()
            || this.buildPaths.optionalFileMasks.trim() !== this.pristineBuildPaths.optionalFileMasks.trim();
    }

    updateBuildPaths(exePath: string, logPath: string, crashReportPath: string, configPath: string, optionalFileMasks: string[]): void {
        const paths = <BuildPaths> {
            exePath: exePath || '',
            logPath: logPath || '',
            crashReportPath: crashReportPath || '',
            configPath: configPath || '',
            optionalFileMasks: (optionalFileMasks || []).join('\r\n'),
        };

        this.buildPaths = paths;
        this.pristineBuildPaths = {...paths};
        this.cdr.detectChanges();
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

    onFileDelete(file: WebProtocol.BuildFile): void {
        this.heliosAdminService
            .deleteBuildFile(this.buildId, file.id)
            .pipe(takeUntil(this.destroy$))
            .subscribe(result => {
                if (result) {
                    this.notificationService.warning('File deleted');

                    const files = this.files$.getValue(),
                        total = this.total$.getValue();

                    this.total$.next(total - 1);
                    this.files$.next(files.filter(f => f.filePath !== file.filePath));
                    this.reload$.next(true);
                }
            });
    }

    onDeleteBuildFiles(buildId: number): void {
        this.heliosAdminService
            .deleteAllBuildFiles(buildId)
            .pipe(takeUntil(this.destroy$))
            .subscribe(result => {
                if (result) {
                    this.notificationService.warning(`Build files were deleted`);
                    this.files$.next([]);
                }
            });
    }

    onProcessFolder(fileEntries: NgxFileDropEntry[]) {
        const pathSet = new Set<string>(),
            filePaths: Array<string> = [];

        for (const droppedFile of fileEntries) {
            if (droppedFile.fileEntry.isFile) {
                const fileEntry = droppedFile.fileEntry as FileSystemFileEntry;

                fileEntry.file((file: File) => {
                    const filePath = (file['webkitRelativePath'] || file.name).trim(),
                        parts = filePath.split('/');

                    filePaths.push(filePath);

                    if (parts.length > 1) {
                        parts.pop();

                        for (let i = 0; i < parts.length; i++) {
                            const subPath = parts.slice(0, i + 1).join('/');

                            if (subPath) {
                                pathSet.add(subPath);
                            }
                        }
                    }
                });
            }
        }

        const commonPath = [...pathSet]
            .filter(path => filePaths.filter(fp => fp.startsWith(`${path}/`)).length === filePaths.length)
            .sort((a, b) => a.length < b.length ? 1 : -1)[0];

        const stripPrefix = commonPath ? `${commonPath}/` : commonPath;
        this.openFileUploadDialog(this.buildId, fileEntries, stripPrefix, false);
    }

    onProcessArchive(fileEntries: NgxFileDropEntry[]): void {
        this.openFileUploadDialog(this.buildId, fileEntries, '', true);
    }

    onDraftBuildUpdate(): void {
        if (!this.buildPathsChanged()) {
            return;
        }

        const build = this.build$.getValue();
        this.updateDraftBuild(this.buildId, build.rev);
    }

    openFileUploadDialog(buildId: number, fileEntries: NgxFileDropEntry[], stripPrefix: string, isArchive: boolean): void {
        const data = <FileUploadDialogData> { buildId: buildId, fileEntries: fileEntries, stripPrefix: stripPrefix, isArchive: isArchive },
            dialogRef = this.dialog.open(FileUploadDialogComponent, { width: '850px', data: data, /* disableClose: true */ });

        dialogRef.afterClosed()
            .pipe(takeUntil(this.destroy$))
            .subscribe(result => {
                this.notificationService.success(`${isArchive ? 'Archive' : 'Files'} uploaded`);

                if (isArchive) {
                    this.monitorArchiveProcessingProgress(buildId);
                } else {
                    this.heliosAdminService.getBuildFiles(buildId).subscribe(response => this.files$.next(response.items));
                }
            });
    }

    private monitorArchiveProcessingProgress(buildId: number): void {
        if (this.timer instanceof Subscription) {
            this.timer.unsubscribe();
            this.timer = null;
        }

        this.timer = timer(0, 1000)
            .pipe(
                takeUntil(this.destroy$),
                switchMap(() => this.heliosAdminService.getBuild(buildId)),
                tap(build => this.build$.next(build)),
                takeWhile(build => build.isProcessing),
            )
            .subscribe();
    }

    private updateDraftBuild(buildId: number, rev: number) {
        const request = new WebProtocol.DraftBuildUpdateRequest();
        if (this.buildPaths.exePath.trim() !== this.pristineBuildPaths.exePath.trim()) request.exePath = this.buildPaths.exePath.trim();
        if (this.buildPaths.logPath.trim() !== this.pristineBuildPaths.logPath.trim()) request.logPath = this.buildPaths.logPath.trim();
        if (this.buildPaths.crashReportPath.trim() !== this.pristineBuildPaths.crashReportPath.trim()) request.crashReportPath = this.buildPaths.crashReportPath.trim();
        if (this.buildPaths.configPath.trim() !== this.pristineBuildPaths.configPath.trim()) request.configPath = this.buildPaths.configPath.trim();

        if (this.buildPaths.optionalFileMasks.trim() !== this.pristineBuildPaths.optionalFileMasks.trim()) {
            const masks = this.buildPaths.optionalFileMasks.split(/(\r?\n)/g).filter(s => s.trim().length > 0),
                maskSet = new Set<string>(masks);

            request.optionalFileMasks = Array.from(maskSet);
        }

        this.heliosAdminService
            .updateDraftBuild(request, buildId, rev)
            .pipe(takeUntil(this.destroy$))
            .subscribe(response => {
                this.notificationService.success(`Build settings updated`);

                if (response.result) {
                    this.updateBuildPaths(
                        response.build.exePath,
                        response.build.logPath,
                        response.build.crashReportPath,
                        response.build.configPath,
                        response.build.optionalFileMasks,
                    );
                }
            });
    }
}
