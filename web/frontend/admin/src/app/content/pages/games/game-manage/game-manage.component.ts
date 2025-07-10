import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnDestroy } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { MatDialog, Sort } from '@angular/material';
import { BehaviorSubject, combineLatest, Subject } from 'rxjs';
import { finalize, map, takeUntil } from 'rxjs/operators';
import { AlertMessage } from '../../../../shared/interfaces/alert-message';
import { Breadcrumb } from '../../../../shared/interfaces/breadcrumb';
import { compare } from '../../../../shared/functions/compare';
import { SubheaderService } from '../../../../core/services/metronic/layout/subheader.service';
import { AccountService } from '../../../../core/services/account.service';
import { ClipboardService } from '../../../../core/services/clipboard.service';
import { NotificationService } from '../../../../core/services/notification.service';
import { HeliosAdminService } from '../../../../protocol/web-admin-protocol.service';
import { BranchCreateDialogComponent } from '../branch-create-dialog/branch-create-dialog.component';
import { BranchCreateDialogData } from '../branch-create-dialog/branch-create-dialog.interface';
import { BranchEditDialogComponent } from '../branch-edit-dialog/branch-edit-dialog.component';
import { BranchEditDialogData } from '../branch-edit-dialog/branch-edit-dialog.interface';
import { BuildRedistDialogComponent } from '../build-redist-dialog/build-redist-dialog.component';
import { BuildRedistDialogData } from '../build-redist-dialog/build-redist-dialog.interface';
import { IniConfigurationDialogComponent } from '../ini-configuration-dialog/ini-configuration-dialog.component';
import { IniConfigurationDialogData } from '../ini-configuration-dialog/ini-configuration-dialog.interface';
import { RegistryConfigurationDialogComponent } from '../registry-configuration-dialog/registry-configuration-dialog.component';
import { RegistryConfigurationDialogData } from '../registry-configuration-dialog/registry-configuration-dialog.data';
import { DraftBuildCreateDialogComponent } from '../draft-build-create-dialog/draft-build-create-dialog.component';
import { DraftBuildCreateDialogData } from '../draft-build-create-dialog/draft-build-create-dialog.interface';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

interface GameBuildPatch {
    commentary?: string;
    changeList?: string;
    isPermanent?: boolean;
    configPath?: string;
    optionalFileMasks?: string[];
    preservedFileMasks?: string[];
    redistributables?: WebProtocol.RedistributableEntry[];
}

@Component({
    selector: 'm-game-manage',
    templateUrl: './game-manage.component.html',
    styleUrls: ['./game-manage.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class GameManageComponent implements OnDestroy {
    public config: any;
    destroy$ = new Subject();
    loading$ = new BehaviorSubject<boolean>(false);
    loadingBranches$ = new BehaviorSubject<boolean>(false);
    loadingBuilds$ = new BehaviorSubject<boolean>(false);
    role$ = new BehaviorSubject<WebProtocol.AccessRole | null>(null);
    favourite$ = new BehaviorSubject<boolean | null>(null);
    accessRole = WebProtocol.AccessRole;
    alerts: Array<AlertMessage> = [];
    gameId?: string;
    game: WebProtocol.Game;
    pristineGame: WebProtocol.Game;
    categories: WebProtocol.GameCategory[];
    categoryAssignment: Map<number, boolean>;
    builds: WebProtocol.Build[];
    buildsSliceLength = 100;
    activeBuild?: WebProtocol.Build = null;
    branches: WebProtocol.GameBranch[];
    menuBranches: WebProtocol.GameBranch[];
    displayedBranchColumns: string[] = [
        'id',
        'title',
        'status',
        'available',
        'gameEngine',
        'platform',
        'password',
        'buildRev',
        'buildCreatedAt',
        'buildCompressedSize',
        'buildTotalSize',
        'iniConfig',
        'registryConfig',
        'isReportable',
        'isDefault',
        'isPublic',
        'isDeleted',
        'updatedAt'
    ];

    constructor(
        private activatedRoute: ActivatedRoute,
        private router: Router,
        private cdr: ChangeDetectorRef,
        private dialog: MatDialog,
        public accountService: AccountService,
        private clipboardService: ClipboardService,
        private subheaderService: SubheaderService,
        private heliosAdminService: HeliosAdminService,
        private notificationService: NotificationService,
    ) {
        combineLatest(
            this.activatedRoute.data.pipe(map(data => data.role)),
            this.activatedRoute.paramMap.pipe(map(params => params.get('id'))),
        ).pipe(
            takeUntil(this.destroy$),
        ).subscribe(([role, gameId]) => {
            this.role$.next(role);
            this.alerts = [];
            this.getGame(gameId);

            if (role < WebProtocol.AccessRole.Uploader) {
                this.addAlert(<AlertMessage>{
                    message: 'You are not allowed to manage this game',
                    kind: 'danger',
                    dismissable: false,
                });
            } else {
                this.getAllGameBranches(gameId);
                this.getGameBuilds(gameId, 0, 100);
                this.isFavouriteGame(gameId);
                this.getGameCategories();
            }
        });
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
        this.loading$.complete();
        this.loadingBranches$.complete();
        this.loadingBuilds$.complete();
    }

    // Common
    updatePageStatus(game: WebProtocol.Game): void {
        const status = this.gameStatus(game),
            badgeClass = this.gameBadgeClass(game);

        this.subheaderService.setStatus(status, badgeClass);
    }

    addAlert(alert: AlertMessage, stack = true): void {
        if (stack) {
            const index = this.alerts.findIndex(a => a.message === alert.message);

            if (index >= 0) {
                this.alerts.splice(index, 1);
            }
        }

        this.alerts.unshift(alert);
    }

    // Role-specific
    isAllowed(minRole: WebProtocol.AccessRole): boolean {
        return this.role$.getValue() >= minRole;
    }

    // Game-specific
    isGameInitialized(): boolean {
        return !!(this.game && this.pristineGame);
    }

    gameChanged(): boolean {
        if (!this.isGameInitialized()) {
            return false;
        }

        return this.game.title !== this.pristineGame.title
            || this.game.description !== this.pristineGame.description
            || this.game.jiraKey !== this.pristineGame.jiraKey
            || this.game.seleneKey !== this.pristineGame.seleneKey
            || this.game.ciUrl !== this.pristineGame.ciUrl
            || this.game.discordUrl !== this.pristineGame.discordUrl
            || this.game.price !== this.pristineGame.price
            || this.game.currency !== this.pristineGame.currency
            || this.game.buildLifetime !== this.pristineGame.buildLifetime
            || this.game.isPublished !== this.pristineGame.isPublished
            || this.game.isDisabled !== this.pristineGame.isDisabled
            || this.game.isDeleted !== this.pristineGame.isDeleted;
    }

    gameValid(): boolean {
        if (!this.isGameInitialized()) {
            return false;
        }

        return this.game.title.trim().length > 0
            && (!this.game.jiraKey || /^[A-Z][A-Z0-9]+$/.test(this.game.jiraKey))
            && (!this.game.seleneKey || /^{[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}}$/.test(this.game.seleneKey))
            && +this.game.price >= 0
            && +this.game.buildLifetime >= 0
            && ['EUR', 'USD', 'RUB'].indexOf(this.game.currency) >= 0;
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

    gameEngine(gameEngine: WebProtocol.GameEngine): string {
        return WebProtocol.GameEngine.getDescription(gameEngine);
    }

    platform(platform: WebProtocol.Platform): string {
        return WebProtocol.Platform.getDescription(platform);
    }

    canShowSeleneIcon(game: WebProtocol.Game): boolean {
        const gameStatus = this.gameStatus(game);
        return gameStatus === 'internal' || gameStatus === 'published';
    }

    canShowToadmanLauncherIcon(game: WebProtocol.Game): boolean {
        return this.gameStatus(game) === 'published';
    }

    suitableBranches(branches: WebProtocol.GameBranch[], platform: WebProtocol.Platform): WebProtocol.GameBranch[] {
        return branches instanceof Array ? branches.filter(b => b.platform === platform) : null;
    }

    applyGame(game: WebProtocol.Game): void {
        game.jiraKey = game.jiraKey || '';
        game.seleneKey = game.seleneKey || '';
        game.ciUrl = game.ciUrl || '';
        game.discordUrl = game.discordUrl || '';
        this.game = game;
        this.pristineGame = WebProtocol.Game.fromJson(game.toJson());
        this.subheaderService.setTitle(this.game.title);
        this.subheaderService.setBreadcrumbs([
            <Breadcrumb>{title: 'Games', page: '/games'},
            <Breadcrumb>{title: this.game.title, page: `/games/${this.game.id}/manage`},
        ]);
        this.updatePageStatus(this.game);

        this.categoryAssignment = new Map<number, boolean>();
        game.categories.forEach(categoryId => this.categoryAssignment.set(categoryId, true));

        this.cdr.detectChanges();
    }

    mergeGame(game: WebProtocol.Game, body: WebProtocol.GameUpdateRequest): void {
        // TODO: implement conflict resolution
        // const oldGame = this.game;
        this.applyGame(game);

        this.addAlert(<AlertMessage>{
            message: 'Game details were changed after you have opened this page. All remote changes are merged now. Try submit again',
            kind: 'warning',
            dismissable: true
        });

        Object.keys(body)
            .filter((key: string) => body[key] !== null)
            .forEach((key: string) => this.game[key] = body[key]);
    }

    getGame(guid: string): void {
        this.gameId = guid;
        this.loading$.next(true);

        this.heliosAdminService.getGame(guid)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe((response: WebProtocol.Game) => this.applyGame(response));
    }

    updateGame(): void {
        if (!this.isGameInitialized()) {
            return;
        }

        const body = new WebProtocol.GameUpdateRequest();
        if (this.game.title.trim() !== this.pristineGame.title.trim()) body.title = this.game.title.trim();
        if (this.game.description.trim() !== this.pristineGame.description.trim()) body.description = this.game.description.trim();
        if (this.game.jiraKey.trim() !== this.pristineGame.jiraKey.trim()) body.jiraKey = this.game.jiraKey.trim();
        if (this.game.seleneKey.trim() !== this.pristineGame.seleneKey.trim()) body.seleneKey = this.game.seleneKey.trim();
        if (this.game.ciUrl.trim() !== this.pristineGame.ciUrl.trim()) body.ciUrl = this.game.ciUrl.trim();
        if (this.game.discordUrl.trim() !== this.pristineGame.discordUrl.trim()) body.discordUrl = this.game.discordUrl.trim();
        if (+this.game.price !== +this.pristineGame.price) body.price = +this.game.price;
        if (this.game.currency.trim() !== this.pristineGame.currency.trim()) body.currency = this.game.currency.trim();
        if (+this.game.buildLifetime !== this.pristineGame.buildLifetime) body.buildLifetime = +this.game.buildLifetime;
        if (this.game.isPublished !== this.pristineGame.isPublished) body.isPublished = this.game.isPublished;
        if (this.game.isDisabled !== this.pristineGame.isDisabled) body.isDisabled = this.game.isDisabled;
        if (this.game.isDeleted !== this.pristineGame.isDeleted) body.isDeleted = this.game.isDeleted;
        this.loading$.next(true);

        this.heliosAdminService
            .updateGame(body, this.game.id, this.game.rev)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe((response: WebProtocol.GameUpdateResponse) => {
                if (response.result) {
                    this.notificationService.success('Game updated');
                    this.applyGame(response.game);
                } else {
                    let message = '',
                        isError = true;

                    switch (response.error) {
                        case WebProtocol.GameUpdateError.Failure:
                            message = 'Server failed to process request';
                            break;
                        case WebProtocol.GameUpdateError.RevMismatch:
                            this.mergeGame(response.game, body);
                            isError = false;
                            break;
                        case WebProtocol.GameUpdateError.NothingToUpdate:
                            message = 'Nothing to update';
                            isError = false;
                            break;
                        case WebProtocol.GameUpdateError.GameTitleAlreadyExists:
                            message = 'Game title already exists';
                            break;
                        case WebProtocol.GameUpdateError.InvalidGameTitle:
                            message = 'Invalid game title';
                            break;
                        case WebProtocol.GameUpdateError.InvalidJiraKey:
                            message = 'Invalid JIRA project key';
                            break;
                        case WebProtocol.GameUpdateError.InvalidSeleneKey:
                            message = 'Invalid Selene migration key';
                            break;
                        case WebProtocol.GameUpdateError.InvalidCiUrl:
                            message = 'Invalid CI server URL';
                            break;
                        case WebProtocol.GameUpdateError.InvalidDiscordUrl:
                            message = 'Invalid Discord URL';
                            break;
                        case WebProtocol.GameUpdateError.InvalidPrice:
                            message = 'Invalid price';
                            break;
                        case WebProtocol.GameUpdateError.InvalidCurrency:
                            message = 'Invalid currency';
                            break;
                        case WebProtocol.GameUpdateError.InvalidBuildLifetime:
                            message = 'Invalid build lifetime';
                            break;
                    }

                    if (message) {
                        isError ? this.notificationService.error(message) : this.notificationService.info(message);
                    }
                }
            });
    }

    // Game branch specific
    branchStatus(branch: WebProtocol.GameBranch): string {
        if (branch.isDeleted) {
            return 'deleted';
        } else if (!branch.buildId) {
            return 'detached';
        } else if (branch.isDefault) {
            return 'default';
        } else if (branch.isPublic) {
            return 'public';
        } else {
            return 'active';
        }
    }

    branchBadgeClass(branch: WebProtocol.GameBranch): string {
        switch (this.branchStatus(branch)) {
            case 'deleted':
                return 'm-timeline-3__item--danger';
            case 'detached':
                return 'm-timeline-3__item--warning';
            case 'default':
                return 'm-timeline-3__item--success';
            case 'public':
                return 'm-timeline-3__item--brand';
            case 'active':
                return 'm-timeline-3__item--metal';
            default:
                return 'm-timeline-3__item--metal';
        }
    }

    canShowSeleneIconForBranch(branch: WebProtocol.GameBranch): boolean {
        if (!branch || !this.pristineGame) {
            return false;
        }

        return this.canShowSeleneIcon(this.pristineGame) && !branch.isDeleted && !!branch.buildId;
    }

    canShowToadmanLauncherIconForBranch(branch: WebProtocol.GameBranch): boolean {
        if (!branch || !this.pristineGame) {
            return false;
        }

        return this.canShowToadmanLauncherIcon(this.pristineGame) && !branch.isDeleted && !!branch.buildId && (branch.isDefault || branch.isPublic);
    }

    onBranchPasswordCopy(password: string): void {
        this.clipboardService.copy(password);
        this.notificationService.info('Branch password copied to clipboard');
    }

    sortBranches(sort: Sort) {
        const data = this.branches.slice();

        if (!sort.active || sort.direction === '') {
            this.branches = data;
            return;
        }

        this.branches = data.sort((a: WebProtocol.GameBranch, b: WebProtocol.GameBranch) => {
            const isAsc = sort.direction === 'asc';

            switch (sort.active) {
                case 'id':
                    return compare(a.id, b.id, isAsc);
                case 'title':
                    return compare(a.title, b.title, isAsc);
                case 'password':
                    return compare(a.password, b.password, isAsc);
                case 'status':
                    return compare(this.branchStatus(a), this.branchStatus(b), isAsc);
                case 'buildRev':
                    return compare(parseInt(a.buildRev, 10), parseInt(b.buildRev, 10), isAsc);
                case 'buildCompressedSize':
                    return compare(a.buildCompressedSize, b.buildCompressedSize, isAsc);
                case 'buildTotalSize':
                    return compare(a.buildTotalSize, b.buildTotalSize, isAsc);
                case 'buildCreatedAt':
                    return compare(a && a.buildCreatedAt.getTime() || 0, b && b.buildCreatedAt.getTime() || 0, isAsc);
                case 'gameEngine':
                    return compare(WebProtocol.GameEngine.getDescription(a.gameEngine).toLowerCase(), WebProtocol.GameEngine.getDescription(b.gameEngine).toLowerCase(), isAsc);
                case 'platform':
                    return compare(WebProtocol.Platform.getDescription(a.platform).toLowerCase(), WebProtocol.Platform.getDescription(b.platform).toLowerCase(), isAsc);
                case 'isDefault':
                    return compare(a.isDefault ? 1 : 0, b.isDefault ? 1 : 0, isAsc);
                case 'isPublic':
                    return compare(a.isPublic ? 1 : 0, b.isPublic ? 1 : 0, isAsc);
                case 'isDeleted':
                    return compare(a.isDeleted ? 1 : 0, b.isDeleted ? 1 : 0, isAsc);
                case 'createdAt':
                    return compare(a.createdAt.getTime(), b.createdAt.getTime(), isAsc);
                case 'updatedAt':
                    return compare(a.updatedAt.getTime(), b.updatedAt.getTime(), isAsc);
                default:
                    return 0;
            }
        });

        this.cdr.detectChanges();
    }

    openBuildRedistDialog(build: WebProtocol.Build): void {
        const
            data = <BuildRedistDialogData>{build: build},
            dialogRef = this.dialog.open(BuildRedistDialogComponent, {width: '800px', data: data});

        dialogRef.afterClosed()
            .pipe(takeUntil(this.destroy$))
            .subscribe(result => {
                if (result) {
                    this.notificationService.success('Game build redistributables updated');
                    this.getAllGameBranches(this.gameId);
                    this.getGameBuilds(this.gameId, 0, 1000);
                    this.cdr.detectChanges();
                }
            });
    }

    openGameBranchCreateDialog(): void {
        const
            data = <BranchCreateDialogData>{gameId: this.gameId, gameTitle: this.game.title},
            dialogRef = this.dialog.open(BranchCreateDialogComponent, {width: '400px', data: data});

        dialogRef.afterClosed()
            .pipe(takeUntil(this.destroy$))
            .subscribe(result => {
                if (result) {
                    this.notificationService.success('New game branch added');
                    this.getAllGameBranches(this.gameId);
                    this.cdr.detectChanges();
                }
            });
    }

    openGameBranchEditCreateDialog(branch: WebProtocol.GameBranch): void {
        const data = <BranchEditDialogData>{branch: branch},
            dialogRef = this.dialog.open(BranchEditDialogComponent, {width: '400px', data: data});

        dialogRef.afterClosed()
            .pipe(takeUntil(this.destroy$))
            .subscribe(result => {
                if (result) {
                    this.notificationService.success(`Game branch updated`);
                    this.getAllGameBranches(this.gameId);
                    this.cdr.detectChanges();
                }
            });
    }

    openIniConfigurationDialog(branch: WebProtocol.GameBranch): void {
        const data = <IniConfigurationDialogData>{branch: branch},
            dialogRef = this.dialog.open(IniConfigurationDialogComponent, {width: '850px', data: data, disableClose: true});

        dialogRef.afterClosed()
            .pipe(takeUntil(this.destroy$))
            .subscribe(result => {
                if (result) {
                    this.notificationService.success(`INI configuration updated`);
                    this.getAllGameBranches(this.gameId);
                    this.cdr.detectChanges();
                }
            });
    }

    openRegistryConfigurationDialog(branch: WebProtocol.GameBranch): void {
        const data = <RegistryConfigurationDialogData>{branch: branch},
            dialogRef = this.dialog.open(RegistryConfigurationDialogComponent, {width: '1024px', data: data, disableClose: true});

        dialogRef.afterClosed()
            .pipe(takeUntil(this.destroy$))
            .subscribe(result => {
                if (result) {
                    this.notificationService.success(`Registry configuration updated`);
                    this.getAllGameBranches(this.gameId);
                    this.cdr.detectChanges();
                }
            });
    }

    openDraftBuildCreateDialog(): void {
        const data = <DraftBuildCreateDialogData>{gameId: this.gameId, gameTitle: this.game.title},
            dialogRef = this.dialog.open(DraftBuildCreateDialogComponent, {width: '500px', data: data});

        dialogRef.afterClosed()
            .pipe(takeUntil(this.destroy$))
            .subscribe((result: WebProtocol.Build | null) => {
                if (result instanceof WebProtocol.Build) {
                    this.notificationService.success(`Draft game build created`);
                    this.router.navigate([`/games/${this.gameId}/builds/${result.buildRev}/draft`]);
                }
            });
    }

    getAllGameBranches(guid: string): void {
        this.loadingBranches$.next(true);

        this.heliosAdminService.getAllGameBranches(guid)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loadingBranches$.next(false)),
            )
            .subscribe((response: WebProtocol.GameBranch[]) => {
                this.branches = response;
                this.menuBranches = response.map((branch: WebProtocol.GameBranch) => WebProtocol.GameBranch.fromJson(branch.toJson()));
                this.cdr.detectChanges();
            });
    }

    setGameBranchAsDefault(branch: WebProtocol.GameBranch): void {
        const body = new WebProtocol.Empty();
        this.loadingBranches$.next(true);

        this.heliosAdminService
            .setGameBranchAsDefault(body, branch.id)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loadingBranches$.next(false)),
            )
            .subscribe((response: WebProtocol.GameBranchBuildResponse) => {
                if (response.result) {
                    this.notificationService.success(`Branch ${branch.title} set as default`);
                    this.getAllGameBranches(this.gameId);
                    this.getGameBuilds(this.gameId, 0, 100);
                } else {
                    this.notificationService.error('Default branch is not changed');
                }
            });
    }

    setGameBranchBuild(branchId: number, buildId: number): void {
        const body = new WebProtocol.GameBranchBuildRequest();
        body.buildId = buildId;
        this.loadingBranches$.next(true);

        this.heliosAdminService
            .setGameBranchBuild(body, branchId)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loadingBranches$.next(false)),
            )
            .subscribe((response: WebProtocol.GameBranchBuildResponse) => {
                if (response.result) {
                    this.notificationService.success(`Build #${response.branch.buildRev} assigned to branch ${response.branch.title}`);
                    this.getAllGameBranches(this.gameId);
                    this.getGameBuilds(this.gameId, 0, 100);
                } else {
                    let message = '';

                    switch (response.error) {
                        case WebProtocol.GameBranchBuildError.Failure:
                            message = 'Server failed to process request';
                            break;
                        case WebProtocol.GameBranchBuildError.BranchNotExists:
                            message = 'Game branch not exists or deleted';
                            break;
                        case WebProtocol.GameBranchBuildError.BuildNotExists:
                            message = 'Game build not exists or deleted';
                            break;
                        case WebProtocol.GameBranchBuildError.GameMismatch:
                            message = `Branch and build game doesn't match`;
                            break;
                        case WebProtocol.GameBranchBuildError.PlatformMismatch:
                            message = `Branch and build platform doesn't match`;
                            break;
                    }

                    this.notificationService.error(message);
                }
            });
    }

    toggleGameBranchAsReportable(branchId: number, branchRev: number, isReportable: boolean): void {
        const body = new WebProtocol.GameBranchUpdateRequest();
        body.isReportable = isReportable;
        this.loadingBranches$.next(true);

        this.heliosAdminService
            .updateGameBranch(body, branchId, branchRev)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loadingBranches$.next(false)),
            )
            .subscribe((response: WebProtocol.GameBranchUpdateResponse) => {
                if (response.result) {
                    this.notificationService.success(`Branch #${response.branch.title} ${isReportable ? '' : 'not'} accepts log reports`);
                    this.getAllGameBranches(this.gameId);
                } else {
                    let message = '';

                    switch (response.error) {
                        case WebProtocol.GameBranchUpdateError.Failure:
                            message = 'Server failed to process request';
                            break;
                        case WebProtocol.GameBranchUpdateError.RevMismatch:
                            message = 'Game branch updated remotely, try submit again';
                            break;
                        case WebProtocol.GameBranchUpdateError.NothingToUpdate:
                            message = 'Nothing to update';
                            break;
                    }

                    this.notificationService.error(message);
                }
            });
    }

    // Game build specific
    buildTagBadgeClass(index: number): string {
        switch (index) {
            case 0:
                return 'm-badge--info';
            case 1:
                return 'm-badge--success';
            case 2:
                return 'm-badge--warning';
            case 3:
                return 'm-badge--danger';
            default:
                return 'm-badge--disabled';
        }
    }

    toggleBuildIndicator(indicator: Element, isActive: boolean): void {
        indicator.classList.remove(isActive ? 'la-dot-circle-o' : 'la-plus-circle', isActive ? 'm--font-brand' : 'm--font-success');
        indicator.classList.add(isActive ? 'la-plus-circle' : 'la-dot-circle-o', isActive ? 'm--font-success' : 'm--font-brand');
    }

    fixBuildRev(rev: string): number {
        return parseInt(rev, 10);
    }

    onBuildAssign(branch: WebProtocol.GameBranch, build: WebProtocol.Build): void {
        if (branch.buildId === build.id) {
            return;
        }

        this.setGameBranchBuild(branch.id, build.id);
    }

    onBuildCommentaryUpdate($event: string): void {
        if (this.activeBuild && this.activeBuild.commentary.trim() !== $event.trim()) {
            this.updateBuild(this.activeBuild.id, this.activeBuild.rev, {commentary: $event} as GameBuildPatch);
        }
    }

    onBuildChangeListUpdate($event: string): void {
        if (this.activeBuild && this.activeBuild.changeList.trim() !== $event.trim()) {
            this.updateBuild(this.activeBuild.id, this.activeBuild.rev, {changeList: $event} as GameBuildPatch);
        }
    }

    onBuildConfigPathUpdate($event: string): void {
        if (this.activeBuild && this.activeBuild.configPath.trim() !== $event.trim()) {
            this.updateBuild(this.activeBuild.id, this.activeBuild.rev, {configPath: $event.trim()} as GameBuildPatch);
        }
    }

    onBuildOptionalFileMasksUpdate($event: string): void {
        if (this.activeBuild) {
            const masks = $event.split(/(\r?\n)/g).filter(s => s.trim().length > 0),
                maskSet = new Set<string>(masks),
                optionalFileMasks = Array.from(maskSet);

            this.updateBuild(this.activeBuild.id, this.activeBuild.rev, {optionalFileMasks: optionalFileMasks} as GameBuildPatch);
        }
    }

    onBuildPreservedFileMasksUpdate($event: string): void {
        if (this.activeBuild) {
            const masks = $event.split(/(\r?\n)/g).filter(s => s.trim().length > 0),
                maskSet = new Set<string>(masks),
                preservedFileMasks = Array.from(maskSet);

            this.updateBuild(this.activeBuild.id, this.activeBuild.rev, {preservedFileMasks: preservedFileMasks} as GameBuildPatch);
        }
    }

    onBuildTogglePermanent(): void {
        if (this.activeBuild && !this.activeBuild.isDeleted) {
            this.updateBuild(this.activeBuild.id, this.activeBuild.rev, {isPermanent: !this.activeBuild.isPermanent} as GameBuildPatch);
        }
    }

    onBuildDelete(): void {
        if (this.activeBuild && !this.activeBuild.isDeleted) {
            this.deleteBuild(this.activeBuild.id);
        }
    }

    onBuildPublish(build: WebProtocol.Build): void {
        if (build.isDraft) {
            this.publishDraftBuild(build.id);
        }
    }

    onCategoryToggle(guid: string, categoryId: number, $event: any): void {
        // Keep menu open
        $event.stopPropagation();

        const isAssigned = this.categoryAssignment.has(categoryId)
            ? this.categoryAssignment.get(categoryId)
            : false;

        isAssigned
            ? this.unassignGameCategory(guid, categoryId)
            : this.assignGameCategory(guid, categoryId);
    }

    getGameBuilds(guid: string, offset: number, limit: number): void {
        this.loadingBuilds$.next(true);

        this.heliosAdminService
            .getBuildsForGame(guid, WebProtocol.BuildOrderBy.CreatedAt, WebProtocol.OrderDirection.Desc, offset, limit)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loadingBuilds$.next(false)),
            )
            .subscribe((response: WebProtocol.CollectionSlice<WebProtocol.Build>) => {
                // TODO: store total for pagination
                this.builds = response.items;
                this.cdr.detectChanges();
            });
    }

    updateBuild(buildId: number, rev: number, patch: GameBuildPatch): void {
        const body = new WebProtocol.BuildUpdateRequest();
        if (typeof patch.commentary === 'string') body.commentary = patch.commentary;
        if (typeof patch.changeList === 'string') body.changeList = patch.changeList;
        if (typeof patch.configPath === 'string') body.configPath = patch.configPath;
        if (typeof patch.isPermanent === 'boolean') body.isPermanent = patch.isPermanent;
        if (patch.optionalFileMasks instanceof Array) body.optionalFileMasks = patch.optionalFileMasks;
        if (patch.preservedFileMasks instanceof Array) body.preservedFileMasks = patch.preservedFileMasks;
        if (patch.redistributables instanceof Array) body.redistributables = patch.redistributables;
        this.loadingBuilds$.next(true);

        this.heliosAdminService
            .updateBuild(body, buildId, rev)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loadingBuilds$.next(false)),
            )
            .subscribe((response: WebProtocol.BuildUpdateResponse) => {
                if (response.result) {
                    this.notificationService.success(`Build #${response.build.buildRev} updated`);
                    this.getAllGameBranches(this.gameId);
                    this.getGameBuilds(this.gameId, 0, 100);
                } else {
                    let message = '',
                        isError = true;

                    switch (response.error) {
                        case WebProtocol.BuildUpdateError.Failure:
                            message = 'Server failed to process request';
                            break;
                        case WebProtocol.BuildUpdateError.NothingToUpdate:
                            message = 'Nothing to update';
                            isError = false;
                            break;
                        case WebProtocol.BuildUpdateError.RevMismatch:
                            message = 'Build was updated remotely, try update again';
                            isError = false;
                            this.getAllGameBranches(this.gameId);
                            this.getGameBuilds(this.gameId, 0, 100);
                            break;
                    }

                    isError
                        ? this.notificationService.error(message)
                        : this.notificationService.warning(message);
                }
            });
    }

    publishDraftBuild(buildId: number): void {
        this.loadingBuilds$.next(true);

        this.heliosAdminService
            .publishDraftBuild(new WebProtocol.Empty(), buildId)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loadingBuilds$.next(false)),
            )
            .subscribe((response: WebProtocol.BuildPublishResponse) => {
                if (response.result) {
                    this.notificationService.success(`Build published successfully`);
                    this.getGameBuilds(this.gameId, 0, 100);
                } else {
                    let message = '';

                    switch (response.error) {
                        case WebProtocol.BuildPublishError.Failure:
                            message = 'Server failed to process request';
                            break;
                        case WebProtocol.BuildPublishError.AlreadyPublished:
                            message = 'Build is already published';
                            break;
                        case WebProtocol.BuildPublishError.NoFiles:
                            message = 'Build contains no files';
                            break;
                        case WebProtocol.BuildPublishError.MissingFiles:
                            message = 'Some build files are missing';
                            break;
                        case WebProtocol.BuildPublishError.InvalidExePath:
                            message = 'Path to executable is invalid';
                            break;
                    }

                    this.notificationService.error(message);
                }
            });
    }

    deleteBuild(buildId: number): void {
        this.loadingBuilds$.next(true);

        this.heliosAdminService
            .deleteBuild(buildId)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loadingBuilds$.next(false)),
            )
            .subscribe((response: WebProtocol.BuildDeleteResponse) => {
                if (response.result) {
                    this.notificationService.success(`Build deleted permanently`);
                    this.getAllGameBranches(this.gameId);
                    this.getGameBuilds(this.gameId, 0, 100);
                } else {
                    let message = '';

                    switch (response.error) {
                        case WebProtocol.BuildDeleteError.Failure:
                            message = 'Server failed to process request';
                            break;
                        case WebProtocol.BuildDeleteError.AlreadyDeleted:
                            message = 'Build is already deleted';
                            break;
                        case WebProtocol.BuildDeleteError.HasAssignedBranches:
                            message = 'Re-assign branches to other builds and try again';
                            break;
                    }

                    this.notificationService.error(message);
                }
            });
    }

    favourGame(guid: string): void {
        this.heliosAdminService
            .favourGame(new WebProtocol.Empty(), guid)
            .pipe(
                takeUntil(this.destroy$),
                map(response => response.result),
            )
            .subscribe(
                result => {
                    this.favourite$.next(true);
                    this.notificationService.success(`Game added to favourites`);
                },
                error => this.notificationService.error(`Unable to add game to favourites`),
            );
    }

    unfavourGame(guid: string): void {
        this.heliosAdminService
            .unfavourGame(guid)
            .pipe(
                takeUntil(this.destroy$),
                map(response => response.result),
            )
            .subscribe(
                result => {
                    this.favourite$.next(false);
                    this.notificationService.info(`Game removed from favourites`);
                },
                error => this.notificationService.error(`Unable to remove game from favourites`),
            );
    }

    private isFavouriteGame(guid: string): void {
        this.favourite$.next(null);

        this.heliosAdminService
            .isFavouriteGame(guid)
            .pipe(
                takeUntil(this.destroy$),
                map(response => response.result),
            )
            .subscribe(result => this.favourite$.next(result));
    }

    private getGameCategories(): void {
        this.heliosAdminService
            .getGameCategories()
            .pipe(
                takeUntil(this.destroy$),
                map(response => response.items),
            )
            .subscribe(categories => {
                this.categories = categories;
                this.cdr.detectChanges();
            });
    }

    private assignGameCategory(guid: string, categoryId: number): void {
        this.heliosAdminService
            .assignGameCategory(new WebProtocol.Empty(), guid, categoryId)
            .pipe(takeUntil(this.destroy$))
            .subscribe(response => {
                if (response.result) {
                    this.categoryAssignment.set(categoryId, true);
                    this.cdr.detectChanges();
                }
            });
    }

    private unassignGameCategory(guid: string, categoryId: number): void {
        this.heliosAdminService
            .unassignGameCategory(guid, categoryId)
            .pipe(takeUntil(this.destroy$))
            .subscribe(response => {
                if (response.result) {
                    this.categoryAssignment.set(categoryId, false);
                    this.cdr.detectChanges();
                }
            });
    }
}
