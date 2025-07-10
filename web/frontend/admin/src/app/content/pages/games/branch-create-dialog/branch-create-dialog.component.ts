import { Component, Inject, OnInit, OnDestroy } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material';
import { BehaviorSubject, Subject } from 'rxjs';
import { finalize, takeUntil } from 'rxjs/operators';
import { BranchCreateDialogData } from './branch-create-dialog.interface';
import { HeliosAdminService } from '../../../../protocol/web-admin-protocol.service';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

@Component({
    selector: 'm-branch-create-dialog',
    templateUrl: 'branch-create-dialog.component.html',
})
export class BranchCreateDialogComponent implements OnInit, OnDestroy {
    destroy$: Subject<any>;
    loading$: BehaviorSubject<boolean>;
    newBranch: WebProtocol.GameBranch;
    errorMessage?: string = null;
    gameEngines: Array<any> = [
        { value: WebProtocol.GameEngine.Generic, label: WebProtocol.GameEngine.getDescription(WebProtocol.GameEngine.Generic) },
        { value: WebProtocol.GameEngine.Ue4, label: WebProtocol.GameEngine.getDescription(WebProtocol.GameEngine.Ue4) },
        { value: WebProtocol.GameEngine.Unity, label: WebProtocol.GameEngine.getDescription(WebProtocol.GameEngine.Unity) },
        { value: WebProtocol.GameEngine.Hydra, label: WebProtocol.GameEngine.getDescription(WebProtocol.GameEngine.Hydra) },
        { value: WebProtocol.GameEngine.Cry, label: WebProtocol.GameEngine.getDescription(WebProtocol.GameEngine.Cry) },
    ];
    platforms: Array<any> = [
        { value: WebProtocol.Platform.Windows, label: WebProtocol.Platform.getDescription(WebProtocol.Platform.Windows) },
        { value: WebProtocol.Platform.Linux, label: WebProtocol.Platform.getDescription(WebProtocol.Platform.Linux) },
        { value: WebProtocol.Platform.Macos, label: WebProtocol.Platform.getDescription(WebProtocol.Platform.Macos) },
        { value: WebProtocol.Platform.Ios, label: WebProtocol.Platform.getDescription(WebProtocol.Platform.Ios) },
        { value: WebProtocol.Platform.Android, label: WebProtocol.Platform.getDescription(WebProtocol.Platform.Android) },
    ];

    constructor(
        public dialogRef: MatDialogRef<BranchCreateDialogComponent>,
        @Inject(MAT_DIALOG_DATA) public data: BranchCreateDialogData,
        private heliosAdminService: HeliosAdminService,
    ) {
        this.initializeGameBranch();
    }

    ngOnInit(): void {
        this.destroy$ = new Subject();
        this.loading$ = new BehaviorSubject<boolean>(false);
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
        this.loading$.complete();
    }

    private initializeGameBranch(): void {
        this.newBranch = new WebProtocol.GameBranch();
        this.newBranch.title = '';
        this.newBranch.description = '';
        this.newBranch.password = '';
        this.newBranch.gameEngine = WebProtocol.GameEngine.Generic;
        this.newBranch.platform = WebProtocol.Platform.Windows;
    }

    isGameBranchValid(): boolean {
        return this.newBranch.title.trim().length > 0;
    }

    createGameBranch(): void {
        const body = new WebProtocol.GameBranchCreateRequest();
        body.title = this.newBranch.title.trim();
        body.description = this.newBranch.description.trim();
        body.password = this.newBranch.password.trim();
        body.gameEngine = this.newBranch.gameEngine ? +this.newBranch.gameEngine : null;
        body.platform = +this.newBranch.platform;
        this.loading$.next(true);

        this.heliosAdminService
            .createGameBranch(body, this.data.gameId)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe((response: WebProtocol.GameBranchCreateResponse) => {
                if (response.result) {
                    this.dialogRef.close(response.branch);
                } else {
                    switch (response.error) {
                        case WebProtocol.GameBranchCreateError.Failure:
                            this.errorMessage = 'Server failed to process request';
                            break;
                        case WebProtocol.GameBranchCreateError.BranchTitleAlreadyExists:
                            this.errorMessage = 'Game branch title already exists';
                            break;
                        case WebProtocol.GameBranchCreateError.InvalidBranchTitle:
                            this.errorMessage = 'Invalid game branch title';
                            break;
                        case WebProtocol.GameBranchCreateError.InvalidGameId:
                            this.errorMessage = `Game identified by ${this.data.gameId} not exists`;
                            break;
                        case WebProtocol.GameBranchCreateError.InvalidGameEngine:
                            this.errorMessage = `Invalid game engine`;
                            break;
                        case WebProtocol.GameBranchCreateError.InvalidPlatform:
                            this.errorMessage = `Invalid platform`;
                            break;
                    }
                }
            });
    }
}
