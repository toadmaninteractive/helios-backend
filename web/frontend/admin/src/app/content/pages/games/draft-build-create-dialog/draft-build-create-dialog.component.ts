import { Component, Inject, OnInit, OnDestroy } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material';
import { BehaviorSubject, Subject } from 'rxjs';
import { finalize, takeUntil } from 'rxjs/operators';
import { DraftBuildCreateDialogData } from './draft-build-create-dialog.interface';
import { HeliosAdminService } from '../../../../protocol/web-admin-protocol.service';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

@Component({
    selector: 'm-draft-build-create-dialog',
    templateUrl: 'draft-build-create-dialog.component.html',
})
export class DraftBuildCreateDialogComponent implements OnInit, OnDestroy {
    destroy$: Subject<any>;
    loading$: BehaviorSubject<boolean>;
    gameId: string;
    gameTitle: string;
    errorMessage?: string = null;
    buildRev = '';
    platform = WebProtocol.Platform.Windows;
    platforms: Array<any> = [
        { value: WebProtocol.Platform.Windows, label: WebProtocol.Platform.getDescription(WebProtocol.Platform.Windows) },
        { value: WebProtocol.Platform.Linux, label: WebProtocol.Platform.getDescription(WebProtocol.Platform.Linux) },
        { value: WebProtocol.Platform.Macos, label: WebProtocol.Platform.getDescription(WebProtocol.Platform.Macos) },
        { value: WebProtocol.Platform.Ios, label: WebProtocol.Platform.getDescription(WebProtocol.Platform.Ios) },
        { value: WebProtocol.Platform.Android, label: WebProtocol.Platform.getDescription(WebProtocol.Platform.Android) },
    ];

    constructor(
        public dialogRef: MatDialogRef<DraftBuildCreateDialogComponent>,
        @Inject(MAT_DIALOG_DATA) public data: DraftBuildCreateDialogData,
        private heliosAdminService: HeliosAdminService,
    ) {
        this.gameId = data.gameId;
        this.gameTitle = data.gameTitle;
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

    buildValid(): boolean {
        return /^[A-Za-z0-9]{1}[A-Za-z0-9-_]*$/.test(this.buildRev.trim());
    }

    onDraftBuildCreate(): void {
        if (!this.buildValid())
            return;

        this.createDraftBuild(this.gameId, this.buildRev, +this.platform);
    }

    private createDraftBuild(gameId: string, buildRev: string, platform: WebProtocol.Platform): void {
        const body = new WebProtocol.DraftBuildCreateRequest();
        body.buildRev = buildRev.trim();
        body.platform = platform;
        this.loading$.next(true);

        this.heliosAdminService
            .createDraftBuild(body, gameId)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe((response: WebProtocol.DraftBuildCreateResponse) => {
                if (response.result) {
                    this.dialogRef.close(response.build);
                } else {
                    switch (response.error) {
                        case WebProtocol.DraftBuildCreateError.Failure:
                            this.errorMessage = 'Server failed to process request';
                            break;
                        case WebProtocol.DraftBuildCreateError.InvalidGameId:
                            this.errorMessage = 'Specified game does not exist';
                            break;
                        case WebProtocol.DraftBuildCreateError.InvalidPlatform:
                            this.errorMessage = 'Invalid platform';
                            break;
                        case WebProtocol.DraftBuildCreateError.InvalidBuildRev:
                            this.errorMessage = 'Invalid build revision';
                            break;
                        case WebProtocol.DraftBuildCreateError.BuildRevAlreadyExists:
                            this.errorMessage = 'Build revision already exists';
                            break;
                    }
                }
            });
    }
}
