import { Component, Inject, OnInit, OnDestroy } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material';
import { BehaviorSubject, Subject } from 'rxjs';
import { finalize, takeUntil } from 'rxjs/operators';
import { BranchEditDialogData } from './branch-edit-dialog.interface';
import { HeliosAdminService } from '../../../../protocol/web-admin-protocol.service';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

@Component({
    selector: 'm-branch-edit-dialog',
    templateUrl: 'branch-edit-dialog.component.html',
})
export class BranchEditDialogComponent implements OnInit, OnDestroy {
    destroy$: Subject<any>;
    loading$: BehaviorSubject<boolean>;
    branch: WebProtocol.GameBranch;
    pristineBranch: WebProtocol.GameBranch;
    errorMessage?: string = null;
    gameEngine = '';
    gameEngines: Array<any> = [
        { value: WebProtocol.GameEngine.Generic, label: WebProtocol.GameEngine.getDescription(WebProtocol.GameEngine.Generic) },
        { value: WebProtocol.GameEngine.Ue4, label: WebProtocol.GameEngine.getDescription(WebProtocol.GameEngine.Ue4) },
        { value: WebProtocol.GameEngine.Unity, label: WebProtocol.GameEngine.getDescription(WebProtocol.GameEngine.Unity) },
        { value: WebProtocol.GameEngine.Hydra, label: WebProtocol.GameEngine.getDescription(WebProtocol.GameEngine.Hydra) },
        { value: WebProtocol.GameEngine.Cry, label: WebProtocol.GameEngine.getDescription(WebProtocol.GameEngine.Cry) },
    ];

    constructor(
        public dialogRef: MatDialogRef<BranchEditDialogComponent>,
        @Inject(MAT_DIALOG_DATA) public data: BranchEditDialogData,
        private heliosAdminService: HeliosAdminService,
    ) {
        this.branch = WebProtocol.GameBranch.fromJson(data.branch.toJson());
        this.pristineBranch = WebProtocol.GameBranch.fromJson(data.branch.toJson());
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

    gameBranchInitialized(): boolean {
        return !!(this.branch && this.pristineBranch);
    }

    gameBranchValid(): boolean {
        return this.branch.title.trim().length > 0;
    }

    gameBranchChanged(): boolean {
        return this.branch.title.trim() !== this.pristineBranch.title.trim()
            || this.branch.description.trim() !== this.pristineBranch.description.trim()
            || this.branch.password !== this.pristineBranch.password
            || this.branch.gameEngine !== this.pristineBranch.gameEngine
            || this.branch.isReportable !== this.pristineBranch.isReportable
            || this.branch.isPublic !== this.pristineBranch.isPublic
            || this.branch.isDeleted !== this.pristineBranch.isDeleted;
    }

    onGameEngineChange(gameEngine: string): void {
        this.branch.gameEngine = +gameEngine;
    }

    updateGameBranch(): void {
        const body = new WebProtocol.GameBranchUpdateRequest();
        if (this.branch.title.trim() !== this.pristineBranch.title.trim()) body.title = this.branch.title.trim();
        if (this.branch.description.trim() !== this.pristineBranch.description.trim()) body.description = this.branch.description.trim();
        if (this.branch.password !== this.pristineBranch.password) body.password = this.branch.password;
        if (this.branch.gameEngine !== this.pristineBranch.gameEngine) body.gameEngine = this.branch.gameEngine;
        if (this.branch.isReportable !== this.pristineBranch.isReportable) body.isReportable = this.branch.isReportable;
        if (this.branch.isPublic !== this.pristineBranch.isPublic) body.isPublic = this.branch.isPublic;
        if (this.branch.isDeleted !== this.pristineBranch.isDeleted) body.isDeleted = this.branch.isDeleted;
        this.loading$.next(true);

        this.heliosAdminService
            .updateGameBranch(body, this.branch.id, this.branch.rev)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe((response: WebProtocol.GameBranchUpdateResponse) => {
                if (response.result) {
                    this.dialogRef.close(response.branch);
                } else {
                    switch (response.error) {
                        case WebProtocol.GameBranchUpdateError.Failure:
                            this.errorMessage = 'Server failed to process request';
                            break;
                        case WebProtocol.GameBranchUpdateError.RevMismatch:
                            this.errorMessage = 'Game branch updated remotely, try submit again';
                            this.branch = response.branch;
                            this.pristineBranch = WebProtocol.GameBranch.fromJson(response.branch.toJson());
                            Object.keys(body).filter(key => body[key] !== null).forEach(key => this.branch[key] = body[key]);
                            break;
                        case WebProtocol.GameBranchUpdateError.NothingToUpdate:
                            this.errorMessage = 'Nothing to update';
                            break;
                        case WebProtocol.GameBranchUpdateError.BranchTitleAlreadyExists:
                            this.errorMessage = 'Game branch title already exists';
                            break;
                        case WebProtocol.GameBranchUpdateError.InvalidBranchTitle:
                            this.errorMessage = 'Invalid game branch title';
                            break;
                    }
                }
            });
    }
}
