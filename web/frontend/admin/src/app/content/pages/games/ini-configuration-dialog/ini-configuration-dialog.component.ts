import { Component, Inject, OnInit, OnDestroy } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material';
import { BehaviorSubject, Subject } from 'rxjs';
import { finalize, takeUntil } from 'rxjs/operators';
import { IniConfigurationDialogData } from './ini-configuration-dialog.interface';
import { HeliosAdminService } from '../../../../protocol/web-admin-protocol.service';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

@Component({
    selector: 'm-ini-configuration-dialog',
    templateUrl: 'ini-configuration-dialog.component.html',
})
export class IniConfigurationDialogComponent implements OnInit, OnDestroy {
    destroy$: Subject<any>;
    loading$: BehaviorSubject<boolean>;
    entries: WebProtocol.IniFileEntry[];
    pristineEntries: WebProtocol.IniFileEntry[];
    errorMessage?: string = null;

    constructor (
        public dialogRef: MatDialogRef<IniConfigurationDialogComponent>,
        @Inject(MAT_DIALOG_DATA) public data: IniConfigurationDialogData,
        private heliosAdminService: HeliosAdminService,
    ) {
        this.entries = data.branch.iniConfig.map(e => WebProtocol.IniFileEntry.fromJson(e.toJson()));
        this.pristineEntries = data.branch.iniConfig.map(e => WebProtocol.IniFileEntry.fromJson(e.toJson()));
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

    entriesInitialized(): boolean {
        return !!(this.entries && this.pristineEntries);
    }

    entryValid(entry: WebProtocol.IniFileEntry): boolean {
        return entry.section.trim().length > 0
            && entry.param.trim().length > 0;
    }

    entriesValid(): boolean {
        if (this.entries.filter(e => !this.entryValid(e)).length > 0) {
            return false;
        }

        return this.entries.length === this.entriesToSet(this.entries, false).size;
    }

    entriesToSet(entries: WebProtocol.IniFileEntry[], includeValue = true): Set<string> {
        const result = new Set<string>();
        entries.forEach(e => result.add(`${e.section}\r\n${e.param}${includeValue ? ('\r\n' + e.value) : ''}`));

        return result;
    }

    entriesChanged(): boolean {
        if (this.entries.length !== this.pristineEntries.length) {
            return true;
        }

        const currentSet = this.entriesToSet(this.entries, true),
            pristineSet = this.entriesToSet(this.pristineEntries, true);

        return [...currentSet].filter(e => !pristineSet.has(e)).length > 0;
    }

    addEntry(): void {
        const entry = new WebProtocol.IniFileEntry();
        entry.section = entry.param = entry.value= '';
        this.entries.push(entry);
    }

    updateGameBranch(): void {
        const body = new WebProtocol.GameBranchUpdateRequest();
        if (this.entriesChanged()) body.iniConfig = this.entries;
        this.loading$.next(true);

        this.heliosAdminService
            .updateGameBranch(body, this.data.branch.id, this.data.branch.rev)
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
                            this.data.branch = response.branch;
                            this.entries = this.data.branch.iniConfig.map(e => WebProtocol.IniFileEntry.fromJson(e.toJson()));
                            this.pristineEntries = this.data.branch.iniConfig.map(e => WebProtocol.IniFileEntry.fromJson(e.toJson()));
                            Object.keys(body).filter(key => body[key] !== null).forEach(key => this.data.branch[key] = body[key]);
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
