import { Component, Inject, OnInit, OnDestroy } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material';
import { BehaviorSubject, Subject } from 'rxjs';
import { finalize, takeUntil } from 'rxjs/operators';
import { RegistryConfigurationDialogData } from './registry-configuration-dialog.data';
import { HeliosAdminService } from '../../../../protocol/web-admin-protocol.service';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

@Component({
    selector: 'm-registry-configuration-dialog',
    templateUrl: 'registry-configuration-dialog.component.html',
})
export class RegistryConfigurationDialogComponent implements OnInit, OnDestroy {
    destroy$: Subject<any>;
    loading$: BehaviorSubject<boolean>;
    entries: WebProtocol.RegistryConfigEntry[];
    pristineEntries: WebProtocol.RegistryConfigEntry[];
    errorMessage?: string = null;
    valueTypes: Array<any> = [
        { value: WebProtocol.RegistryValueType.String, label: WebProtocol.RegistryValueType.getDescription(WebProtocol.RegistryValueType.String) },
        { value: WebProtocol.RegistryValueType.Binary, label: WebProtocol.RegistryValueType.getDescription(WebProtocol.RegistryValueType.Binary) },
        { value: WebProtocol.RegistryValueType.Dword, label: WebProtocol.RegistryValueType.getDescription(WebProtocol.RegistryValueType.Dword) },
        { value: WebProtocol.RegistryValueType.Qword, label: WebProtocol.RegistryValueType.getDescription(WebProtocol.RegistryValueType.Qword) },
        // { value: WebProtocol.RegistryValueType.MultiString, label: WebProtocol.RegistryValueType.getDescription(WebProtocol.RegistryValueType.MultiString) },
        // { value: WebProtocol.RegistryValueType.ExpandableString, label: WebProtocol.RegistryValueType.getDescription(WebProtocol.RegistryValueType.ExpandableString) },
    ];

    constructor (
        public dialogRef: MatDialogRef<RegistryConfigurationDialogComponent>,
        @Inject(MAT_DIALOG_DATA) public data: RegistryConfigurationDialogData,
        private heliosAdminService: HeliosAdminService,
    ) {
        this.entries = data.branch.registryConfig.map(e => WebProtocol.RegistryConfigEntry.fromJson(e.toJson()));
        this.pristineEntries = data.branch.registryConfig.map(e => WebProtocol.RegistryConfigEntry.fromJson(e.toJson()));
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

    entryValid(entry: WebProtocol.RegistryConfigEntry): boolean {
        const isValueValid = (entry.valueType === WebProtocol.RegistryValueType.String && entry.value.length >= 0)
            || (entry.valueType === WebProtocol.RegistryValueType.Binary && /^([0-9a-fA-F]{2}\s*)*$/.test(entry.value))
            || (entry.valueType === WebProtocol.RegistryValueType.Dword && /^-?[0-9]+$/.test(entry.value) && +entry.value >= -2147483648 && +entry.value <= 2147483647)
            || (entry.valueType === WebProtocol.RegistryValueType.Qword && /^-?[0-9]+$/.test(entry.value) && +entry.value >= Number.MIN_SAFE_INTEGER && +entry.value <= Number.MAX_SAFE_INTEGER);

        return entry.path.trim().length > 0
            && entry.key.trim().length > 0
            && isValueValid;
    }

    entriesValid(): boolean {
        if (this.entries.filter(e => !this.entryValid(e)).length > 0) {
            return false;
        }

        return this.entries.length === this.entriesToSet(this.entries, false).size;
    }

    entriesToSet(entries: WebProtocol.RegistryConfigEntry[], includeValue = true): Set<string> {
        const result = new Set<string>();
        entries.forEach(e => result.add(`${e.path}\r\n${e.key}${includeValue ? ('\r\n' + e.value) : ''}`));

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
        const entry = new WebProtocol.RegistryConfigEntry();
        entry.path = entry.key = entry.value = '';
        entry.valueType = WebProtocol.RegistryValueType.String;
        this.entries.push(entry);
    }

    updateGameBranch(): void {
        const body = new WebProtocol.GameBranchUpdateRequest();
        if (this.entriesChanged()) body.registryConfig = this.entries;
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
                            this.entries = this.data.branch.registryConfig.map(e => WebProtocol.RegistryConfigEntry.fromJson(e.toJson()));
                            this.pristineEntries = this.data.branch.registryConfig.map(e => WebProtocol.RegistryConfigEntry.fromJson(e.toJson()));
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
