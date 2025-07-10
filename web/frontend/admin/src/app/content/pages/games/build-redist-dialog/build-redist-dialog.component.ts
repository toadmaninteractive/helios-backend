import { Component, Inject, OnInit, OnDestroy } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material';
import { BehaviorSubject, Subject } from 'rxjs';
import { finalize, takeUntil } from 'rxjs/operators';
import { BuildRedistDialogData } from './build-redist-dialog.interface';
import { HeliosAdminService } from '../../../../protocol/web-admin-protocol.service';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

@Component({
    selector: 'm-build-redist-dialog',
    templateUrl: 'build-redist-dialog.component.html',
})
export class BuildRedistDialogComponent implements OnInit, OnDestroy {
    destroy$: Subject<any>;
    loading$: BehaviorSubject<boolean>;
    entries: WebProtocol.RedistributableEntry[];
    pristineEntries: WebProtocol.RedistributableEntry[];
    errorMessage?: string = null;

    constructor (
        public dialogRef: MatDialogRef<BuildRedistDialogComponent>,
        @Inject(MAT_DIALOG_DATA) public data: BuildRedistDialogData,
        private heliosAdminService: HeliosAdminService,
    ) {
        this.entries = data.build.redistributables.map(e => WebProtocol.RedistributableEntry.fromJson(e.toJson()));
        this.pristineEntries = data.build.redistributables.map(e => WebProtocol.RedistributableEntry.fromJson(e.toJson()));
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

    entryValid(entry: WebProtocol.RedistributableEntry): boolean {
        return entry.name.trim().length > 0
            && entry.url.trim().length > 0;
    }

    entriesValid(): boolean {
        if (this.entries.filter(e => !this.entryValid(e)).length > 0) {
            return false;
        }

        return this.entries.length === this.entriesToSet(this.entries, false).size;
    }

    entriesToSet(entries: WebProtocol.RedistributableEntry[], includeUrl = true): Set<string> {
        return entries
            .map(e => `${e.name}\r\n${includeUrl ? ('\r\n' + e.url) : ''}`)
            .reduce((acc, s) => acc.add(s), new Set<string>());
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
        const entry = new WebProtocol.RedistributableEntry();
        entry.name = entry.url = '';
        this.entries.push(entry);
    }

    updateBuild(): void {
        const body = new WebProtocol.BuildUpdateRequest();
        if (this.entriesChanged()) body.redistributables = this.entries;
        this.loading$.next(true);

        this.heliosAdminService
            .updateBuild(body, this.data.build.id, this.data.build.rev)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe((response: WebProtocol.BuildUpdateResponse) => {
                if (response.result) {
                    this.dialogRef.close(response.build);
                } else {
                    switch (response.error) {
                        case WebProtocol.BuildUpdateError.Failure:
                            this.errorMessage = 'Server failed to process request';
                            break;
                        case WebProtocol.BuildUpdateError.RevMismatch:
                            this.errorMessage = 'Game build updated remotely, try submit again';
                            this.data.build = response.build;
                            this.entries = this.data.build.redistributables.map(e => WebProtocol.RedistributableEntry.fromJson(e.toJson()));
                            this.pristineEntries = this.data.build.redistributables.map(e => WebProtocol.RedistributableEntry.fromJson(e.toJson()));
                            Object.keys(body).filter(key => body[key] !== null).forEach(key => this.data.build[key] = body[key]);
                            break;
                        case WebProtocol.BuildUpdateError.NothingToUpdate:
                            this.errorMessage = 'Nothing to update';
                            break;
                    }
                }
            });
    }
}
