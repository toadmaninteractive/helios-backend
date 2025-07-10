import { Component, Inject, OnInit, OnDestroy } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material';
import { HttpEvent, HttpEventType, HttpUploadProgressEvent } from '@angular/common/http';
import { BehaviorSubject, from, Observable, Subject } from 'rxjs';
import { filter, finalize, mergeMap, takeUntil, tap } from 'rxjs/operators';
import { FileSystemFileEntry, NgxFileDropEntry } from 'ngx-file-drop';
import { FileUploadDialogData } from './file-upload-dialog.interface';
import { UploadService } from '../../../../core/services/upload.service';

interface FileItem {
    path: string;
    file: File;
}

@Component({
    selector: 'm-file-upload-dialog',
    templateUrl: 'file-upload-dialog.component.html',
    styleUrls: ['./file-upload-dialog.component.scss'],
})
export class FileUploadDialogComponent implements OnInit, OnDestroy {
    destroy$ = new Subject();
    loading$ = new BehaviorSubject<boolean>(true);
    filePaths$ = new BehaviorSubject<string[]>([]);
    buildId: number;
    fileEntries: NgxFileDropEntry[];
    stripPrefix: string;
    isArchive: boolean;
    subjectMap = new Map<string, BehaviorSubject<number>>();

    constructor(
        public dialogRef: MatDialogRef<FileUploadDialogComponent>,
        @Inject(MAT_DIALOG_DATA) public data: FileUploadDialogData,
        private uploadService: UploadService,
    ) {
        this.buildId = data.buildId;
        this.fileEntries = data.fileEntries.filter(f => f.fileEntry.isFile);
        this.stripPrefix = data.stripPrefix;
        this.isArchive = data.isArchive;
    }

    ngOnInit(): void {
        const items: FileItem[] = [],
            fileCount = this.fileEntries.length;

        this.fileEntries.forEach(fe => {
            const fsfe = fe.fileEntry as FileSystemFileEntry;

            fsfe.file((file: File) => {
                const filePath = (file['webkitRelativePath'] || file.name).trim().replace(new RegExp(`^${this.stripPrefix}`, 'gi'), ''),
                    item = <FileItem> { path: filePath, file: file };

                items.push(item);

                if (items.length === fileCount) {
                    this.uploadFiles(items);
                }
            });
        });
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
        this.loading$.complete();
        this.filePaths$.complete();
    }

    private uploadFile(fileItem: FileItem): Observable<Object> {
        const filePaths = this.filePaths$.getValue(),
            subj$ = new BehaviorSubject<number>(0);

        this.subjectMap.set(fileItem.path, subj$);
        filePaths.unshift(fileItem.path);
        this.filePaths$.next(filePaths);

        return this.uploadService
            .uploadBuildFile(this.buildId, fileItem.file, fileItem.path)
            .pipe(
                filter((event: HttpEvent<any>) => event.type === HttpEventType.UploadProgress),
                tap((event: HttpUploadProgressEvent) => subj$.next(Math.floor(1000 * event.loaded / event.total) / 10)),
                finalize(() => subj$.complete()),
            );
    }

    private uploadArchive(fileItem: FileItem): Observable<Object> {
        const filePaths = this.filePaths$.getValue(),
            subj$ = new BehaviorSubject<number>(0);

        this.subjectMap.set(fileItem.path, subj$);
        filePaths.unshift(fileItem.path);
        this.filePaths$.next(filePaths);

        return this.uploadService
            .uploadBuildArchive(this.buildId, fileItem.file, fileItem.path)
            .pipe(
                filter((event: HttpEvent<any>) => event.type === HttpEventType.UploadProgress),
                tap((event: HttpUploadProgressEvent) => subj$.next(Math.floor(1000 * event.loaded / event.total) / 10)),
                finalize(() => subj$.complete()),
            );
    }

    private uploadFiles(fileItems: FileItem[]): void {
        from(fileItems)
            .pipe(
                // takeUntil(this.destroy$),
                mergeMap(fileItem => this.isArchive ? this.uploadArchive(fileItem) : this.uploadFile(fileItem), null, 1),
                takeUntil(this.destroy$),
                finalize(() => {
                    this.loading$.next(false);

                    if (this.isArchive) {
                        this.dialogRef.close(true);
                    }
                }),
            )
            .subscribe(result => { /* */ });
    }
}
