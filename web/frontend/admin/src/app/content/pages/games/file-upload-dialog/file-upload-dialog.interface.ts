import { NgxFileDropEntry } from 'ngx-file-drop';

export interface FileUploadDialogData {
    buildId: number;
    fileEntries: NgxFileDropEntry[];
    stripPrefix: string;
    isArchive: boolean;
}
