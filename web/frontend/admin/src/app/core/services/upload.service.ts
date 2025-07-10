import { Injectable } from '@angular/core';
import { HttpClient, HttpRequest, HttpEventType } from '@angular/common/http';
import { Observable } from 'rxjs';

@Injectable({
    providedIn: 'root',
})
export class UploadService {
    constructor (private httpClient: HttpClient) { }

    public uploadBuildFile(buildId: number, file: File, filePath: string): Observable<Object> {
        const formData = new FormData();
        formData.append('file', file, filePath);

        const url = `/api/admin/build/${buildId}/draft/upload`,
            req = new HttpRequest('POST', url, formData, { reportProgress: true });

        return this.httpClient.request(req);
    }

    public uploadBuildArchive(buildId: number, file: File, filePath: string): Observable<Object> {
        const formData = new FormData();
        formData.append('archive', file, filePath);

        const url = `/api/admin/build/${buildId}/draft/upload/archive`,
            req = new HttpRequest('POST', url, formData, { reportProgress: true });

        return this.httpClient.request(req);
    }
}
