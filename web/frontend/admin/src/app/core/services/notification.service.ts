import { Injectable } from '@angular/core';
import { IndividualConfig, ToastrService } from 'ngx-toastr';

@Injectable({
    providedIn: 'root',
})
export class NotificationService {
    constructor(private toastr: ToastrService) { }

    private defaultOptions(): Partial<IndividualConfig> {
        return {
            closeButton: true,
            positionClass: 'toast-top-right',
            progressAnimation: 'increasing',
            progressBar: true,
            timeOut: 3000,
        };
    }

    error(message: string): void {
        setTimeout(() => this.toastr.error(message, null, this.defaultOptions()));
    }

    warning(message: string): void {
        setTimeout(() => this.toastr.warning(message, null, this.defaultOptions()));
    }

    info(message: string): void {
        setTimeout(() => this.toastr.info(message, null, this.defaultOptions()));
    }

    success(message: string): void {
        setTimeout(() => this.toastr.success(message, null, this.defaultOptions()));
    }
}
