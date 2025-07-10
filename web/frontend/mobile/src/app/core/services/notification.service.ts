import { Injectable } from '@angular/core';

@Injectable({
    providedIn: 'root',
})
export class NotificationService {
    constructor() { }

    // FIXME: show OnsenUI error dialog
    error(message: string): void {
        // setTimeout(() => this.toastr.error(message, null, this.defaultOptions()));
    }

    // FIXME: show OnsenUI warning dialog
    warning(message: string): void {
        // setTimeout(() => this.toastr.warning(message, null, this.defaultOptions()));
    }

    // FIXME: show OnsenUI info dialog
    info(message: string): void {
        // setTimeout(() => this.toastr.info(message, null, this.defaultOptions()));
    }

    // FIXME: show OnsenUI success dialog
    success(message: string): void {
        // setTimeout(() => this.toastr.success(message, null, this.defaultOptions()));
    }
}
