import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnInit, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { BehaviorSubject, Subject } from 'rxjs';
import { finalize, takeUntil } from 'rxjs/operators';
import { ClipboardService } from '../../../core/services/clipboard.service';
import { NotificationService } from '../../../core/services/notification.service';
import { HeliosAdminService } from '../../../protocol/web-admin-protocol.service';
import * as WebProtocol from '../../../protocol/web-protocol.data';

@Component({
    selector: 'm-settings',
    templateUrl: './settings.component.html',
    styleUrls: ['./settings.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class SettingsComponent implements OnInit, OnDestroy {
    public config: any;
    destroy$: Subject<any>;
    loading$: BehaviorSubject<boolean>;
    settings: WebProtocol.Settings;
    pristineSettings: WebProtocol.Settings;

    constructor(
        private router: Router,
        private cdr: ChangeDetectorRef,
        private clipboardService: ClipboardService,
        private heliosAdminService: HeliosAdminService,
        private notificationService: NotificationService,
    ) { }

    ngOnInit(): void {
        this.destroy$ = new Subject();
        this.loading$ = new BehaviorSubject<boolean>(false);
        this.getSettings();
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
        this.loading$.complete();
    }

    isInitialized(): boolean {
        return !!(this.settings && this.pristineSettings);
    }

    settingsChanged(): boolean {
        if (!this.isInitialized()) {
            return false;
        }

        return (+this.settings.personnelSessionDuration !== this.pristineSettings.personnelSessionDuration)
            || +this.settings.clientSessionDuration !== this.pristineSettings.clientSessionDuration
            || +this.settings.registerConfirmCodeLifetime !== this.pristineSettings.registerConfirmCodeLifetime
            || +this.settings.phoneConfirmCodeLifetime !== this.pristineSettings.phoneConfirmCodeLifetime
            || +this.settings.passwordResetConfirmCodeLifetime !== this.pristineSettings.passwordResetConfirmCodeLifetime;
    }

    settingsValid(): boolean {
        if (!this.isInitialized()) {
            return false;
        }

        return +this.settings.personnelSessionDuration > 0
            && +this.settings.clientSessionDuration > 0
            && +this.settings.registerConfirmCodeLifetime > 0
            && +this.settings.phoneConfirmCodeLifetime > 0
            && +this.settings.passwordResetConfirmCodeLifetime > 0;
    }

    onCiApiKeyCopy(ciApiKey: string): void {
        this.clipboardService.copy(ciApiKey);
        this.notificationService.info('CI API key copied to clipboard');
    }

    // REST API
    getSettings(): void {
        this.loading$.next(true);

        this.heliosAdminService
            .getSettings()
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe((response: WebProtocol.Settings) => {
                this.settings = response;
                this.pristineSettings = WebProtocol.Settings.fromJson(response.toJson());
                this.cdr.detectChanges();
            });
    }

    updateSettings(): void {
        if (!this.isInitialized()) {
            return;
        }

        const body = new WebProtocol.SettingsUpdateRequest();
        body.personnelSessionDuration = (+this.settings.personnelSessionDuration !== this.pristineSettings.personnelSessionDuration) ? this.settings.personnelSessionDuration : null;
        body.clientSessionDuration = (+this.settings.clientSessionDuration !== this.pristineSettings.clientSessionDuration) ? this.settings.clientSessionDuration : null;
        body.registerConfirmCodeLifetime = (+this.settings.registerConfirmCodeLifetime !== this.pristineSettings.registerConfirmCodeLifetime) ? this.settings.registerConfirmCodeLifetime : null;
        body.phoneConfirmCodeLifetime = (+this.settings.phoneConfirmCodeLifetime !== this.pristineSettings.phoneConfirmCodeLifetime) ? this.settings.phoneConfirmCodeLifetime : null;
        body.passwordResetConfirmCodeLifetime = (+this.settings.passwordResetConfirmCodeLifetime !== this.pristineSettings.passwordResetConfirmCodeLifetime) ? this.settings.passwordResetConfirmCodeLifetime : null;
        this.loading$.next(true);

        this.heliosAdminService
            .updateSettings(body)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe((response: WebProtocol.GenericResponse) => {
                response.result
                    ? this.notificationService.success('Settings updated')
                    : this.notificationService.error('Settings not updated');

                if (response.result) {
                    this.cdr.detectChanges();
                    this.getSettings();
                }
            });
    }

    regenerateCiApiKey(): void {
        if (!this.isInitialized()) {
            return;
        }

        this.loading$.next(true);

        this.heliosAdminService
            .regenerateCiApiKey(new WebProtocol.Empty())
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe((response: WebProtocol.RegenerateCiApiKeyResponse) => {
                this.notificationService.success('CI API key updated');
                this.settings.ciApiKey = response.ciApiKey;
                this.pristineSettings.ciApiKey = response.ciApiKey;
                this.cdr.detectChanges();
            });
    }
}
