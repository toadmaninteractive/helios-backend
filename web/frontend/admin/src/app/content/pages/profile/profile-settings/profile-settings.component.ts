import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnInit, OnDestroy } from '@angular/core';
import { Router } from '@angular/router';
import { BehaviorSubject, Subject } from 'rxjs';
import { finalize, takeUntil } from 'rxjs/operators';
import { SubheaderService } from '../../../../core/services/metronic/layout/subheader.service';
import { AccountService } from '../../../../core/services/account.service';
import { ClipboardService } from '../../../../core/services/clipboard.service';
import { NotificationService } from '../../../../core/services/notification.service';
import { HeliosAuthService } from '../../../../protocol/web-auth-protocol.service';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

@Component({
    selector: 'm-profile-settings',
    templateUrl: './profile-settings.component.html',
    styleUrls: ['./profile-settings.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ProfileSettingsComponent implements OnInit, OnDestroy {
    public config: any;
    destroy$ = new Subject();
    loading$ = new BehaviorSubject<boolean>(false);
    apiKey: string;
    pristineApiKey: string;

    constructor(
        private router: Router,
        private cdr: ChangeDetectorRef,
        private accountService: AccountService,
        private clipboardService: ClipboardService,
        private heliosAuthService: HeliosAuthService,
        private notificationService: NotificationService,
        private subheaderService: SubheaderService,
    ) { }

    ngOnInit(): void {
        this.refresh();
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
        this.loading$.complete();
    }

    isInitialized(): boolean {
        return typeof this.apiKey === 'string'
            && typeof this.pristineApiKey === 'string';
    }

    refresh(): void {
        this.heliosAuthService.getMyPersonnelProfile()
            .pipe(takeUntil(this.destroy$))
            .subscribe(profile => {
                this.subheaderService.setTitle(profile.name);

                const role = profile.isSuperadmin ? 'Super Administrator' : profile.isGameManager ? 'Maintainer' : null,
                    badgeClass = profile.isSuperadmin ? 'm-badge--warning' : profile.isGameManager ? 'm-badge--info' : null;

                if (role && badgeClass) {
                    this.subheaderService.setStatus(role, badgeClass);
                }

                this.apiKey = profile.apiKey || '';
                this.pristineApiKey = profile.apiKey || '';
                this.cdr.detectChanges();
            });
    }

    onApiKeyCopy(apiKey: string): void {
        this.clipboardService.copy(apiKey);
        this.notificationService.info('API key copied to clipboard');
    }

    // REST API
    regeneratePersonnelApiKey(): void {
        this.loading$.next(true);

        this.heliosAuthService
            .regeneratePersonnelApiKey(new WebProtocol.Empty())
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe((response: WebProtocol.PersonnelApiKeyUpdateResponse) => {
                this.apiKey = response.apiKey;
                this.pristineApiKey = response.apiKey;
                this.notificationService.info('API key updated');
                this.cdr.detectChanges();
            });
    }
}
