import { Component, ChangeDetectionStrategy, ChangeDetectorRef, HostBinding } from '@angular/core';
import { DomSanitizer, SafeUrl } from '@angular/platform-browser';
import { Router } from '@angular/router';
import { finalize } from 'rxjs/operators';

// Helios stuff
import { Constants } from '../../../../shared/config/constants';
import { AccountService } from '../../../../core/services/account.service';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

@Component({
    selector: 'm-login',
    templateUrl: './login.component.html',
    styleUrls: ['./login.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class LoginComponent {
    @HostBinding('class') classes = 'm-login__signin';
    slackSysadminDmUrl: SafeUrl;
    username = '';
    password = '';
    signingIn = false;
    signInError?: string = null;

    constructor(
        private router: Router,
        private sanitizer: DomSanitizer,
        private cdr: ChangeDetectorRef,
        private accountService: AccountService,
    ) {
        this.slackSysadminDmUrl = this.sanitizer.bypassSecurityTrustUrl(Constants.slackSysadminDmUrl);
    }

    submit() {
        this.signingIn = true;
        this.signInError = null;
        this.cdr.detectChanges();

        this.accountService
            .signIn(this.username, this.password)
            .pipe(
                finalize(() => {
                    this.signingIn = false;
                    this.cdr.detectChanges();
                }),
            )
            .subscribe((response: WebProtocol.PersonnelAccountProfile | WebProtocol.PersonnelLoginResponse) => {
                if (response instanceof WebProtocol.PersonnelAccountProfile) {
                    this.router.navigate(['/dashboard']);
                } else {
                    let message = 'Unpredicted error happened';

                    switch (response.error) {
                        case WebProtocol.PersonnelLoginError.Failure: message = 'Internal server error, try again later'; break;
                        case WebProtocol.PersonnelLoginError.AlreadyLoggedIn: message = 'Already logged in, reload this page'; break;
                        case WebProtocol.PersonnelLoginError.AccountNotExists: message = 'Account does not exist'; break;
                        case WebProtocol.PersonnelLoginError.AccountIsBlocked: message = 'Account is blocked, contact sysadmin'; break;
                        case WebProtocol.PersonnelLoginError.AccountIsDeleted: message = 'Account is deleted, sorry about it'; break;
                        case WebProtocol.PersonnelLoginError.InvalidPassword: message = 'Invalid credentials'; break;
                    }

                    this.signInError = message;
                }
            });
    }
}
