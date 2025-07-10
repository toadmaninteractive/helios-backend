import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { BehaviorSubject, Observable, of } from 'rxjs';
import { filter, finalize, switchMap, tap } from 'rxjs/operators';
import { Constants } from '../../shared/config/constants';
import { HeliosAuthService } from '../../protocol/web-auth-protocol.service';
import * as WebProtocol from '../../protocol/web-protocol.data';

@Injectable({
    providedIn: 'root',
})
export class AccountService {
    private isInitializing$ = new BehaviorSubject(true);
    private isSignedIn$ = new BehaviorSubject<boolean | null>(null);
    isSigningIn$ = new BehaviorSubject(false);
    profile$ = new BehaviorSubject<WebProtocol.PersonnelAccountProfile | null>(null);

    constructor(
        private router: Router,
        private heliosAuthService: HeliosAuthService,
    ) {
        this.initialize();
    }

    private initialize(): void {
        if (!this.isInitializing$.getValue()) {
            this.isInitializing$.next(true);
        }

        this.heliosAuthService
            .getPersonnelStatus()
            .pipe(
                switchMap(response => response.loggedIn ? this.heliosAuthService.getMyPersonnelProfile() : of(null)),
                finalize(() => this.isInitializing$.next(false)),
            )
            .subscribe((profile: WebProtocol.PersonnelAccountProfile | null) => {
                this.profile$.next(profile);
                this.isSignedIn$.next(profile instanceof WebProtocol.PersonnelAccountProfile);
            });
    }

    private reset(): void {
        this.isSignedIn$.next(false);
        this.profile$.next(null);
        this.router.navigate([Constants.loginUrl]);
    }

    isSignedIn(): Observable<boolean> {
        return this.isSignedIn$.pipe(filter(value => value !== null));
    }

    signIn(username: string, password: string): Observable<WebProtocol.PersonnelAccountProfile | WebProtocol.PersonnelLoginResponse> {
        if (this.isSigningIn$.getValue()) {
            return;
        }

        this.isSigningIn$.next(true);

        const request = new WebProtocol.PersonnelLoginRequest();
        request.username = username;
        request.password = password;

        return this.heliosAuthService
            .loginPersonnel(request)
            .pipe(
                switchMap(response => response.result ? this.heliosAuthService.getMyPersonnelProfile() : of(response)),
                tap(profile => {
                    if (profile instanceof WebProtocol.PersonnelAccountProfile) {
                        // Success
                        this.profile$.next(profile);
                        this.isSignedIn$.next(true);
                    } else {
                        // Failure
                        this.profile$.next(null);
                    }
                }),
                finalize(() => this.isSigningIn$.next(false)),
            );
    }

    signOut(): Observable<WebProtocol.GenericResponse> {
        return this.heliosAuthService
            .logoutPersonnel(new WebProtocol.Empty())
            .pipe(
                tap((response: WebProtocol.GenericResponse) => {
                    console.log(`Sign out ${response.result ? 'successful' : 'failure'}`);
                    this.reset();
                })
            );
    }
}
