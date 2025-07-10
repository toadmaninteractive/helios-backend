import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { BehaviorSubject, Observable } from 'rxjs';
import { filter, finalize, tap } from 'rxjs/operators';
import { HeliosAuthService } from '../../protocol/web-auth-protocol.service';
import * as WebProtocol from '../../protocol/web-protocol.data';

@Injectable({
    providedIn: 'root',
})
export class AccountService {
    private isInitializing$ = new BehaviorSubject(true);
    private isSignedIn$ = new BehaviorSubject<boolean | null>(null);
    isSigningIn$ = new BehaviorSubject(false);
    userId$ = new BehaviorSubject<number | null>(null);
    username$ = new BehaviorSubject<string | null>(null);
    email$ = new BehaviorSubject<string | null>(null);

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
            .pipe(finalize(() => this.isInitializing$.next(false)))
            .subscribe((response: WebProtocol.PersonnelStatusResponse) => {
                if (response.loggedIn) {
                    this.userId$.next(response.userId);
                    this.username$.next(response.username);
                    this.email$.next(response.email);
                }

                this.isSignedIn$.next(response.loggedIn);
            });
    }

    private reset(): void {
        this.isSignedIn$.next(false);
        this.userId$.next(null);
        this.username$.next(null);
        this.email$.next(null);
        this.router.navigate(['/login']); // FIXME: ensure valid path, was: /session/signin
    }

    isSignedIn(): Observable<boolean> {
        return this.isSignedIn$.pipe(filter(value => value !== null));
    }

    signIn(username: string, password: string): Observable<WebProtocol.PersonnelLoginResponse> {
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
                tap((response: WebProtocol.PersonnelLoginResponse) => {
                    if (response.result) {
                        // Success
                        this.userId$.next(response.userId);
                        this.username$.next(response.username);
                        this.email$.next(response.email);
                        this.isSignedIn$.next(true);
                    }
                }),
                finalize(() => this.isSigningIn$.next(false))
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
