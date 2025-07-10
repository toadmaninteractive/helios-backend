import { Injectable } from '@angular/core';
import { CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot, Router } from '@angular/router';
import { Observable } from 'rxjs';
import { tap } from 'rxjs/operators';
import { AccountService } from '../services/account.service';

@Injectable({
    providedIn: 'root',
})
export class AuthorizedGuard implements CanActivate {
    constructor (private router: Router, private accountService: AccountService) { }

    canActivate(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<boolean> {
        return this.accountService.isSignedIn()
            .pipe(
                tap((signedIn: boolean) => !signedIn && this.router.navigate(['/login'])) // was: /session/signin
            );
    }
}
