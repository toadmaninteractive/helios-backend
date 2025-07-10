import { Injectable } from '@angular/core';
import { CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot, Router, UrlTree } from '@angular/router';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { Constants } from '../../shared/config/constants';
import { AccountService } from '../services/account.service';

@Injectable({
    providedIn: 'root',
})
export class NotAuthorizedGuard implements CanActivate {
    constructor (private router: Router, private accountService: AccountService) { }

    canActivate(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<boolean | UrlTree> {
        return this.accountService.isSignedIn()
            .pipe(map(signedIn => !signedIn || this.router.parseUrl(Constants.defaultUrl)));
    }
}
