import { Injectable } from '@angular/core';
import { CanActivate, ActivatedRouteSnapshot, RouterStateSnapshot, Router, UrlTree } from '@angular/router';
import { Observable } from 'rxjs';
import { filter, map } from 'rxjs/operators';
import { Constants } from '../../shared/config/constants';
import { AccountService } from '../services/account.service';
import * as WebProtocol from '../../protocol/web-protocol.data';

@Injectable({
    providedIn: 'root',
})
export class GameManagerGuard implements CanActivate {
    constructor (private router: Router, private accountService: AccountService ) { }

    canActivate(route: ActivatedRouteSnapshot, state: RouterStateSnapshot): Observable<boolean | UrlTree> {
        return this.accountService.profile$
            .pipe(
                filter(profile => profile instanceof WebProtocol.PersonnelAccountProfile),
                map(profile => profile.isGameManager ? true : this.router.parseUrl(Constants.defaultUrl)),
            );
    }
}
