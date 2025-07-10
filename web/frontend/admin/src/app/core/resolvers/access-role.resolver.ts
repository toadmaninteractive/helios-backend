import { Injectable } from '@angular/core';
import { ActivatedRouteSnapshot, Resolve } from '@angular/router';
import { combineLatest, Observable } from 'rxjs';
import { first, map } from 'rxjs/operators';
import { AccountService } from '../services/account.service';
import { HeliosAdminService } from '../../protocol/web-admin-protocol.service';
import * as WebProtocol from '../../protocol/web-protocol.data';

@Injectable()
export class AccessRoleResolver implements Resolve<any> {
    constructor (
        private accountService: AccountService,
        private heliosAdminService: HeliosAdminService,
    ) { }

    resolve(route: ActivatedRouteSnapshot): Observable<WebProtocol.AccessRole | null> {
        return combineLatest(
            this.accountService.profile$.asObservable().pipe(first()),
            this.heliosAdminService.getMyRolesForGame(route.params.id),
        ).pipe(
            map(([profile, response]) => {
                if (profile && profile.isSuperadmin) {
                    return WebProtocol.AccessRole.Admin;
                }

                const inheritedRoles = Object.values(response.groupRoles || {})
                    .map(role => WebProtocol.AccessRole.fromJson(role.role));

                return [response.userRole, ...inheritedRoles]
                    .filter(role => role !== null)
                    .sort((a, b) => a < b ? 1 : -1)[0] || null;
            }),
        );
    }
}
