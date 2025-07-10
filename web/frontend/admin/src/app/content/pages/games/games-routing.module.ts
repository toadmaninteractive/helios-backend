import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { SuperadminGuard } from '../../../core/guards/superadmin.guard';
import { AccessRoleResolver } from '../../../core/resolvers/access-role.resolver';
import { BuildOverviewComponent } from './build-overview/build-overview.component';
import { DraftBuildManageComponent } from './draft-build-manage/draft-build-manage.component';
import { GamesOverviewComponent } from './games-overview/games-overview.component';
import { GameAclAccountsComponent } from './game-acl-accounts/game-acl-accounts.component';
import { GameAclGroupsComponent } from './game-acl-groups/game-acl-groups.component';
import { GameManageComponent } from './game-manage/game-manage.component';

const routes: Routes = [
    {
        path: '',
        children: [
            {
                path: '',
                component: GamesOverviewComponent,
            },
            {
                path: ':id/manage',
                component: GameManageComponent,
                resolve: { role: AccessRoleResolver },
            },
            {
                path: ':id/acl',
                canActivate: [SuperadminGuard],
                children: [
                    {
                        path: '',
                        pathMatch: 'full',
                        redirectTo: 'accounts',
                    },
                    {
                        path: 'accounts',
                        component: GameAclAccountsComponent,
                    },
                    {
                        path: 'groups',
                        component: GameAclGroupsComponent,
                    },
                ],
            },
            {
                path: ':id/builds/:rev',
                component: BuildOverviewComponent,
                resolve: { role: AccessRoleResolver },
            },
            {
                path: ':id/builds/:rev/draft',
                component: DraftBuildManageComponent,
                resolve: { role: AccessRoleResolver },
            },
        ]
    }
];

@NgModule({
    imports: [RouterModule.forChild(routes)],
    exports: [RouterModule]
})
export class GamesRoutingModule { }
