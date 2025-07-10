import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { PagesComponent } from './pages.component';
import { ErrorPageComponent } from './error-page/error-page.component';
import { AuthorizedGuard } from '../../core/guards/authorized.guard';
import { GameManagerGuard } from '../../core/guards/game-manager.guard';
import { NotAuthorizedGuard } from '../../core/guards/not-authorized.guard';
import { SuperadminGuard } from '../../core/guards/superadmin.guard';

const routes: Routes = [
    {
        path: '',
        component: PagesComponent,
        canActivate: [AuthorizedGuard],
        children: [
            {
                path: 'profile',
                loadChildren: () => import('./profile/profile.module').then(m => m.ProfileModule),
            },
            {
                path: 'dashboard',
                loadChildren: () => import('./dashboard/dashboard.module').then(m => m.DashboardModule),
            },
            {
                path: 'games',
                loadChildren: () => import('./games/games.module').then(m => m.GamesModule),
                canActivate: [GameManagerGuard],
            },
            {
                path: 'categories',
                loadChildren: () => import('./categories/categories.module').then(m => m.CategoriesModule),
                canActivate: [GameManagerGuard],
            },
            {
                path: 'clients',
                loadChildren: () => import('./clients/clients.module').then(m => m.ClientsModule),
                canActivate: [SuperadminGuard],
            },
            {
                path: 'personnel',
                loadChildren: () => import('./personnel/personnel.module').then(m => m.PersonnelModule),
                canActivate: [SuperadminGuard],
            },
            {
                path: 'settings',
                loadChildren: () => import('./settings/settings.module').then(m => m.SettingsModule),
                canActivate: [SuperadminGuard],
            },
            {
                path: '',
                redirectTo: 'dashboard',
                pathMatch: 'full',
            },
        ]
    },
    {
        path: 'login',
        canActivate: [NotAuthorizedGuard],
        loadChildren: () => import('./auth/auth.module').then(m => m.AuthModule),
    },
    {
        path: '404',
        component: ErrorPageComponent
    },
    {
        path: 'error/:type',
        component: ErrorPageComponent
    },
];

@NgModule({
    imports: [RouterModule.forChild(routes)],
    exports: [RouterModule]
})
export class PagesRoutingModule { }
