import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { PagesComponent } from './pages.component';
import { ErrorPageComponent } from './error-page/error-page.component';
import { AuthorizedGuard } from '../../core/guards/authorized.guard';
import { NotAuthorizedGuard } from '../../core/guards/not-authorized.guard';

const routes: Routes = [
    {
        path: '',
        component: PagesComponent,
        canActivate: [AuthorizedGuard],
        children: [
            {
                path: '',
                redirectTo: 'games',
                pathMatch: 'full'
            },
            {
                path: 'games',
                loadChildren: './games/games.module#GamesModule'
            },
        ]
    },
    {
        path: 'login',
        canActivate: [NotAuthorizedGuard],
        loadChildren: './auth/auth.module#AuthModule',
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
