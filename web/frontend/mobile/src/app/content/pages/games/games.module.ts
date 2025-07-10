import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule, Routes } from '@angular/router';
import { SharedModule } from '../../../shared/shared.module';

import { GamesOverviewComponent } from './games-overview/games-overview.component';
import { GameBuildsComponent } from './game-builds/game-builds.component';

const routes: Routes = [
    {
        path: '',
        children: [
            {
                path: '',
                component: GamesOverviewComponent
            },
            {
                path: ':id/builds',
                component: GameBuildsComponent,
            },
        ]
    }
];

@NgModule({
    imports: [
        CommonModule,
        SharedModule,
        RouterModule.forChild(routes),
    ],
    exports: [],
    providers: [],
    declarations: [
        GamesOverviewComponent,
        GameBuildsComponent,
    ],
})
export class GamesModule { }
