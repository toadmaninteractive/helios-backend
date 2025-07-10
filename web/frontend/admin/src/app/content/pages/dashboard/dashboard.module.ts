import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatIconModule, MatTooltipModule } from '@angular/material';
import { ChartsModule } from 'ng2-charts';
import { PerfectScrollbarModule } from 'ngx-perfect-scrollbar';
import { SharedModule } from '../../../shared/shared.module';
import { PortletModule } from '../../../components/general/portlet/portlet.module';
import { DashboardRoutingModule } from './dashboard-routing.module';
import { DashboardOverviewComponent } from './dashboard-overview/dashboard-overview.component';
import { FavouriteGamesComponent } from './favourite-games/favourite-games.component';
import { PopularGamesComponent } from './popular-games/popular-games.component';
import { RecentBuildsComponent } from './recent-builds/recent-builds.component';

@NgModule({
    imports: [
        // Angular modules
        CommonModule,

        // Angular Material modules
        MatIconModule,
        MatTooltipModule,

        // Third party modules
        ChartsModule,
        PerfectScrollbarModule,

        // Project modules
        SharedModule,
        PortletModule,

        // Child routing module
        DashboardRoutingModule,
    ],
    exports: [
        FavouriteGamesComponent,
        PopularGamesComponent,
        RecentBuildsComponent,
    ],
    providers: [],
    declarations: [
        FavouriteGamesComponent,
        PopularGamesComponent,
        RecentBuildsComponent,
        DashboardOverviewComponent,
    ]
})
export class DashboardModule { }
