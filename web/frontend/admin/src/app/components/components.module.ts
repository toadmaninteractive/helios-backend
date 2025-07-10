import { RouterModule } from '@angular/router';
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { QuickSidebarComponent } from './layout/quick-sidebar/quick-sidebar.component';
import { ScrollTopComponent } from './layout/scroll-top/scroll-top.component';
import { TooltipsComponent } from './layout/tooltips/tooltips.component';
import { ListSettingsComponent } from './layout/quick-sidebar/list-settings/list-settings.component';
import { MessengerModule } from './layout/quick-sidebar/messenger/messenger.module';
import { SharedModule } from '../shared/shared.module';
import { ListTimelineModule } from './layout/quick-sidebar/list-timeline/list-timeline.module';
import { PerfectScrollbarModule } from 'ngx-perfect-scrollbar';
import { AlertModule } from './general/alert/alert.module';
import { GravatarModule } from './general/gravatar/gravatar.module';
import { NoticeComponent } from './general/notice/notice.component';
import { PortletModule } from './general/portlet/portlet.module';

import {
    MatButtonModule,
    MatCheckboxModule,
    MatIconModule,
    MatInputModule,
    MatPaginatorModule,
    MatProgressBarModule,
    MatProgressSpinnerModule,
    MatSelectModule,
    MatSortModule,
    MatTableModule,
    MatTabsModule,
    MatTooltipModule,
} from '@angular/material';

@NgModule({
    declarations: [
        QuickSidebarComponent,
        ScrollTopComponent,
        TooltipsComponent,
        ListSettingsComponent,
        NoticeComponent,
    ],
    exports: [
        QuickSidebarComponent,
        ScrollTopComponent,
        TooltipsComponent,
        ListSettingsComponent,
        NoticeComponent,

        PortletModule,
        AlertModule,
        GravatarModule,
    ],
    imports: [
        CommonModule,
        RouterModule,
        PerfectScrollbarModule,
        MessengerModule,
        ListTimelineModule,
        SharedModule,
        PortletModule,
        AlertModule,
        GravatarModule,

        MatButtonModule,
        MatCheckboxModule,
        MatIconModule,
        MatInputModule,
        MatPaginatorModule,
        MatProgressBarModule,
        MatProgressSpinnerModule,
        MatSelectModule,
        MatSortModule,
        MatTableModule,
        MatTabsModule,
        MatTooltipModule,
    ]
})
export class ComponentsModule { }
