import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import {
    MatButtonModule,
    MatCheckboxModule,
    MatDividerModule,
    MatIconModule,
    MatInputModule,
    MatMenuModule,
    MatPaginatorModule,
    MatSortModule,
    MatTableModule,
    MatTooltipModule
} from '@angular/material';
import { PerfectScrollbarModule } from 'ngx-perfect-scrollbar';
import { SweetAlert2Module } from '@sweetalert2/ngx-sweetalert2';
import { SharedModule } from '../../../shared/shared.module';
import { ComponentsModule } from '../../../components/components.module';
import { PersonnelRoutingModule } from './personnel-routing.module';
import { PersonnelAccountManageComponent } from './personnel-account-manage/personnel-account-manage.component';
import { PersonnelAccountsComponent } from './personnel-accounts/personnel-accounts.component';
import { PersonnelGroupManageComponent } from './personnel-group-manage/personnel-group-manage.component';
import { PersonnelGroupsComponent } from './personnel-groups/personnel-groups.component';

@NgModule({
    imports: [
        // Angular modules
        CommonModule,
        FormsModule,

        // Angular Material modules
        MatButtonModule,
        MatCheckboxModule,
        MatDividerModule,
        MatIconModule,
        MatInputModule,
        MatMenuModule,
        MatPaginatorModule,
        MatSortModule,
        MatTableModule,
        MatTooltipModule,

        // Third party modules
        PerfectScrollbarModule,
        SweetAlert2Module,

        // Project modules
        SharedModule,
        ComponentsModule,

        // Child routing module
        PersonnelRoutingModule,
    ],
    providers: [],
    declarations: [
        PersonnelAccountManageComponent,
        PersonnelAccountsComponent,
        PersonnelGroupManageComponent,
        PersonnelGroupsComponent,
    ]
})
export class PersonnelModule { }
