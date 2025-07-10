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
import { SharedModule } from '../../../shared/shared.module';
import { ProfileRoutingModule } from './profile-routing.module';
import { ProfileAclComponent } from './profile-acl/profile-acl.component';
import { ProfileSettingsComponent } from './profile-settings/profile-settings.component';

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

        // Project modules
        SharedModule,

        // Child routing module
        ProfileRoutingModule,
    ],
    providers: [],
    declarations: [
        ProfileAclComponent,
        ProfileSettingsComponent,
    ]
})
export class ProfileModule { }
