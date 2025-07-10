import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import {
    MatAutocompleteModule,
    MatButtonToggleModule,
    MatButtonModule,
    MatCheckboxModule,
    MatDialogModule,
    MatDividerModule,
    MatExpansionModule,
    MatIconModule,
    MatInputModule,
    MatMenuModule,
    MatPaginatorModule,
    MatSelectModule,
    MatSortModule,
    MatTableModule,
    MatTooltipModule,
} from '@angular/material';
import { NgxFileDropModule } from 'ngx-file-drop';
import { PerfectScrollbarModule } from 'ngx-perfect-scrollbar';
import { SweetAlert2Module } from '@sweetalert2/ngx-sweetalert2';
import { ComponentsModule } from '../../../components/components.module';
import { SharedModule } from '../../../shared/shared.module';
import { GamesRoutingModule } from './games-routing.module';
import { BranchCreateDialogComponent } from './branch-create-dialog/branch-create-dialog.component';
import { BranchEditDialogComponent } from './branch-edit-dialog/branch-edit-dialog.component';
import { BuildOverviewComponent } from './build-overview/build-overview.component';
import { BuildRedistDialogComponent } from './build-redist-dialog/build-redist-dialog.component';
import { DraftBuildCreateDialogComponent } from './draft-build-create-dialog/draft-build-create-dialog.component';
import { DraftBuildManageComponent } from './draft-build-manage/draft-build-manage.component';
import { FileUploadDialogComponent } from './file-upload-dialog/file-upload-dialog.component';
import { GameAclAccountsComponent } from './game-acl-accounts/game-acl-accounts.component';
import { GameAclGroupsComponent } from './game-acl-groups/game-acl-groups.component';
import { GameCreateDialogComponent } from './game-create-dialog/game-create-dialog.component';
import { GameManageComponent } from './game-manage/game-manage.component';
import { GamesOverviewComponent } from './games-overview/games-overview.component';
import { IniConfigurationDialogComponent } from './ini-configuration-dialog/ini-configuration-dialog.component';
import { RegistryConfigurationDialogComponent } from './registry-configuration-dialog/registry-configuration-dialog.component';

@NgModule({
    imports: [
        // Angular modules
        CommonModule,
        FormsModule,

        // Angular Material modules
        MatAutocompleteModule,
        MatButtonToggleModule,
        MatButtonModule,
        MatCheckboxModule,
        MatDialogModule,
        MatDividerModule,
        MatExpansionModule,
        MatIconModule,
        MatInputModule,
        MatMenuModule,
        MatPaginatorModule,
        MatSelectModule,
        MatSortModule,
        MatTableModule,
        MatTooltipModule,

        // Third party modules
        NgxFileDropModule,
        PerfectScrollbarModule,
        SweetAlert2Module,

        // Project modules
        SharedModule,
        ComponentsModule,

        // Child routing module
        GamesRoutingModule,
    ],
    exports: [
        BranchCreateDialogComponent,
        BranchEditDialogComponent,
        BuildRedistDialogComponent,
        DraftBuildCreateDialogComponent,
        FileUploadDialogComponent,
        GameCreateDialogComponent,
        IniConfigurationDialogComponent,
        RegistryConfigurationDialogComponent,
    ],
    providers: [],
    declarations: [
        BranchCreateDialogComponent,
        BranchEditDialogComponent,
        BuildOverviewComponent,
        BuildRedistDialogComponent,
        DraftBuildCreateDialogComponent,
        DraftBuildManageComponent,
        FileUploadDialogComponent,
        GameAclAccountsComponent,
        GameAclGroupsComponent,
        GameCreateDialogComponent,
        GameManageComponent,
        GamesOverviewComponent,
        IniConfigurationDialogComponent,
        RegistryConfigurationDialogComponent,
    ],
    entryComponents: [
        BranchCreateDialogComponent,
        BranchEditDialogComponent,
        BuildRedistDialogComponent,
        DraftBuildCreateDialogComponent,
        FileUploadDialogComponent,
        GameCreateDialogComponent,
        IniConfigurationDialogComponent,
        RegistryConfigurationDialogComponent,
    ]
})
export class GamesModule { }
