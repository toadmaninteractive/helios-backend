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
import { SweetAlert2Module } from '@sweetalert2/ngx-sweetalert2';
import { ComponentsModule } from '../../../components/components.module';
import { SharedModule } from '../../../shared/shared.module';
import { CategoriesRoutingModule } from './categories-routing.module';
import { CategoryEditDialogComponent } from './category-edit-dialog/category-edit-dialog.component';
import { CategoriesOverviewComponent } from './categories-overview/categories-overview.component';

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
        SweetAlert2Module,

        // Project modules
        SharedModule,
        ComponentsModule,

        // Child routing module
        CategoriesRoutingModule,
    ],
    exports: [
        CategoryEditDialogComponent,
    ],
    providers: [],
    declarations: [
        CategoryEditDialogComponent,
        CategoriesOverviewComponent,
    ],
    entryComponents: [
        CategoryEditDialogComponent,
    ]
})
export class CategoriesModule { }
