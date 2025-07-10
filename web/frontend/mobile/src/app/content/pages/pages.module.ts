import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { HttpClientModule } from '@angular/common/http';

import { SharedModule } from '../../shared/shared.module';
import { PagesRoutingModule } from './pages-routing.module';

import { MenuComponent } from '../layout/menu/menu.component';
import { PagesComponent } from './pages.component';
import { ErrorPageComponent } from './error-page/error-page.component';

import { AuthorizedGuard } from '../../core/guards/authorized.guard';
import { NotAuthorizedGuard } from '../../core/guards/not-authorized.guard';

@NgModule({
    declarations: [
        // Layout
        MenuComponent,

        // Pages
        ErrorPageComponent,
        PagesComponent,
    ],
    imports: [
        // Angular modules
        CommonModule,
        FormsModule,
        HttpClientModule,

        // Project modules
        SharedModule,

        // Routing module
        PagesRoutingModule,
    ],
    providers: [
        AuthorizedGuard,
        NotAuthorizedGuard,
    ]
})
export class PagesModule { }
