import { LayoutModule } from '../layout/layout.module';
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import { HttpClientModule } from '@angular/common/http';

import { PagesRoutingModule } from './pages-routing.module';
import { ComponentsModule } from '../../components/components.module';
import { PagesComponent } from './pages.component';
import { SharedModule } from '../../shared/shared.module';

import { ErrorPageComponent } from './error-page/error-page.component';

import { AuthorizedGuard } from '../../core/guards/authorized.guard';
import { NotAuthorizedGuard } from '../../core/guards/not-authorized.guard';

@NgModule({
    declarations: [
        PagesComponent,
        ErrorPageComponent,
    ],
    imports: [
        CommonModule,
        HttpClientModule,
        FormsModule,
        PagesRoutingModule,
        SharedModule,
        LayoutModule,
        ComponentsModule,
    ],
    providers: [
        AuthorizedGuard,
        NotAuthorizedGuard,
    ]
})
export class PagesModule { }
