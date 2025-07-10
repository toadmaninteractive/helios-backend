import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatButtonModule, MatCheckboxModule, MatInputModule, MatPaginatorModule, MatSortModule, MatTableModule, MatTooltipModule } from '@angular/material';
import { GravatarModule } from '../../../components/general/gravatar/gravatar.module';
import { ClientsRoutingModule } from './clients-routing.module';
import { ClientAccountsComponent } from './client-accounts/client-accounts.component';

@NgModule({
    imports: [
        // Angular modules
        CommonModule,

        // Angular Material modules
        MatButtonModule,
        MatCheckboxModule,
        MatInputModule,
        MatPaginatorModule,
        MatSortModule,
        MatTableModule,
        MatTooltipModule,

        // Project modules
        GravatarModule,

        // Child routing module
        ClientsRoutingModule,
    ],
    providers: [],
    declarations: [ClientAccountsComponent]
})
export class ClientsModule { }
