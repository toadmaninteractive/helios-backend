import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { ClientAccountsComponent } from './client-accounts/client-accounts.component';

const routes: Routes = [
    {
        path: '',
        children: [
            {
                path: 'accounts',
                component: ClientAccountsComponent,
            },
            {
                path: '*',
                redirectTo: 'accounts',
            },
        ]
    }
];

@NgModule({
    imports: [RouterModule.forChild(routes)],
    exports: [RouterModule]
})
export class ClientsRoutingModule { }
