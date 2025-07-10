import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { ProfileAclComponent } from './profile-acl/profile-acl.component';
import { ProfileSettingsComponent } from './profile-settings/profile-settings.component';

const routes: Routes = [
    {
        path: 'acl',
        component: ProfileAclComponent,
    },
    {
        path: 'settings',
        component: ProfileSettingsComponent,
    }
];

@NgModule({
    imports: [RouterModule.forChild(routes)],
    exports: [RouterModule]
})
export class ProfileRoutingModule { }
