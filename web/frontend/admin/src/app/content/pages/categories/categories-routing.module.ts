import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { CategoriesOverviewComponent } from './categories-overview/categories-overview.component';

const routes: Routes = [
    {
        path: '',
        children: [
            {
                path: '',
                component: CategoriesOverviewComponent,
            },
        ]
    }
];

@NgModule({
    imports: [RouterModule.forChild(routes)],
    exports: [RouterModule]
})
export class CategoriesRoutingModule { }
