import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { MatProgressSpinnerModule, MatProgressBarModule } from '@angular/material';
import { SharedModule } from '../../../shared/shared.module';
import { PortletComponent } from './portlet.component';

@NgModule({
    imports: [
        CommonModule,
        SharedModule,
        MatProgressSpinnerModule,
        MatProgressBarModule
    ],
    declarations: [PortletComponent],
    exports: [PortletComponent]
})
export class PortletModule {}
