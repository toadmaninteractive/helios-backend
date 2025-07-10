import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';
import {
    MatButtonModule,
    MatCardModule,
    MatCheckboxModule,
    MatFormFieldModule,
    MatIconModule,
    MatInputModule,
    MatRadioModule,
    MatSelectModule,
    MatTooltipModule,
} from '@angular/material';
import { SharedModule } from '../../../shared/shared.module';
import { SettingsRoutingModule } from './settings-routing.module';
import { SettingsComponent } from './settings.component';

@NgModule({
    imports: [
        // Angular modules
        CommonModule,
        FormsModule,

        // Angular Material modules
        MatButtonModule,
        MatCardModule,
        MatCheckboxModule,
        MatFormFieldModule,
        MatIconModule,
        MatInputModule,
        MatRadioModule,
        MatSelectModule,
        MatTooltipModule,

        // Project modules
        SharedModule,

        // Child routing module
        SettingsRoutingModule,
    ],
    providers: [],
    declarations: [SettingsComponent]
})
export class SettingsModule { }
