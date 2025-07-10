import { NgModule, Optional, SkipSelf } from '@angular/core';
import { CommonModule } from '@angular/common';

import { HeliosAdminService } from './web-admin-protocol.service';
import { HeliosAuthService } from './web-auth-protocol.service';
import { HeliosMobileService } from './web-mobile-protocol.service';

@NgModule({
    imports: [CommonModule],
    declarations: [],
    exports: [],
    providers: [
        HeliosAdminService,
        HeliosAuthService,
        HeliosMobileService,
    ]
})
export class ProtocolModule {
    constructor (@Optional() @SkipSelf() parentModule: ProtocolModule) {
        if (parentModule) {
            throw new Error('ProtocolModule is already loaded. Import it in the AppModule only');
        }
    }
}
