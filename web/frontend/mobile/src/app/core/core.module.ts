import { NgModule, Optional, SkipSelf  } from '@angular/core';
import { CommonModule } from '@angular/common';

// Helios services
import { AccountService } from './services/account.service';
import { ClipboardService } from './services/clipboard.service';
import { FilterService } from './services/filter.service';
import { NotificationService } from './services/notification.service';
import { SplashScreenService } from './services/splash-screen.service';
import { WindowRefService } from './services/window-ref.service';

// Helios guards
import { AuthorizedGuard } from './guards/authorized.guard';
import { NotAuthorizedGuard } from './guards/not-authorized.guard';

@NgModule({
    imports: [CommonModule],
    declarations: [],
    exports: [],
    providers: [
        // Helios services
        AccountService,
        ClipboardService,
        FilterService,
        NotificationService,
        SplashScreenService,
        WindowRefService,

        // Helios guards
        AuthorizedGuard,
        NotAuthorizedGuard,
    ]
})
export class CoreModule {
    constructor (@Optional() @SkipSelf() parentModule: CoreModule) {
        if (parentModule) {
            throw new Error('CoreModule is already loaded. Import it in the AppModule only');
        }
    }
}
