import { Component, ChangeDetectionStrategy, ChangeDetectorRef } from '@angular/core';
import { AccountService } from '../../../core/services/account.service';

@Component({
    selector: 'm-menu',
    templateUrl: './menu.component.html',
    styleUrls: ['./menu.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MenuComponent {
    constructor (
        private cdr: ChangeDetectorRef,
        public accountService: AccountService,
    ) { }

    signOut(): void {
        this.accountService.signOut().subscribe();
    }
}
