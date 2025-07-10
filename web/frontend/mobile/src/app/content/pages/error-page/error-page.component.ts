import { ChangeDetectionStrategy, Component } from '@angular/core';

@Component({
    selector: 'm-error-page',
    templateUrl: './error-page.component.html',
    styleUrls: ['./error-page.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ErrorPageComponent { }
