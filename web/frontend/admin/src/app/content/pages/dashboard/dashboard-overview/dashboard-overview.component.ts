import { ChangeDetectionStrategy, Component } from '@angular/core';

@Component({
    selector: 'm-dashboard-overview',
    templateUrl: './dashboard-overview.component.html',
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DashboardOverviewComponent { }
