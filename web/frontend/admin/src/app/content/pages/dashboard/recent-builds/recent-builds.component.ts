import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnDestroy, OnInit } from '@angular/core';
import { BehaviorSubject, Subject } from 'rxjs';
import { finalize, takeUntil } from 'rxjs/operators';
import { HeliosAdminService } from '../../../../protocol/web-admin-protocol.service';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

@Component({
    selector: 'm-recent-builds',
    templateUrl: './recent-builds.component.html',
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class RecentBuildsComponent implements OnInit, OnDestroy {
    public config: any;
    destroy$: Subject<any>;
    loading$: BehaviorSubject<boolean>;
    builds$: BehaviorSubject<WebProtocol.Build[]>;

    constructor(
        private cdr: ChangeDetectorRef,
        private heliosAdminService: HeliosAdminService,
    ) { }

    ngOnInit(): void {
        this.destroy$ = new Subject();
        this.loading$ = new BehaviorSubject<boolean>(false);
        this.builds$ = new BehaviorSubject<WebProtocol.Build[]>([]);
        this.getRecentBuilds();
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
        this.loading$.complete();
        this.builds$.complete();
    }

    getRecentBuilds(): void {
        this.loading$.next(true);

        this.heliosAdminService
            .getBuilds(WebProtocol.BuildOrderBy.CreatedAt, WebProtocol.OrderDirection.Desc, 0, 10, true)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe((response: WebProtocol.CollectionSlice<WebProtocol.Build>) => this.builds$.next(response.items));
    }

    tagBadgeClass(index: number): string {
        switch (index) {
            case 0: return 'm-badge--info';
            case 1: return 'm-badge--success';
            case 2: return 'm-badge--warning';
            case 3: return 'm-badge--danger';
            default: return 'm-badge--disabled';
        }
    }

    platform(platform: WebProtocol.Platform): string {
        return WebProtocol.Platform.getDescription(platform);
    }
}
