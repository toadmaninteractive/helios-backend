import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnInit, OnDestroy } from '@angular/core';
import { BehaviorSubject, Subject, Subscription } from 'rxjs';
import { takeUntil, tap } from 'rxjs/operators';
import { MobilePlatform, WindowRefService } from '../../../../core/services/window-ref.service';
import { HeliosMobileService } from '../../../../protocol/web-mobile-protocol.service';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

@Component({
    selector: 'm-games-overview',
    templateUrl: './games-overview.component.html',
    styleUrls: ['./games-overview.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class GamesOverviewComponent implements OnInit, OnDestroy {
    destroy$: Subject<any>;
    loading$ = new BehaviorSubject(false);
    error$ = new Subject<string>();
    games$ = new BehaviorSubject<WebProtocol.GameItem[]>([]);
    mobilePlatform$ = new BehaviorSubject<MobilePlatform>(MobilePlatform.Unknown);
    subBuilds: Subscription;

    constructor (
        private cdr: ChangeDetectorRef,
        private windowRefService: WindowRefService,
        private heliosMobileService: HeliosMobileService,
    ) {
        this.mobilePlatform$.next(this.windowRefService.getMobilePlatform());
    }

    ngOnInit(): void {
        this.destroy$ = new Subject();

        if (this.isSupportedPlatform(this.mobilePlatform$.getValue())) {
            this.getMobileGames();
        }
    }

    ngOnDestroy(): void {
        if (this.subBuilds instanceof Subscription) {
            this.subBuilds.unsubscribe();
        }

        this.destroy$.next();
        this.destroy$.complete();
    }

    platformName(platform: MobilePlatform): string {
        switch (platform) {
            case MobilePlatform.Unknown: return 'Unknown';
            case MobilePlatform.WindowsPhone: return 'Windows Phone';
            case MobilePlatform.Android: return 'Android';
            case MobilePlatform.Ios: return 'iOS';
            default: return 'Not detected';
        }
    }

    isSupportedPlatform(platform: MobilePlatform): boolean {
        return platform === MobilePlatform.Android || platform === MobilePlatform.Ios;
    }

    getMobileGames(): void {
        if (this.subBuilds instanceof Subscription) {
            this.subBuilds.unsubscribe();
        }

        const platform = this.mobilePlatform$.getValue() === MobilePlatform.Android ? WebProtocol.Platform.Android : WebProtocol.Platform.Ios;
        this.error$.next(null);
        this.loading$.next(true);

        this.subBuilds = this.heliosMobileService
            .getMobileGames(platform)
            .pipe(
                takeUntil(this.destroy$),
                tap(response => this.loading$.next(false)),
            )
            .subscribe(
                (response: WebProtocol.GameItemList) => this.games$.next(response.games),
                error => this.error$.next('Failed to get games')
            );
    }
}
