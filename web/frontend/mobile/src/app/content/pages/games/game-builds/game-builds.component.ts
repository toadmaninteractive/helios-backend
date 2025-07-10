import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnInit, OnDestroy } from '@angular/core';
import { ActivatedRoute, ParamMap, Router } from '@angular/router';
import { BehaviorSubject, Subject, Subscription } from 'rxjs';
import { takeUntil, tap } from 'rxjs/operators';
import { MobilePlatform, WindowRefService } from '../../../../core/services/window-ref.service';
import { HeliosMobileService } from '../../../../protocol/web-mobile-protocol.service';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

@Component({
    selector: 'm-game-builds',
    templateUrl: './game-builds.component.html',
    styleUrls: ['./game-builds.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class GameBuildsComponent implements OnInit, OnDestroy {
    destroy$: Subject<any>;
    gameId$ = new BehaviorSubject<string>(null);
    loading$ = new BehaviorSubject(false);
    error$ = new Subject<string>();
    mobilePlatform$ = new BehaviorSubject<MobilePlatform>(MobilePlatform.Unknown);
    builds$ = new BehaviorSubject<WebProtocol.Build[]>([]);
    subBuilds: Subscription;

    constructor (
        private activatedRoute: ActivatedRoute,
        private cdr: ChangeDetectorRef,
        private windowRefService: WindowRefService,
        private heliosMobileService: HeliosMobileService,
    ) {
        this.mobilePlatform$.next(this.windowRefService.getMobilePlatform());
    }

    ngOnInit(): void {
        this.destroy$ = new Subject();

        this.activatedRoute.paramMap
            .pipe(takeUntil(this.destroy$))
            .subscribe((params: ParamMap) => this.initialize(params.get('id')));
    }

    ngOnDestroy(): void {
        if (this.subBuilds instanceof Subscription) {
            this.subBuilds.unsubscribe();
        }

        this.destroy$.next();
        this.destroy$.complete();
    }

    private initialize(gameId: string): void {
        this.gameId$.next(gameId);

        if (this.isSupportedPlatform(this.mobilePlatform$.getValue())) {
            this.getMobileBuilds();
        }
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

    getMobileBuilds(): void {
        if (this.subBuilds instanceof Subscription) {
            this.subBuilds.unsubscribe();
        }

        const platform = this.mobilePlatform$.getValue() === MobilePlatform.Android ? WebProtocol.Platform.Android : WebProtocol.Platform.Ios;
        this.error$.next(null);
        this.loading$.next(true);

        this.subBuilds = this.heliosMobileService
            .getMobileBuilds(this.gameId$.getValue(), platform, WebProtocol.BuildOrderBy.CreatedAt, WebProtocol.OrderDirection.Desc, 0, 1000)
            .pipe(
                takeUntil(this.destroy$),
                tap(response => this.loading$.next(false)),
            )
            .subscribe(
                (response: WebProtocol.CollectionSlice<WebProtocol.Build>) => this.builds$.next(response.items),
                error => this.error$.next('Failed to get game builds')
            );
    }
}
