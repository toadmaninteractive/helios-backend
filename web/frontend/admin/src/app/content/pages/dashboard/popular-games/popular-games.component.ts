import { ChangeDetectionStrategy, ChangeDetectorRef, Component, OnDestroy, OnInit } from '@angular/core';
import { BehaviorSubject, Subject } from 'rxjs';
import { finalize, takeUntil } from 'rxjs/operators';
import { HeliosAdminService } from '../../../../protocol/web-admin-protocol.service';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

@Component({
    selector: 'm-popular-games',
    templateUrl: './popular-games.component.html',
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PopularGamesComponent implements OnInit, OnDestroy {
    public config: any;
    destroy$: Subject<any>;
    loading$: BehaviorSubject<boolean>;
    games$: BehaviorSubject<WebProtocol.PopularGame[]>;
    barChartOptions: any = {
        scaleShowVerticalLines: true,
        responsive: true,
        legend: { display: true, position: 'bottom' },
        scales: {
            yAxes: [
                { ticks: { beginAtZero: true } }
            ]
        }
    };
    barChartLabels: string[] = ['Purchases'];
    barChartType = 'bar'; // bar | horizontalBar
    barChartColors = [
        { backgroundColor: ['rgba(130, 190, 80, .75)'], borderColor: ['#82be50'] },
        { backgroundColor: ['rgba(70, 90, 155, .75)'], borderColor: ['#465a9b'] },
        { backgroundColor: ['rgba(220, 45, 95, .75)'], borderColor: ['#dc2d5f'] },
        { backgroundColor: ['rgba(50, 135, 210, .75)'], borderColor: ['#3287d2'] },
        { backgroundColor: ['rgba(240, 140, 30, .75)'], borderColor: ['#f08c1e'] },
        { backgroundColor: ['rgba(65, 150, 135, .75)'], borderColor: ['#419687'] },
        { backgroundColor: ['rgba(160, 50, 130, .75)'], borderColor: ['#a03282'] },
        { backgroundColor: ['rgba(235, 100, 45, .75)'], borderColor: ['#eb642d'] },
        { backgroundColor: ['rgba(15, 85, 130, .75)'], borderColor: ['#0f5582'] },
        { backgroundColor: ['rgba(110, 50, 120, .75)'], borderColor: ['#6e3278'] },
    ];
    barChartData: any[] = [];

    constructor(
        private cdr: ChangeDetectorRef,
        private heliosAdminService: HeliosAdminService,
    ) { }

    ngOnInit(): void {
        this.destroy$ = new Subject();
        this.loading$ = new BehaviorSubject<boolean>(false);
        this.games$ = new BehaviorSubject<WebProtocol.PopularGame[]>([]);
        this.getPopularGames();
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
        this.loading$.complete();
        this.games$.complete();
    }

    getPopularGames(): void {
        this.loading$.next(true);

        this.heliosAdminService
            .getPopularGames(WebProtocol.PopularGameOrderBy.Purchases, WebProtocol.OrderDirection.Desc, 0, 10)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe((response: WebProtocol.CollectionSlice<WebProtocol.PopularGame>) => {
                this.games$.next(response.items);
                this.barChartData = response.items.map((game: WebProtocol.PopularGame) => <Object>{ data: [game.purchases], label: game.title });
                this.cdr.detectChanges();
            });
    }
}
