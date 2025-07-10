import { ChangeDetectionStrategy, Component, OnDestroy, OnInit } from '@angular/core';
import { BehaviorSubject, Subject } from 'rxjs';
import { finalize, map, takeUntil } from 'rxjs/operators';
import { HeliosAdminService } from '../../../../protocol/web-admin-protocol.service';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

@Component({
    selector: 'm-favourite-games',
    templateUrl: './favourite-games.component.html',
    styleUrls: ['./favourite-games.component.scss'],
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class FavouriteGamesComponent implements OnInit, OnDestroy {
    public config: any;
    destroy$ = new Subject();
    loading$ = new BehaviorSubject<boolean>(false);
    games$ = new BehaviorSubject<WebProtocol.Game[]>(null);

    constructor(private heliosAdminService: HeliosAdminService) { }

    ngOnInit(): void {
        this.getFavouriteGames();
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
        this.loading$.complete();
        this.games$.complete();
    }

    getFavouriteGames(): void {
        this.loading$.next(true);

        this.heliosAdminService
            .getFavouriteGames()
            .pipe(
                takeUntil(this.destroy$),
                map(response => response.items),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe(games => this.games$.next(games));
    }
}
