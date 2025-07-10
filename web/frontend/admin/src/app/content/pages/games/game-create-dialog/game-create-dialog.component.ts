import { Component, Inject, OnInit, OnDestroy } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material';
import { BehaviorSubject, Subject } from 'rxjs';
import { finalize, takeUntil } from 'rxjs/operators';
import { HeliosAdminService } from '../../../../protocol/web-admin-protocol.service';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

@Component({
    selector: 'm-game-create-dialog',
    templateUrl: 'game-create-dialog.component.html',
})
export class GameCreateDialogComponent implements OnInit, OnDestroy {
    destroy$: Subject<any>;
    loading$: BehaviorSubject<boolean>;
    newGame: WebProtocol.Game;
    errorMessage?: string = null;

    constructor(
        public dialogRef: MatDialogRef<GameCreateDialogComponent>,
        @Inject(MAT_DIALOG_DATA) public data: any,
        private heliosAdminService: HeliosAdminService,
    ) {
        this.initializeGame();
    }

    ngOnInit(): void {
        this.destroy$ = new Subject();
        this.loading$ = new BehaviorSubject<boolean>(false);
    }

    ngOnDestroy(): void {
        this.destroy$.next();
        this.destroy$.complete();
        this.loading$.complete();
    }

    private initializeGame(): void {
        this.newGame = new WebProtocol.Game();
        this.newGame.id = '';
        this.newGame.title = '';
        this.newGame.price = 0;
        this.newGame.currency = 'EUR';
    }

    isGameValid(): boolean {
        return /^[a-z]+[a-z0-9]*$/.test(this.newGame.id)
            && this.newGame.title.trim().length > 0
            && +this.newGame.price > 0
            && this.newGame.currency === 'EUR';
    }

    createGame(): void {
        const body = new WebProtocol.GameCreateRequest();
        body.id = this.newGame.id;
        body.title = this.newGame.title;
        body.price = +this.newGame.price;
        body.currency = this.newGame.currency;
        this.loading$.next(true);

        this.heliosAdminService
            .createGame(body)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe((response: WebProtocol.GameCreateResponse) => {
                if (response.result) {
                    this.dialogRef.close(response.game);
                } else {
                    switch (response.error) {
                        case WebProtocol.GameCreateError.Failure:
                            this.errorMessage = 'Server failed to process request';
                            break;
                        case WebProtocol.GameCreateError.GameIdAlreadyExists:
                            this.errorMessage = 'Game ID already exists';
                            break;
                        case WebProtocol.GameCreateError.GameTitleAlreadyExists:
                            this.errorMessage = 'Game title already exists';
                            break;
                        case WebProtocol.GameCreateError.InvalidPrice:
                            this.errorMessage = 'Invalid price';
                            break;
                        case WebProtocol.GameCreateError.InvalidCurrency:
                            this.errorMessage = 'Invalid currency';
                            break;
                    }
                }
            });
    }
}
