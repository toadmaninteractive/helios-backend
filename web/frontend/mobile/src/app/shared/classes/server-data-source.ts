import { CollectionViewer, DataSource } from '@angular/cdk/collections';
import { BehaviorSubject, Observable, of } from 'rxjs';
import { catchError, finalize } from 'rxjs/operators';

export class ServerDataSource<ItemType> implements DataSource<ItemType> {
    private dataSubject = new BehaviorSubject<ItemType[]>([]);
    private loadingSubject = new BehaviorSubject<boolean>(false);
    public loading$ = this.loadingSubject.asObservable();

    connect(collectionViewer: CollectionViewer): Observable<ItemType[]> {
        return this.dataSubject.asObservable();
    }

    disconnect(collectionViewer: CollectionViewer): void {
        this.dataSubject.complete();
        this.loadingSubject.complete();
    }

    loadItems(dataGetter: () => Observable<ItemType[]>): void {
        this.loadingSubject.next(true);

        dataGetter()
            .pipe(
                catchError(() => of([])),
                finalize(() => this.loadingSubject.next(false))
            )
            .subscribe(items => this.dataSubject.next(items));
    }
}
