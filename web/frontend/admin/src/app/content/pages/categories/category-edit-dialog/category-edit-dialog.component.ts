import { Component, Inject, OnInit, OnDestroy } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material';
import { BehaviorSubject, Subject } from 'rxjs';
import { finalize, takeUntil } from 'rxjs/operators';
import { CategoryEditDialogData } from './category-edit-dialog.interface';
import { HeliosAdminService } from '../../../../protocol/web-admin-protocol.service';
import * as WebProtocol from '../../../../protocol/web-protocol.data';

@Component({
    selector: 'm-category-edit-dialog',
    templateUrl: 'category-edit-dialog.component.html',
})
export class CategoryEditDialogComponent implements OnInit, OnDestroy {
    destroy$: Subject<any>;
    loading$: BehaviorSubject<boolean>;
    category: WebProtocol.GameCategory;
    pristineCategory: WebProtocol.GameCategory;
    isNew: boolean;
    errorMessage?: string = null;

    constructor(
        public dialogRef: MatDialogRef<CategoryEditDialogComponent>,
        @Inject(MAT_DIALOG_DATA) public data: CategoryEditDialogData,
        private heliosAdminService: HeliosAdminService,
    ) {
        this.isNew = !data.category;
        this.category = this.isNew ? this.initializeGameCategory() : WebProtocol.GameCategory.fromJson(data.category.toJson());
        this.pristineCategory = this.isNew ? null : WebProtocol.GameCategory.fromJson(data.category.toJson());
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

    private initializeGameCategory(): WebProtocol.GameCategory {
        const category = new WebProtocol.GameCategory();
        category.name = '';
        category.description = '';
        category.sortOrder = 1;

        return category;
    }

    isGameCategoryValid(): boolean {
        return this.category.name.trim().length > 0;
    }

    gameCategoryChanged(): boolean {
        return this.category.name.trim() !== this.pristineCategory.name.trim()
            || this.category.description.trim() !== this.pristineCategory.description.trim()
            || this.category.sortOrder !== this.pristineCategory.sortOrder;
    }

    canSubmit(): boolean {
        return this.isGameCategoryValid() && (this.isNew ? true : this.gameCategoryChanged());
    }

    createGameCategory(): void {
        const body = new WebProtocol.GameCategoryCreateRequest();
        body.name = this.category.name.trim();
        body.description = this.category.description.trim();
        body.sortOrder = +this.category.sortOrder;
        this.loading$.next(true);

        this.heliosAdminService
            .createGameCategory(body)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe((response: WebProtocol.GameCategoryManageResponse) => {
                if (response.result) {
                    this.dialogRef.close(response.category);
                } else {
                    switch (response.error) {
                        case WebProtocol.GameCategoryManageError.Failure:
                            this.errorMessage = 'Server failed to process request';
                            break;
                        case WebProtocol.GameCategoryManageError.AlreadyExists:
                            this.errorMessage = 'Game category name already exists';
                            break;
                        case WebProtocol.GameCategoryManageError.InvalidName:
                            this.errorMessage = 'Invalid game category name';
                            break;
                    }
                }
            });
    }

    updateGameCategory(): void {
        const body = new WebProtocol.GameCategoryUpdateRequest();
        if (this.category.name.trim() !== this.pristineCategory.name.trim()) body.name = this.category.name.trim();
        if (this.category.description.trim() !== this.pristineCategory.description.trim()) body.description = this.category.description.trim();
        if (this.category.sortOrder !== this.pristineCategory.sortOrder) body.sortOrder = this.category.sortOrder;
        this.loading$.next(true);

        this.heliosAdminService
            .updateGameCategory(body, this.category.id, this.category.rev)
            .pipe(
                takeUntil(this.destroy$),
                finalize(() => this.loading$.next(false)),
            )
            .subscribe((response: WebProtocol.GameCategoryManageResponse) => {
                if (response.result) {
                    this.dialogRef.close(response.category);
                } else {
                    switch (response.error) {
                        case WebProtocol.GameCategoryManageError.Failure:
                            this.errorMessage = 'Server failed to process request';
                            break;
                        case WebProtocol.GameCategoryManageError.AlreadyExists:
                            this.errorMessage = 'Game category name already exists';
                            break;
                        case WebProtocol.GameCategoryManageError.InvalidName:
                            this.errorMessage = 'Invalid game category name';
                            break;
                    }
                }
            });
    }
}
