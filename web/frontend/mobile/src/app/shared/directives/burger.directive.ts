import { Directive, Input, AfterContentInit, ElementRef, OnDestroy } from '@angular/core';

@Directive({
    selector: '[mBurger]'
})
export class BurgerDirective implements AfterContentInit, OnDestroy {
    @Input('mBurger') menu: Element;
    private listener: EventListener;

    constructor (private el: ElementRef) { }

    ngAfterContentInit(): void {
        this.listener = this.el.nativeElement.addEventListener('click', () => {
            this.el.nativeElement.classList.toggle('is-active');
            this.menu.classList.toggle('is-active');
        });
    }

    ngOnDestroy(): void {
        this.el.nativeElement.removeEventListener('click', this.listener);
    }
}
