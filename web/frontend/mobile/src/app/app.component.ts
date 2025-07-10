import { Component, ChangeDetectionStrategy, ViewChild, ElementRef, AfterViewInit } from '@angular/core';
import { SplashScreenService } from './core/services/splash-screen.service';

@Component({
    // tslint:disable-next-line:component-selector
    selector: 'body[m-root]',
    templateUrl: './app.component.html',
    changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppComponent implements AfterViewInit {
    @ViewChild('splashScreen', { read: ElementRef, static: false }) splashScreen: ElementRef;

    constructor(private splashScreenService: SplashScreenService) { }

    ngAfterViewInit(): void {
        if (this.splashScreen) {
            this.splashScreenService.init(this.splashScreen.nativeElement);
        }
    }
}
