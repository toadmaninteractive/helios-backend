import { Component, OnInit, Input, ViewChild, ElementRef, AfterViewInit, ChangeDetectionStrategy, ChangeDetectorRef } from '@angular/core';
import { Router, NavigationEnd, NavigationStart } from '@angular/router';
import { AnimationBuilder, AnimationPlayer, style, animate } from '@angular/animations';
import { AccountService } from '../../core/services/account.service';

@Component({
    selector: 'm-pages',
    templateUrl: './pages.component.html',
    changeDetection: ChangeDetectionStrategy.OnPush
})
export class PagesComponent implements OnInit, AfterViewInit {
    @Input() selfLayout: any = 'blank';
    @ViewChild('mContentWrapper', { static: false }) contentWrapper: ElementRef;
    @ViewChild('mContent', { static: false }) mContent: ElementRef;
    public player: AnimationPlayer;

    constructor (
        private el: ElementRef,
        private router: Router,
        private animationBuilder: AnimationBuilder,
        private cdr: ChangeDetectorRef,
        private accountService: AccountService,
    ) {
        // Update self layout
        this.accountService.isSignedIn()
            .subscribe(signedIn => {
                this.selfLayout = signedIn ? 'default' : 'blank';
                // this.cdr.detectChanges();
            });

        // Animate page load
        this.router.events.subscribe(event => {
            if (event instanceof NavigationStart) {
                if (this.contentWrapper) {
                    // Hide content
                    this.contentWrapper.nativeElement.style.display = 'none';
                }
            }

            if (event instanceof NavigationEnd) {
                if (this.contentWrapper) {
                    // Show content back
                    this.contentWrapper.nativeElement.style.display = '';

                    // Animate the content
                    this.animate(this.contentWrapper.nativeElement);
                }
            }
        });
    }

    ngOnInit(): void { }

    ngAfterViewInit(): void {
        setTimeout(() => {
            if (this.mContent) {
                // Keep content element in the service
                // this.layoutRefService.addElement('content', this.mContent.nativeElement);
            }
        });
    }

    // Animate page load
    animate(element) {
        this.player = this.animationBuilder
            .build([
                style({ opacity: 0, transform: 'translateY(15px)' }),
                animate('500ms ease', style({ opacity: 1, transform: 'translateY(0)' })),
                style({ transform: 'none' }),
            ])
            .create(element);

        this.player.play();
    }
}
