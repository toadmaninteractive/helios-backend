import { AfterViewInit, ChangeDetectionStrategy, Component, ElementRef, HostBinding, Inject, OnInit } from '@angular/core';
import { DOCUMENT } from '@angular/common';
import { filter } from 'rxjs/operators';
import { NavigationEnd, Router } from '@angular/router';
import { MenuAsideOffcanvasDirective } from '../../../shared/directives/menu-aside-offcanvas.directive';
import { ClassInitService } from '../../../core/services/metronic/class-init.service';
import { LayoutConfigService } from '../../../core/services/metronic/layout-config.service';
import { LayoutRefService } from '../../../core/services/metronic/layout/layout-ref.service';
import { MenuAsideService } from '../../../core/services/metronic/layout/menu-aside.service';
import { AccountService } from '../../../core/services/account.service';
import * as WebProtocol from '../../../protocol/web-protocol.data';

@Component({
    selector: 'm-aside-left',
    templateUrl: './aside-left.component.html',
    changeDetection: ChangeDetectionStrategy.OnPush
})
export class AsideLeftComponent implements OnInit, AfterViewInit {
    @HostBinding('class') classes = 'm-grid__item m-aside-left';
    @HostBinding('id') id = 'm_aside_left';
    @HostBinding('attr.mMenuAsideOffcanvas') mMenuAsideOffcanvas: MenuAsideOffcanvasDirective;
    currentRouteUrl = '';
    insideTm: any;
    outsideTm: any;

    constructor (
        private el: ElementRef,
        private router: Router,
        public classInitService: ClassInitService,
        public menuAsideService: MenuAsideService,
        public layoutConfigService: LayoutConfigService,
        private layoutRefService: LayoutRefService,
        public accountService: AccountService,
        @Inject(DOCUMENT) private document: Document
    ) {
        // subscribe to menu classes update
        this.classInitService.onClassesUpdated$.subscribe(classes => {
            // join the classes array and pass to variable
            this.classes = 'm-grid__item m-aside-left ' + classes.aside_left.join(' ');
        });
    }

    ngAfterViewInit(): void {
        setTimeout(() => {
            this.mMenuAsideOffcanvas = new MenuAsideOffcanvasDirective(this.el);
            // manually call the directives' lifecycle hook method
            this.mMenuAsideOffcanvas.ngAfterViewInit();

            // keep aside left element reference
            this.layoutRefService.addElement('asideLeft', this.el.nativeElement);
        });
    }

    ngOnInit() {
        this.currentRouteUrl = this.router.url.split(/[?#]/)[0];

        this.router.events
            .pipe(filter(event => event instanceof NavigationEnd))
            .subscribe(event => this.currentRouteUrl = this.router.url.split(/[?#]/)[0]);
    }

    isMenuItemIsActive(item): boolean {
        if (item.submenu) {
            return this.isMenuRootItemIsActive(item);
        }

        if (!item.page) {
            return false;
        }

        // dashboard
        if (item.page !== '/' && this.currentRouteUrl.startsWith(item.page)) {
            return true;
        }
        return this.currentRouteUrl === item.page;
    }

    isMenuRootItemIsActive(item): boolean {
        let result: boolean = false;

        for (const subItem of item.submenu) {
            result = this.isMenuItemIsActive(subItem);
            if (result) {
                return true;
            }
        }

        return false;
    }

    /**
     * Use for fixed left aside menu, to show menu on mouseenter event.
     * @param e Event
     */
    mouseEnter(e: Event) {
        // check if the left aside menu is fixed
        if (this.document.body.classList.contains('m-aside-left--fixed')) {
            if (this.outsideTm) {
                clearTimeout(this.outsideTm);
                this.outsideTm = null;
            }

            this.insideTm = setTimeout(() => {
                // if the left aside menu is minimized
                if (this.document.body.classList.contains('m-aside-left--minimize') && mUtil.isInResponsiveRange('desktop')) {
                    // show the left aside menu
                    this.document.body.classList.remove('m-aside-left--minimize');
                    this.document.body.classList.add('m-aside-left--minimize-hover');
                }
            }, 300);
        }
    }

    /**
     * Use for fixed left aside menu, to show menu on mouseenter event.
     * @param e Event
     */
    mouseLeave(e: Event) {
        if (this.document.body.classList.contains('m-aside-left--fixed')) {
            if (this.insideTm) {
                clearTimeout(this.insideTm);
                this.insideTm = null;
            }

            this.outsideTm = setTimeout(() => {
                // if the left aside menu is expand
                if (this.document.body.classList.contains('m-aside-left--minimize-hover') && mUtil.isInResponsiveRange('desktop')) {
                    // hide back the left aside menu
                    this.document.body.classList.remove('m-aside-left--minimize-hover');
                    this.document.body.classList.add('m-aside-left--minimize');
                }
            }, 500);
        }
    }

    matchesFor(expectedRole: string | null, profile: WebProtocol.PersonnelAccountProfile): boolean {
        switch (expectedRole) {
            case 'game_manager': return profile && (profile.isGameManager || profile.isSuperadmin);
            case 'superadmin': return profile && profile.isSuperadmin;
            default: return true;
        }
    }
}
