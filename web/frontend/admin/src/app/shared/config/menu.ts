import { ConfigModel } from '../interfaces/config';

export class MenuConfig implements ConfigModel {
    public config: any = {};

    constructor() {
        this.config = {
            header: {
                self: {},
            },
            aside: {
                self: {},
                items: [
                    {
                        title: 'Dashboard',
                        desc: 'Some description goes here',
                        root: true,
                        icon: 'dashboard',
                        page: '/dashboard',
                        // badge: { type: 'm-badge--success', value: '2' }
                    },
                    { section: 'Projects', for: 'game_manager' },
                    {
                        title: 'Games',
                        root: true,
                        icon: 'games',
                        page: '/games',
                        for: 'game_manager',
                    },
                    {
                        title: 'Categories',
                        root: true,
                        icon: 'style',
                        page: '/categories',
                        for: 'game_manager',
                    },
                    { section: 'Clients', for: 'superadmin' },
                    {
                        title: 'Accounts',
                        root: true,
                        icon: 'people_outline',
                        page: '/clients/accounts',
                        for: 'superadmin',
                        breadcrumbs: false,
                    },
                    { section: 'Personnel', for: 'superadmin' },
                    {
                        title: 'Accounts',
                        root: true,
                        icon: 'people',
                        page: '/personnel/accounts',
                        for: 'superadmin',
                        breadcrumbs: false,
                    },
                    {
                        title: 'Groups',
                        root: true,
                        icon: 'group_work',
                        page: '/personnel/groups',
                        for: 'superadmin',
                        breadcrumbs: false,
                    },
                    { section: 'Admin', for: 'superadmin' },
                    {
                        title: 'Settings',
                        root: true,
                        icon: 'settings',
                        page: '/settings',
                        for: 'superadmin',
                    },
                ]
            }
        };
    }
}
