import { ConfigModel } from '../interfaces/config';

export class PagesConfig implements ConfigModel {
    public config: any = {};

    constructor() {
        this.config = {
            dashboard: {
                page: {
                    title: 'Dashboard',
                    desc: 'Dashboard description'
                }
            },
            games: {
                page: {
                    title: 'Games',
                    desc: 'Games description'
                }
            },
            categories: {
                page: {
                    title: 'Categories',
                    desc: 'Game categories'
                }
            },
            clients: {
                accounts: {
                    page: {
                        title: 'Client Accounts',
                        desc: 'Client account management',
                    }
                },
            },
            personnel: {
                accounts: {
                    page: {
                        title: 'Personnel Accounts',
                        desc: 'Personnel account management',
                    }
                },
                groups: {
                    page: {
                        title: 'Personnel Groups',
                        desc: 'Personnel group management',
                    }
                },
            },
            settings: {
                page: {
                    title: 'Settings',
                    desc: 'Helios settings'
                }
            },
            404: {
                page: { title: '404 Not Found', desc: '', subheader: false }
            }
        };
    }
}
