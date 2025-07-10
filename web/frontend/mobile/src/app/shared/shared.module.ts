import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { BurgerDirective } from './directives/burger.directive';

import { ConsoleLogPipe } from './pipes/console-log.pipe';
import { FirstLetterPipe } from './pipes/first-letter.pipe';
import { JoinPipe } from './pipes/join.pipe';
import { PrettyDurationPipe } from './pipes/pretty-duration.pipe';
import { PrettySizePipe } from './pipes/pretty-size.pipe';
import { SafePipe } from './pipes/safe.pipe';
import { TimeElapsedPipe } from './pipes/time-elapsed.pipe';

@NgModule({
    imports: [CommonModule],
    declarations: [
        // Directives
        BurgerDirective,

        // Pipes
        ConsoleLogPipe,
        FirstLetterPipe,
        JoinPipe,
        PrettyDurationPipe,
        PrettySizePipe,
        SafePipe,
        TimeElapsedPipe,
    ],
    exports: [
        // Directives
        BurgerDirective,

        // Pipes
        ConsoleLogPipe,
        FirstLetterPipe,
        JoinPipe,
        PrettyDurationPipe,
        PrettySizePipe,
        SafePipe,
        TimeElapsedPipe,
    ],
    providers: []
})
export class SharedModule { }
