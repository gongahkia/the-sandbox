import { Component } from '@angular/core';
import { RouterOutlet } from '@angular/router';
import { OptionCalculatorComponent } from './option-calculator/option-calculator.component';

@Component({
  selector: 'app-root',
  standalone: true,
  imports: [RouterOutlet, OptionCalculatorComponent],
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})
export class AppComponent {
  title = 'option-calculator';
}