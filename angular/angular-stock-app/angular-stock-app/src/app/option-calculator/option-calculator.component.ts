import { Component, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { interval, Subscription } from 'rxjs';
import { takeWhile } from 'rxjs/operators';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';
import { DatePipe } from '@angular/common';

interface StockData {
  '01. symbol'?: string;
  '05. price'?: string;
  '07. latest trading day'?: string;
}

interface OptionInputs {
  stockPrice: number;
  strikePrice: number;
  timeToExpiration: number;
  interestRate: number;
  volatility: number;
  optionType: 'call' | 'put';
}

@Component({
  selector: 'app-option-calculator',
  standalone: true,
  imports: [CommonModule, FormsModule, DatePipe],
  templateUrl: './option-calculator.component.html',
  styleUrls: ['./option-calculator.component.css']
})
export class OptionCalculatorComponent implements OnInit {
  inputMethod: 'manual' | 'ticker' = 'manual';
  ticker = '';
  submittedTicker = '';
  loadingProgress = 0;
  result: any = null;
  inputs: OptionInputs = {
    stockPrice: 0,
    strikePrice: 0,
    timeToExpiration: 0,
    interestRate: 0,
    volatility: 0,
    optionType: 'call'
  };
  data: StockData | null = null;
  error: any = null;
  isLoading = false;

  private loadingSubscription: Subscription | null = null;

  constructor(private http: HttpClient) {}

  ngOnInit() {}

  isValidTicker(ticker: string): boolean {
    return /^[A-Z]{1,5}$/.test(ticker);
  }

  handleSubmit() {
    this.submittedTicker = this.ticker;
    this.fetchStockData();
  }

  parseFloat(value: string): number {
    return parseFloat(value);
  }

  fetchStockData() {
    if (this.isValidTicker(this.submittedTicker)) {
      this.isLoading = true;
      this.loadingProgress = 0;
      this.startLoadingAnimation();

      this.http.get<StockData>(`/api/stock?ticker=${this.submittedTicker}`).subscribe(
        (data) => {
          this.data = data;
          this.isLoading = false;
          this.loadingProgress = 100;
          if (data['05. price']) {
            this.inputs.stockPrice = parseFloat(data['05. price']);
          }
        },
        (error) => {
          this.error = error;
          this.isLoading = false;
          this.loadingProgress = 100;
        }
      );
    }
  }

  startLoadingAnimation() {
    if (this.loadingSubscription) {
      this.loadingSubscription.unsubscribe();
    }
    this.loadingSubscription = interval(100)
      .pipe(takeWhile(() => this.loadingProgress < 90))
      .subscribe(() => {
        this.loadingProgress += 5;
      });
  }

  calculateOption() {
    const { stockPrice, strikePrice, timeToExpiration, interestRate, volatility, optionType } = this.inputs;
    
    const d1 = (Math.log(stockPrice / strikePrice) + (interestRate + volatility ** 2 / 2) * timeToExpiration) / (volatility * Math.sqrt(timeToExpiration));
    const d2 = d1 - volatility * Math.sqrt(timeToExpiration);
    
    const normalCDF = (x: number) => {
      const t = 1 / (1 + 0.2316419 * Math.abs(x));
      const d = 0.3989423 * Math.exp(-x * x / 2);
      const probability = d * t * (0.3193815 + t * (-0.3565638 + t * (1.781478 + t * (-1.821256 + t * 1.330274))));
      return x > 0 ? 1 - probability : probability;
    };

    let optionPrice: number;
    if (optionType === 'call') {
      optionPrice = stockPrice * normalCDF(d1) - strikePrice * Math.exp(-interestRate * timeToExpiration) * normalCDF(d2);
    } else {
      optionPrice = strikePrice * Math.exp(-interestRate * timeToExpiration) * normalCDF(-d2) - stockPrice * normalCDF(-d1);
    }

    this.result = {
      optionPrice,
      timestamp: new Date().getTime()
    };
  }
}