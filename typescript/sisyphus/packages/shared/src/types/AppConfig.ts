export interface AppConfig {
  window: {
    width: number;
    height: number;
  };
  theme: string;
  font: string;
  plugins: string[];
  [key: string]: unknown;
}