export interface WeatherData {
  location: Location
  current: CurrentWeather
  forecast: ForecastDay[]
}

export interface Location {
  name: string
  country: string
  lat: number
  lon: number
}

export interface CurrentWeather {
  temperature: number
  feelsLike: number
  humidity: number
  windSpeed: number
  windDirection: string
  pressure: number
  uvIndex: number
  visibility: number
  condition: string
  isDay: boolean
}

export interface ForecastDay {
  date: string
  condition: string
  maxTemp: number
  minTemp: number
  chanceOfRain: number
}