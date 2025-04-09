import type { WeatherData } from "./types"

// This is a mock implementation of the GribStream API
// In a real application, you would replace this with actual API calls
export async function fetchWeatherData(location: string): Promise<WeatherData> {
  // Simulate API call delay
  await new Promise((resolve) => setTimeout(resolve, 1500))

  // Generate mock data based on location
  return generateMockWeatherData(location)
}

function generateMockWeatherData(location: string): WeatherData {
  const isDay = new Date().getHours() > 6 && new Date().getHours() < 20
  const randomTemp = Math.floor(Math.random() * 30) + 5 // 5-35°C

  // Generate different weather conditions based on location name's first letter
  const firstChar = location.charAt(0).toLowerCase()
  let condition = "Clear"

  if (["a", "b", "c"].includes(firstChar)) {
    condition = "Sunny"
  } else if (["d", "e", "f"].includes(firstChar)) {
    condition = "Partly Cloudy"
  } else if (["g", "h", "i"].includes(firstChar)) {
    condition = "Cloudy"
  } else if (["j", "k", "l"].includes(firstChar)) {
    condition = "Light Rain"
  } else if (["m", "n", "o"].includes(firstChar)) {
    condition = "Heavy Rain"
  } else if (["p", "q", "r"].includes(firstChar)) {
    condition = "Thunderstorm"
  } else if (["s", "t"].includes(firstChar)) {
    condition = "Snow"
  } else if (["u", "v", "w"].includes(firstChar)) {
    condition = "Foggy"
  } else {
    condition = "Clear"
  }

  // Generate forecast for next 7 days
  const forecast = Array.from({ length: 7 }, (_, i) => {
    const date = new Date()
    date.setDate(date.getDate() + i + 1)

    return {
      date: date.toISOString().split("T")[0],
      condition: i % 2 === 0 ? condition : getRandomCondition(),
      maxTemp: randomTemp + Math.floor(Math.random() * 5),
      minTemp: randomTemp - Math.floor(Math.random() * 8),
      chanceOfRain: Math.floor(Math.random() * 100),
    }
  })

  return {
    location: {
      name: location,
      country: "Sample Country",
      lat: 0,
      lon: 0,
    },
    current: {
      temperature: randomTemp,
      feelsLike: randomTemp - 2,
      humidity: Math.floor(Math.random() * 50) + 30,
      windSpeed: Math.floor(Math.random() * 30) + 5,
      windDirection: "NE",
      pressure: Math.floor(Math.random() * 50) + 1000,
      uvIndex: Math.floor(Math.random() * 10) + 1,
      visibility: Math.floor(Math.random() * 10) + 5,
      condition,
      isDay,
    },
    forecast,
  }
}

function getRandomCondition(): string {
  const conditions = [
    "Sunny",
    "Clear",
    "Partly Cloudy",
    "Cloudy",
    "Light Rain",
    "Heavy Rain",
    "Thunderstorm",
    "Snow",
    "Foggy",
    "Drizzle",
  ]

  return conditions[Math.floor(Math.random() * conditions.length)]
}