import { NextResponse } from "next/server"

const API_ENDPOINTS = [
  "https://api.data.gov.sg/v1/environment/24-hour-weather-forecast",
  "https://api.data.gov.sg/v1/environment/4-day-weather-forecast",
  "https://api.data.gov.sg/v1/environment/2-hour-weather-forecast",
  "https://api.data.gov.sg/v1/environment/psi",
  "https://api.data.gov.sg/v1/environment/uv-index",
  "https://api.data.gov.sg/v1/environment/wind-speed",
  "https://api.data.gov.sg/v1/environment/wind-direction",
  "https://api.data.gov.sg/v1/environment/rainfall",
  "https://api.data.gov.sg/v1/environment/relative-humidity",
  "https://api.data.gov.sg/v1/environment/air-temperature",
  "https://api.data.gov.sg/v1/environment/pm25",
  "https://api.data.gov.sg/v1/environment/wet-bulb-temperature",
  "https://api.data.gov.sg/v1/environment/lightning",
]

export async function GET() {
  try {
    const responses = await Promise.all(API_ENDPOINTS.map((url) => fetch(url)))
    const data = await Promise.all(responses.map((res) => res.json()))

    const [
      forecast24hr,
      forecast4day,
      forecast2hr,
      psi,
      uv,
      windspeed,
      windDirection,
      rainfall,
      humidity,
      airTemp,
      pm25,
      wetBulb,
      lightning,
    ] = data

    return NextResponse.json({
      forecast24hr,
      forecast4day,
      forecast2hr,
      psi,
      uv,
      windspeed,
      windDirection,
      rainfall,
      humidity,
      airTemp,
      pm25,
      wetBulb,
      lightning,
    })
  } catch (error) {
    console.error("Error fetching weather data:", error)
    return NextResponse.json({ error: "Failed to fetch weather data" }, { status: 500 })
  }
}