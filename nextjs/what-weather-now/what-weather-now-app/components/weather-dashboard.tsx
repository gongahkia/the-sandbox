"use client"

import { useState, useEffect } from "react"
import { motion } from "framer-motion"
import WeatherCard from "./weather-card"
import ForecastList from "./forecast-list"
import WeatherDetails from "./weather-details"
import LocationSearch from "./location-search"
import { fetchWeatherData } from "@/lib/api"
import type { WeatherData } from "@/lib/types"
import LoadingState from "./loading-state"

export default function WeatherDashboard() {
  const [location, setLocation] = useState("New York")
  const [weatherData, setWeatherData] = useState<WeatherData | null>(null)
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)

  useEffect(() => {
    const getWeatherData = async () => {
      try {
        setLoading(true)
        setError(null)
        const data = await fetchWeatherData(location)
        setWeatherData(data)
      } catch (err) {
        setError("Failed to fetch weather data. Please try again.")
        console.error(err)
      } finally {
        setLoading(false)
      }
    }

    getWeatherData()
  }, [location])

  return (
    <div className="container mx-auto px-4 py-8 max-w-6xl">
      <motion.div
        initial={{ opacity: 0, y: -20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ duration: 0.5 }}
        className="mb-8"
      >
        <h1 className="text-3xl font-light text-white mb-6">
          <span className="mr-2">🌦️</span>
          What Weather Now
        </h1>
        <LocationSearch onLocationChange={setLocation} currentLocation={location} />
      </motion.div>

      {loading ? (
        <LoadingState />
      ) : error ? (
        <div className="text-red-400 p-4 rounded-lg bg-red-900/20 backdrop-blur-sm">{error}</div>
      ) : weatherData ? (
        <motion.div
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ duration: 0.5, delay: 0.2 }}
          className="grid grid-cols-1 lg:grid-cols-3 gap-6"
        >
          <div className="lg:col-span-1">
            <WeatherCard weatherData={weatherData.current} location={location} />
          </div>
          <div className="lg:col-span-2">
            <div className="grid gap-6">
              <WeatherDetails weatherData={weatherData.current} />
              <ForecastList forecast={weatherData.forecast} />
            </div>
          </div>
        </motion.div>
      ) : null}
    </div>
  )
}