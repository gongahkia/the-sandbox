"use client"

import { useState, useEffect } from "react"
import WeatherForecast from "./WeatherForecast"
import EnvironmentalData from "./EnvironmentalData"
import WeatherMap from "./WeatherMap"
import { motion } from "framer-motion"

export default function WeatherDashboard() {
  const [weatherData, setWeatherData] = useState(null)
  const [isLoading, setIsLoading] = useState(true)
  const [error, setError] = useState(null)

  useEffect(() => {
    const fetchData = async () => {
      try {
        const response = await fetch("/api/weather")
        if (!response.ok) {
          throw new Error("Failed to fetch weather data")
        }
        const data = await response.json()

        const requiredKeys = [
          "forecast24hr",
          "forecast4day",
          "forecast2hr",
          "psi",
          "uv",
          "windspeed",
          "windDirection",
          "rainfall",
          "humidity",
          "airTemp",
          "pm25",
          "wetBulb",
          "lightning",
        ]
        const missingKeys = requiredKeys.filter((key) => !data[key])

        if (missingKeys.length > 0) {
          console.warn("Missing data for:", missingKeys.join(", "))
        }

        setWeatherData(data)
      } catch (err) {
        setError(err.message)
      } finally {
        setIsLoading(false)
      }
    }

    fetchData()
  }, [])

  if (isLoading) {
    return <div className="flex items-center justify-center h-screen">Loading...</div>
  }

  if (error) {
    return <div className="flex items-center justify-center h-screen text-red-500">Error: {error}</div>
  }

  return (
    <div className="container mx-auto px-4 py-8 mb-16">
      <motion.h1
        className="text-4xl font-bold mb-8 text-center text-gray-800 flex items-center justify-center"
        initial={{ opacity: 0, y: -50 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ duration: 0.5 }}
      >
        Rainman
        <motion.span
          className="ml-2 text-5xl"
          animate={{
            y: [0, -10, 0],
            rotate: [0, 10, 0],
          }}
          transition={{
            duration: 2,
            repeat: Number.POSITIVE_INFINITY,
            repeatType: "reverse",
          }}
        >
          🌧️
        </motion.span>
      </motion.h1>
      <motion.div
        className="grid grid-cols-1 md:grid-cols-2 gap-8"
        initial={{ opacity: 0 }}
        animate={{ opacity: 1 }}
        transition={{ duration: 0.5, delay: 0.2 }}
      >
        <WeatherForecast data={weatherData} />
        <EnvironmentalData data={weatherData} />
      </motion.div>
      <motion.div initial={{ opacity: 0 }} animate={{ opacity: 1 }} transition={{ duration: 0.5, delay: 0.4 }}>
        <WeatherMap data={weatherData} />
      </motion.div>
    </div>
  )
}