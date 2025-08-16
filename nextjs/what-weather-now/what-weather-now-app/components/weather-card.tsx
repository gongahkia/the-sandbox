"use client"

import { motion } from "framer-motion"
import type { CurrentWeather } from "@/lib/types"
import WeatherIcon from "./weather-icon"

interface WeatherCardProps {
  weatherData: CurrentWeather
  location: string
}

export default function WeatherCard({ weatherData, location }: WeatherCardProps) {
  const getTimeString = () => {
    const now = new Date()
    return now.toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" })
  }

  const getDateString = () => {
    const now = new Date()
    return now.toLocaleDateString([], { weekday: "long", month: "long", day: "numeric" })
  }

  return (
    <motion.div
      className="rounded-2xl overflow-hidden h-full"
      whileHover={{ scale: 1.02 }}
      transition={{ type: "spring", stiffness: 300 }}
    >
      <div className="relative h-full backdrop-blur-md bg-white/10 p-6 rounded-2xl overflow-hidden">
        <motion.div
          className="absolute inset-0 z-0 opacity-30"
          animate={{
            background: weatherData.isDay
              ? "linear-gradient(to bottom right, #64B5F6, #1E88E5)"
              : "linear-gradient(to bottom right, #303F9F, #1A237E)",
          }}
          transition={{ duration: 1.5 }}
        />

        <div className="relative z-10">
          <div className="flex justify-between items-start">
            <div>
              <h2 className="text-2xl font-medium text-white mb-1">{location}</h2>
              <p className="text-blue-100 opacity-80">{getDateString()}</p>
              <p className="text-blue-100 opacity-80">{getTimeString()}</p>
            </div>
            <motion.div
              initial={{ scale: 0.8, opacity: 0 }}
              animate={{ scale: 1, opacity: 1 }}
              transition={{ duration: 0.5, delay: 0.2 }}
              className="w-20 h-20"
            >
              <WeatherIcon condition={weatherData.condition} isDay={weatherData.isDay} />
            </motion.div>
          </div>

          <div className="mt-8">
            <div className="flex items-end">
              <motion.span
                initial={{ y: 20, opacity: 0 }}
                animate={{ y: 0, opacity: 1 }}
                transition={{ duration: 0.5, delay: 0.3 }}
                className="text-6xl font-light text-white"
              >
                {Math.round(weatherData.temperature)}°
              </motion.span>
              <span className="text-2xl text-blue-100 ml-1 mb-1">C</span>
            </div>
            <p className="text-blue-100 text-lg mt-2 capitalize">{weatherData.condition}</p>
          </div>

          <div className="mt-8 grid grid-cols-2 gap-4">
            <div className="bg-white/10 rounded-xl p-3">
              <p className="text-blue-100 opacity-70 text-sm">Feels Like</p>
              <p className="text-white text-lg">{Math.round(weatherData.feelsLike)}°C</p>
            </div>
            <div className="bg-white/10 rounded-xl p-3">
              <p className="text-blue-100 opacity-70 text-sm">Humidity</p>
              <p className="text-white text-lg">{weatherData.humidity}%</p>
            </div>
          </div>
        </div>
      </div>
    </motion.div>
  )
}