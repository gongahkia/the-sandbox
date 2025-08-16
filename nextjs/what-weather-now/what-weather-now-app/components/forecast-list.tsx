"use client"

import { motion } from "framer-motion"
import type { ForecastDay } from "@/lib/types"
import WeatherIcon from "./weather-icon"

interface ForecastListProps {
  forecast: ForecastDay[]
}

export default function ForecastList({ forecast }: ForecastListProps) {
  return (
    <div className="backdrop-blur-md bg-white/10 rounded-2xl p-6">
      <h3 className="text-xl font-medium text-white mb-4">7-Day Forecast</h3>
      <div className="space-y-4">
        {forecast.map((day, index) => (
          <motion.div
            key={day.date}
            initial={{ opacity: 0, x: -20 }}
            animate={{ opacity: 1, x: 0 }}
            transition={{ duration: 0.3, delay: index * 0.05 }}
            className="flex items-center justify-between p-3 rounded-xl hover:bg-white/5 transition-colors"
          >
            <div className="flex items-center">
              <div className="w-10 h-10 mr-4 flex items-center justify-center">
                <WeatherIcon condition={day.condition} isDay={true} size={36} />
              </div>
              <div>
                <p className="text-white">{formatDate(day.date)}</p>
                <p className="text-blue-100 opacity-70 text-sm">{day.condition}</p>
              </div>
            </div>
            <div className="flex items-center">
              <span className="text-blue-100 text-sm mr-3">{Math.round(day.minTemp)}°</span>
              <div className="w-20 h-2 bg-white/20 rounded-full overflow-hidden">
                <motion.div
                  initial={{ width: 0 }}
                  animate={{ width: `${getTempPercentage(day.minTemp, day.maxTemp, day.maxTemp)}%` }}
                  transition={{ duration: 1, delay: 0.5 + index * 0.1 }}
                  className="h-full bg-gradient-to-r from-blue-400 to-red-400"
                />
              </div>
              <span className="text-white text-sm ml-3">{Math.round(day.maxTemp)}°</span>
            </div>
          </motion.div>
        ))}
      </div>
    </div>
  )
}

function formatDate(dateString: string): string {
  const date = new Date(dateString)
  return date.toLocaleDateString([], { weekday: "short", month: "short", day: "numeric" })
}

function getTempPercentage(min: number, max: number, value: number): number {
  return ((value - min) / (max - min)) * 100
}