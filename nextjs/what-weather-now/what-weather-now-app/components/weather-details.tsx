"use client"

import { motion } from "framer-motion"
import { Wind, Droplets, Sun, Thermometer } from "lucide-react"
import type { CurrentWeather } from "@/lib/types"

interface WeatherDetailsProps {
  weatherData: CurrentWeather
}

export default function WeatherDetails({ weatherData }: WeatherDetailsProps) {
  const detailItems = [
    {
      label: "Wind Speed",
      value: `${weatherData.windSpeed} km/h`,
      icon: Wind,
      color: "from-emerald-500 to-teal-500",
    },
    {
      label: "Humidity",
      value: `${weatherData.humidity}%`,
      icon: Droplets,
      color: "from-blue-500 to-indigo-500",
    },
    {
      label: "UV Index",
      value: weatherData.uvIndex,
      icon: Sun,
      color: "from-amber-500 to-orange-500",
    },
    {
      label: "Pressure",
      value: `${weatherData.pressure} hPa`,
      icon: Thermometer,
      color: "from-rose-500 to-pink-500",
    },
  ]

  return (
    <div className="backdrop-blur-md bg-white/10 rounded-2xl p-6">
      <h3 className="text-xl font-medium text-white mb-4">Weather Details</h3>
      <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
        {detailItems.map((item, index) => (
          <motion.div
            key={item.label}
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.3, delay: index * 0.1 }}
            className="relative overflow-hidden rounded-xl p-4"
          >
            <div className={`absolute inset-0 bg-gradient-to-br ${item.color} opacity-20`} />
            <div className="relative z-10">
              <div className="flex items-center mb-2">
                <item.icon className="w-5 h-5 text-white opacity-70 mr-2" />
                <span className="text-sm text-white opacity-70">{item.label}</span>
              </div>
              <p className="text-xl font-medium text-white">{item.value}</p>
            </div>
          </motion.div>
        ))}
      </div>
    </div>
  )
}