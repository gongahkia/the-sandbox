"use client"

import { motion } from "framer-motion"
import { Cloud, CloudRain, CloudSnow, CloudLightning, Sun, Moon, CloudDrizzle, CloudFog } from "lucide-react"

interface WeatherIconProps {
  condition: string
  isDay: boolean
  size?: number
}

export default function WeatherIcon({ condition, isDay, size = 100 }: WeatherIconProps) {
  const lowerCondition = condition.toLowerCase()

  // Map weather condition to appropriate icon
  const getIcon = () => {
    if (lowerCondition.includes("clear") || lowerCondition.includes("sunny")) {
      return isDay ? <Sun className="text-amber-400" /> : <Moon className="text-blue-200" />
    } else if (lowerCondition.includes("cloud") || lowerCondition.includes("overcast")) {
      return <Cloud className="text-gray-200" />
    } else if (lowerCondition.includes("rain") || lowerCondition.includes("shower")) {
      return <CloudRain className="text-blue-300" />
    } else if (lowerCondition.includes("snow") || lowerCondition.includes("sleet")) {
      return <CloudSnow className="text-white" />
    } else if (lowerCondition.includes("thunder") || lowerCondition.includes("lightning")) {
      return <CloudLightning className="text-amber-300" />
    } else if (lowerCondition.includes("drizzle")) {
      return <CloudDrizzle className="text-blue-200" />
    } else if (lowerCondition.includes("fog") || lowerCondition.includes("mist")) {
      return <CloudFog className="text-gray-300" />
    } else {
      // Default icon
      return isDay ? <Sun className="text-amber-400" /> : <Moon className="text-blue-200" />
    }
  }

  return (
    <motion.div
      className="flex items-center justify-center w-full h-full"
      initial={{ rotate: 0 }}
      animate={{
        rotate: lowerCondition.includes("clear") || lowerCondition.includes("sunny") ? 360 : 0,
        scale: [1, 1.05, 1],
      }}
      transition={{
        rotate: { duration: 20, repeat: Number.POSITIVE_INFINITY, ease: "linear" },
        scale: { duration: 3, repeat: Number.POSITIVE_INFINITY, ease: "easeInOut" },
      }}
      style={{ width: size, height: size }}
    >
      {getIcon()}
    </motion.div>
  )
}