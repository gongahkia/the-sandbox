"use client"

import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card"
import { motion } from "framer-motion"

export default function EnvironmentalData({ data }) {
  const { psi, uv, pm25, wetBulb } = data

  const getPsiReading = () => {
    try {
      return psi.items[0].readings.psi_twenty_four_hourly.national || "N/A"
    } catch (error) {
      console.error("Error getting PSI reading:", error)
      return "N/A"
    }
  }

  const getUvIndex = () => {
    try {
      return uv.items[0].index[0].value || "N/A"
    } catch (error) {
      console.error("Error getting UV index:", error)
      return "N/A"
    }
  }

  const getPm25Reading = () => {
    try {
      return pm25.items[0].readings.pm25_one_hourly.national || "N/A"
    } catch (error) {
      console.error("Error getting PM2.5 reading:", error)
      return "N/A"
    }
  }

  const getWetBulbTemperature = () => {
    try {
      return wetBulb.items[0].stations[0].value ? `${wetBulb.items[0].stations[0].value}°C` : "N/A"
    } catch (error) {
      console.error("Error getting Wet Bulb Temperature:", error)
      return "N/A"
    }
  }

  return (
    <motion.div initial={{ opacity: 0, y: 20 }} animate={{ opacity: 1, y: 0 }} transition={{ duration: 0.5 }}>
      <Card>
        <CardHeader>
          <CardTitle>Environmental Data</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-2 gap-4">
            {[
              { title: "PSI", value: getPsiReading() },
              { title: "UV Index", value: getUvIndex() },
              { title: "PM2.5", value: getPm25Reading() },
              { title: "Wet Bulb Temperature", value: getWetBulbTemperature() },
            ].map((item, index) => (
              <motion.div
                key={item.title}
                initial={{ opacity: 0 }}
                animate={{ opacity: 1 }}
                transition={{ delay: 0.1 * (index + 1) }}
              >
                <h3 className="font-semibold">{item.title}</h3>
                <p>{item.value}</p>
              </motion.div>
            ))}
          </div>
        </CardContent>
      </Card>
    </motion.div>
  )
}