"use client"

import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card"
import { motion } from "framer-motion"

export default function WeatherForecast({ data }) {
  const { forecast24hr, forecast4day, forecast2hr } = data

  return (
    <motion.div initial={{ opacity: 0, y: 20 }} animate={{ opacity: 1, y: 0 }} transition={{ duration: 0.5 }}>
      <Card>
        <CardHeader>
          <CardTitle>Weather Forecast</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="space-y-4">
            <motion.div initial={{ opacity: 0 }} animate={{ opacity: 1 }} transition={{ delay: 0.2 }}>
              <h3 className="font-semibold">24-hour Forecast</h3>
              <p>{forecast24hr.items[0].general.forecast}</p>
            </motion.div>
            <motion.div initial={{ opacity: 0 }} animate={{ opacity: 1 }} transition={{ delay: 0.4 }}>
              <h3 className="font-semibold">4-day Forecast</h3>
              <ul>
                {forecast4day.items[0].forecasts.map((day, index) => (
                  <li key={index}>
                    {day.date}: {day.forecast}
                  </li>
                ))}
              </ul>
            </motion.div>
            <motion.div initial={{ opacity: 0 }} animate={{ opacity: 1 }} transition={{ delay: 0.6 }}>
              <h3 className="font-semibold">2-hour Forecast</h3>
              <p>{forecast2hr.items[0].forecasts[0].forecast}</p>
            </motion.div>
          </div>
        </CardContent>
      </Card>
    </motion.div>
  )
}