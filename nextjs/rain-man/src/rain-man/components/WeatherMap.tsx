"use client"

import { useEffect, useRef } from "react"
import { MapContainer, TileLayer, Marker, Popup } from "react-leaflet"
import "leaflet/dist/leaflet.css"
import L from "leaflet"
import { motion } from "framer-motion"

export default function WeatherMap({ data }) {
  const { windspeed, windDirection, rainfall, humidity, airTemp, lightning } = data
  const mapRef = useRef(null)

  useEffect(() => {
    if (typeof window !== "undefined") {
      delete L.Icon.Default.prototype._getIconUrl
      L.Icon.Default.mergeOptions({
        iconRetinaUrl: "/marker-icon-2x.png",
        iconUrl: "/marker-icon.png",
        shadowUrl: "/marker-shadow.png",
      })
    }
  }, [])

  const singaporeCenter = [1.3521, 103.8198]

  const getStations = () => {
    if (!windspeed || !windspeed.stations) return []
    return windspeed.stations.map((station, index) => ({
      ...station,
      windDirection: windDirection?.stations[index]?.value || "N/A",
      rainfall: rainfall?.stations[index]?.value || "N/A",
      humidity: humidity?.stations[index]?.value || "N/A",
      temperature: airTemp?.stations[index]?.value || "N/A",
    }))
  }

  const getLightningStrikes = () => {
    if (!lightning || !lightning.data) return []
    return lightning.data
  }

  return (
    <motion.div
      className="mt-8"
      initial={{ opacity: 0, y: 20 }}
      animate={{ opacity: 1, y: 0 }}
      transition={{ duration: 0.5 }}
    >
      <h2 className="text-2xl font-bold mb-4">Weather Map</h2>
      <div style={{ height: "500px" }}>
        <MapContainer center={singaporeCenter} zoom={11} style={{ height: "100%", width: "100%" }} ref={mapRef}>
          <TileLayer url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png" />
          {getStations().map((station, index) => (
            <Marker key={index} position={[station.location.latitude, station.location.longitude]}>
              <Popup>
                <div>
                  <h3 className="font-semibold">{station.name}</h3>
                  <p>Wind Speed: {station.value} km/h</p>
                  <p>Wind Direction: {station.windDirection}°</p>
                  <p>Rainfall: {station.rainfall} mm</p>
                  <p>Humidity: {station.humidity}%</p>
                  <p>Temperature: {station.temperature}°C</p>
                </div>
              </Popup>
            </Marker>
          ))}
          {getLightningStrikes().map((strike, index) => (
            <Marker
              key={index}
              position={[strike.latitude, strike.longitude]}
              icon={L.divIcon({ className: "lightning-icon", html: "⚡" })}
            >
              <Popup>Lightning Strike</Popup>
            </Marker>
          ))}
        </MapContainer>
      </div>
    </motion.div>
  )
}