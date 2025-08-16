"use client"

import { useEffect, useRef } from "react"

interface IPMapProps {
  latitude: number
  longitude: number
}

export default function IPMap({ latitude, longitude }: IPMapProps) {
  const mapRef = useRef<HTMLDivElement>(null)

  useEffect(() => {
    if (!mapRef.current || !latitude || !longitude) return

    // In a real application, you would integrate with a mapping library like Leaflet or Google Maps
    // For this example, we'll create a simple visual representation with a cyberpunk aesthetic

    const canvas = document.createElement("canvas")
    canvas.width = mapRef.current.clientWidth
    canvas.height = mapRef.current.clientHeight
    mapRef.current.innerHTML = ""
    mapRef.current.appendChild(canvas)

    const ctx = canvas.getContext("2d")
    if (!ctx) return

    // Draw a dark background
    ctx.fillStyle = "#0a0e17"
    ctx.fillRect(0, 0, canvas.width, canvas.height)

    // Draw grid lines
    ctx.strokeStyle = "#1e2738"
    ctx.lineWidth = 1

    // Vertical grid lines
    for (let x = 0; x < canvas.width; x += 20) {
      ctx.beginPath()
      ctx.moveTo(x, 0)
      ctx.lineTo(x, canvas.height)
      ctx.stroke()
    }

    // Horizontal grid lines
    for (let y = 0; y < canvas.height; y += 20) {
      ctx.beginPath()
      ctx.moveTo(0, y)
      ctx.lineTo(canvas.width, y)
      ctx.stroke()
    }

    // Convert lat/long to x,y coordinates (simplified)
    const x = ((longitude + 180) / 360) * canvas.width
    const y = ((90 - latitude) / 180) * canvas.height

    // Draw outer glow
    const gradient = ctx.createRadialGradient(x, y, 5, x, y, 50)
    gradient.addColorStop(0, "rgba(0, 255, 157, 0.8)")
    gradient.addColorStop(0.5, "rgba(0, 255, 157, 0.2)")
    gradient.addColorStop(1, "rgba(0, 255, 157, 0)")

    ctx.fillStyle = gradient
    ctx.beginPath()
    ctx.arc(x, y, 50, 0, Math.PI * 2)
    ctx.fill()

    // Draw the marker
    ctx.fillStyle = "#00ff9d"
    ctx.beginPath()
    ctx.arc(x, y, 5, 0, Math.PI * 2)
    ctx.fill()

    // Draw targeting circles
    ctx.strokeStyle = "#00ff9d"
    ctx.lineWidth = 1

    // Animated targeting circles
    const drawAnimatedCircles = () => {
      ctx.clearRect(0, 0, canvas.width, canvas.height)

      // Redraw background
      ctx.fillStyle = "#0a0e17"
      ctx.fillRect(0, 0, canvas.width, canvas.height)

      // Redraw grid
      ctx.strokeStyle = "#1e2738"
      ctx.lineWidth = 1
      for (let x = 0; x < canvas.width; x += 20) {
        ctx.beginPath()
        ctx.moveTo(x, 0)
        ctx.lineTo(x, canvas.height)
        ctx.stroke()
      }
      for (let y = 0; y < canvas.height; y += 20) {
        ctx.beginPath()
        ctx.moveTo(0, y)
        ctx.lineTo(canvas.width, y)
        ctx.stroke()
      }

      // Redraw glow
      ctx.fillStyle = gradient
      ctx.beginPath()
      ctx.arc(x, y, 50, 0, Math.PI * 2)
      ctx.fill()

      // Redraw marker
      ctx.fillStyle = "#00ff9d"
      ctx.beginPath()
      ctx.arc(x, y, 5, 0, Math.PI * 2)
      ctx.fill()

      // Draw animated targeting circles
      const time = Date.now() / 1000

      ctx.strokeStyle = "#00ff9d"
      ctx.lineWidth = 1

      for (let i = 1; i <= 3; i++) {
        const radius = 10 + i * 15 + Math.sin(time * i) * 5
        const alpha = 0.7 - radius / 100

        ctx.strokeStyle = `rgba(0, 255, 157, ${alpha})`
        ctx.beginPath()
        ctx.arc(x, y, radius, 0, Math.PI * 2)
        ctx.stroke()
      }

      // Draw crosshairs
      ctx.strokeStyle = "rgba(0, 255, 157, 0.5)"
      ctx.lineWidth = 1

      // Horizontal line
      ctx.beginPath()
      ctx.moveTo(x - 60, y)
      ctx.lineTo(x + 60, y)
      ctx.stroke()

      // Vertical line
      ctx.beginPath()
      ctx.moveTo(x, y - 60)
      ctx.lineTo(x, y + 60)
      ctx.stroke()

      // Add coordinates text with technical font
      ctx.fillStyle = "#00ff9d"
      ctx.font = "12px monospace"
      ctx.fillText(`LAT: ${latitude.toFixed(6)}`, x + 15, y - 15)
      ctx.fillText(`LON: ${longitude.toFixed(6)}`, x + 15, y)

      // Add some technical-looking data
      ctx.fillStyle = "#8a94a8"
      ctx.font = "10px monospace"
      ctx.fillText(`SIGNAL: STRONG`, 10, 20)
      ctx.fillText(`ACCURACY: 98.2%`, 10, 35)
      ctx.fillText(`PING: 42ms`, 10, 50)

      requestAnimationFrame(drawAnimatedCircles)
    }

    drawAnimatedCircles()

    // Handle window resize
    const handleResize = () => {
      if (mapRef.current) {
        canvas.width = mapRef.current.clientWidth
        canvas.height = mapRef.current.clientHeight
      }
    }

    window.addEventListener("resize", handleResize)
    return () => {
      window.removeEventListener("resize", handleResize)
    }
  }, [latitude, longitude])

  if (!latitude || !longitude) {
    return (
      <div className="flex items-center justify-center h-full bg-[#0d121f] text-[#8a94a8] font-mono">
        <p>NO LOCATION DATA AVAILABLE</p>
      </div>
    )
  }

  return <div ref={mapRef} className="w-full h-full" />
}