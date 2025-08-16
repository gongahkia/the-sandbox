"use client"

import { useState, useEffect } from "react"
import Image from "next/image"
import { Loader2 } from "lucide-react"

interface Camera {
  camera_id: string
  image: string
  location: {
    latitude: number
    longitude: number
  }
  timestamp: string
}

interface TrafficData {
  items: [
    {
      cameras: Camera[]
      timestamp: string
    },
  ]
}

export default function TrafficGrid() {
  const [data, setData] = useState<TrafficData | null>(null)
  const [isLoading, setIsLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)

  useEffect(() => {
    async function fetchTrafficImages() {
      try {
        const res = await fetch("https://api.data.gov.sg/v1/transport/traffic-images")
        if (!res.ok) throw new Error("Failed to fetch traffic images")
        const jsonData = await res.json()
        setData(jsonData)
      } catch (err) {
        setError("Failed to load traffic data. Please try again later.")
      } finally {
        setIsLoading(false)
      }
    }

    fetchTrafficImages()
  }, [])

  if (isLoading) {
    return (
      <div className="flex justify-center items-center h-64">
        <Loader2 className="h-12 w-12 animate-spin text-neo-blue" />
      </div>
    )
  }

  if (error) {
    return (
      <div className="bg-red-100 border-4 border-red-500 p-4 text-red-700 text-center">
        <p className="text-2xl font-bold">{error}</p>
      </div>
    )
  }

  return (
    <div>
      <p className="text-xl font-bold mb-4">
        Images retrieved at: {new Date(data?.items[0].timestamp || "").toLocaleString()}
      </p>
      <div className="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 xl:grid-cols-5 gap-4">
        {data?.items[0].cameras.map((camera: Camera) => (
          <div key={camera.camera_id} className="bg-white p-4 shadow-neobrutalist border-4 border-black">
            <Image
              src={camera.image || "/placeholder.svg"}
              alt={`Traffic at ${camera.location.latitude}, ${camera.location.longitude}`}
              width={400}
              height={225}
              className="w-full h-auto object-cover mb-2"
            />
            <p className="text-lg font-bold font-impact">Camera ID: {camera.camera_id}</p>
            <p className="text-sm">
              Location: {camera.location.latitude.toFixed(4)}, {camera.location.longitude.toFixed(4)}
            </p>
          </div>
        ))}
      </div>
    </div>
  )
}