"use client"

import type React from "react"
import { useState, useEffect } from "react"
import { motion, AnimatePresence } from "framer-motion"
import { Card, CardContent } from "@/components/ui/card"
import { Input } from "@/components/ui/input"
import { Label } from "@/components/ui/label"
import { Button } from "@/components/ui/button"
import Link from "next/link"

interface CarparkData {
  carpark_number: string
  total_lots: number
  lots_available: number
}

interface CarparkGridProps {
  data: CarparkData[]
}

const CarparkGrid: React.FC<CarparkGridProps> = ({ data }) => {
  const [searchTerm, setSearchTerm] = useState("")
  const [filteredData, setFilteredData] = useState(data)
  const [displayedData, setDisplayedData] = useState<CarparkData[]>([])
  const [displayCount, setDisplayCount] = useState(100)

  useEffect(() => {
    const filtered = data.filter((carpark) => carpark.carpark_number.toLowerCase().includes(searchTerm.toLowerCase()))
    setFilteredData(filtered)
    setDisplayCount(100)
  }, [searchTerm, data])

  useEffect(() => {
    setDisplayedData(filteredData.slice(0, displayCount))
  }, [filteredData, displayCount])

  const getColor = (availability: number) => {
    const hue = availability * 120
    return `hsl(${hue}, 80%, 50%)`
  }

  const getSize = (totalLots: number) => {
    const minSize = 100
    const maxSize = 200
    const maxLots = Math.max(...data.map((d) => d.total_lots))
    return minSize + (totalLots / maxLots) * (maxSize - minSize)
  }

  const loadMore = () => {
    setDisplayCount((prevCount) => prevCount + 100)
  }

  return (
    <div className="space-y-8">
      <div className="text-center mb-4">
        <span>Made with ❤️ by </span>
        <Link href="https://github.com/gongahkia" className="text-blue-600 hover:underline">
          Gabriel Ong
        </Link>
      </div>

      <div className="bg-white p-6 rounded-lg shadow-md mb-8">
        <h2 className="text-2xl font-bold mb-4 text-center">Legend</h2>
        <div className="flex justify-center items-center space-x-8">
          {[0, 0.5, 1].map((value) => (
            <div key={value} className="flex items-center">
              <motion.div
                className="w-8 h-8 rounded-full mr-2"
                style={{ backgroundColor: getColor(value) }}
                whileHover={{ scale: 1.2 }}
              />
              <span>{value === 0 ? "Low" : value === 0.5 ? "Medium" : "High"} Availability</span>
            </div>
          ))}
        </div>
        <div className="mt-4 text-center text-sm text-gray-600">Card size represents total number of lots</div>
      </div>

      <div className="max-w-md mx-auto mb-8">
        <Label htmlFor="search" className="text-lg font-semibold mb-2">
          Search Carpark
        </Label>
        <Input
          id="search"
          type="text"
          placeholder="Enter carpark number..."
          value={searchTerm}
          onChange={(e) => setSearchTerm(e.target.value)}
          className="w-full"
        />
      </div>

      <motion.div
        className="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 lg:grid-cols-4 xl:grid-cols-5 gap-6 justify-items-center"
        layout
      >
        <AnimatePresence>
          {displayedData.map((carpark) => {
            const availability = carpark.lots_available / carpark.total_lots
            const color = getColor(availability)
            const size = getSize(carpark.total_lots)

            return (
              <motion.div
                key={carpark.carpark_number}
                layout
                initial={{ opacity: 0, scale: 0.8 }}
                animate={{ opacity: 1, scale: 1 }}
                exit={{ opacity: 0, scale: 0.8 }}
                transition={{ duration: 0.3 }}
              >
                <Card className="overflow-hidden" style={{ width: size, height: size }}>
                  <CardContent className="h-full flex flex-col items-center justify-center p-4">
                    <motion.div className="text-lg font-bold mb-2" layoutId={`title-${carpark.carpark_number}`}>
                      {carpark.carpark_number}
                    </motion.div>
                    <motion.div
                      className="rounded-full mb-2"
                      style={{
                        backgroundColor: color,
                        width: "60px",
                        height: "60px",
                      }}
                      animate={{ scale: [1, 1.1, 1] }}
                      transition={{ duration: 2, repeat: Number.POSITIVE_INFINITY }}
                    />
                    <motion.div className="text-sm text-center">
                      <div>
                        {carpark.lots_available} / {carpark.total_lots}
                      </div>
                      <div>{(availability * 100).toFixed(0)}% available</div>
                    </motion.div>
                  </CardContent>
                </Card>
              </motion.div>
            )
          })}
        </AnimatePresence>
      </motion.div>

      {filteredData.length > displayCount && (
        <div className="flex justify-center mt-8">
          <Button onClick={loadMore} variant="outline">
            Load More
          </Button>
        </div>
      )}
    </div>
  )
}

export default CarparkGrid