"use client"

import type React from "react"

import { useState } from "react"
import { motion, AnimatePresence } from "framer-motion"
import { Search, MapPin } from "lucide-react"

interface LocationSearchProps {
  onLocationChange: (location: string) => void
  currentLocation: string
}

// Sample popular locations
const popularLocations = [
  "New York",
  "London",
  "Tokyo",
  "Paris",
  "Sydney",
  "Berlin",
  "Toronto",
  "Singapore",
  "Dubai",
  "Mumbai",
]

export default function LocationSearch({ onLocationChange, currentLocation }: LocationSearchProps) {
  const [searchValue, setSearchValue] = useState("")
  const [isExpanded, setIsExpanded] = useState(false)

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault()
    if (searchValue.trim()) {
      onLocationChange(searchValue)
      setSearchValue("")
      setIsExpanded(false)
    }
  }

  return (
    <div className="relative">
      <div className="flex items-center">
        <motion.form
          onSubmit={handleSubmit}
          className="relative flex-1"
          initial={{ width: "100%" }}
          animate={{ width: isExpanded ? "100%" : "100%" }}
        >
          <div className="relative">
            <input
              type="text"
              value={searchValue}
              onChange={(e) => setSearchValue(e.target.value)}
              onFocus={() => setIsExpanded(true)}
              placeholder="Search for a city..."
              className="w-full py-3 pl-10 pr-4 bg-white/10 backdrop-blur-md text-white placeholder-blue-100/50 rounded-xl focus:outline-none focus:ring-2 focus:ring-blue-400/30 transition-all"
            />
            <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-blue-100/70 w-5 h-5" />
          </div>
        </motion.form>
      </div>

      <AnimatePresence>
        {isExpanded && (
          <motion.div
            initial={{ opacity: 0, y: -10 }}
            animate={{ opacity: 1, y: 0 }}
            exit={{ opacity: 0, y: -10 }}
            transition={{ duration: 0.2 }}
            className="absolute z-20 mt-2 w-full bg-slate-800/90 backdrop-blur-md rounded-xl shadow-lg overflow-hidden"
          >
            <div className="p-3">
              <p className="text-blue-100/70 text-xs uppercase font-medium mb-2 px-2">Popular Cities</p>
              <div className="grid grid-cols-2 gap-2">
                {popularLocations.map((location) => (
                  <button
                    key={location}
                    onClick={() => {
                      onLocationChange(location)
                      setIsExpanded(false)
                    }}
                    className="flex items-center p-2 rounded-lg hover:bg-white/10 text-left transition-colors"
                  >
                    <MapPin className="w-4 h-4 text-blue-100/50 mr-2" />
                    <span className="text-white text-sm">{location}</span>
                  </button>
                ))}
              </div>
            </div>
            <div className="border-t border-white/10 p-3 text-center">
              <button
                onClick={() => setIsExpanded(false)}
                className="text-blue-100/70 text-sm hover:text-white transition-colors"
              >
                Close
              </button>
            </div>
          </motion.div>
        )}
      </AnimatePresence>
    </div>
  )
}