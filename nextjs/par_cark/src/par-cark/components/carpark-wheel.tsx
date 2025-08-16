"use client"

import type React from "react"
import { PieChart, Pie, Cell, Tooltip, ResponsiveContainer } from "recharts"

interface CarparkData {
  carpark_number: string
  total_lots: number
  lots_available: number
}

interface CarparkWheelProps {
  data: CarparkData[]
}

const CarparkWheel: React.FC<CarparkWheelProps> = ({ data }) => {
  const processedData = data.map((item) => ({
    name: item.carpark_number,
    value: item.total_lots,
    availability: item.lots_available / item.total_lots,
  }))

  const getColor = (availability: number) => {
    // Convert availability (0-1) to a color scale from red (low availability) to green (high availability)
    const r = Math.floor(255 * (1 - availability))
    const g = Math.floor(255 * availability)
    return `rgb(${r}, ${g}, 0)`
  }

  return (
    <ResponsiveContainer width="100%" height={600}>
      <PieChart>
        <Pie
          data={processedData}
          cx="50%"
          cy="50%"
          outerRadius={200}
          innerRadius={100}
          dataKey="value"
          labelLine={false}
          label={({ name, value }) => `${name}: ${value}`}
        >
          {processedData.map((entry, index) => (
            <Cell key={`cell-${index}`} fill={getColor(entry.availability)} />
          ))}
        </Pie>
        <Tooltip
          content={({ active, payload }) => {
            if (active && payload && payload.length) {
              const data = payload[0].payload
              return (
                <div className="bg-white p-4 rounded shadow">
                  <p className="font-bold">{data.name}</p>
                  <p>Total Lots: {data.value}</p>
                  <p>Available: {Math.round(data.availability * data.value)}</p>
                  <p>Availability: {(data.availability * 100).toFixed(2)}%</p>
                </div>
              )
            }
            return null
          }}
        />
      </PieChart>
    </ResponsiveContainer>
  )
}

export default CarparkWheel