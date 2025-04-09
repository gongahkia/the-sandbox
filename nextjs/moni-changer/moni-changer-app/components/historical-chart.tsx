"use client"

import { useState, useEffect } from "react"
import { Area, AreaChart, ResponsiveContainer, Tooltip, XAxis, YAxis } from "recharts"

interface HistoricalChartProps {
  baseCurrency: string
  targetCurrency: string
}

interface DataPoint {
  date: string
  rate: number
}

export default function HistoricalChart({ baseCurrency, targetCurrency }: HistoricalChartProps) {
  const [data, setData] = useState<DataPoint[]>([])
  const [loading, setLoading] = useState(true)
  const [timeframe, setTimeframe] = useState<"7d" | "1m" | "3m" | "1y">("7d")

  useEffect(() => {
    fetchHistoricalData()
  }, [baseCurrency, targetCurrency, timeframe])

  const fetchHistoricalData = async () => {
    setLoading(true)

    // In a real app, we would fetch from Unirate's historical API
    // For this demo, we'll generate mock data
    const mockData = generateMockHistoricalData(timeframe)
    setData(mockData)
    setLoading(false)
  }

  const generateMockHistoricalData = (period: "7d" | "1m" | "3m" | "1y"): DataPoint[] => {
    const data: DataPoint[] = []
    const now = new Date()
    let days: number

    switch (period) {
      case "7d":
        days = 7
        break
      case "1m":
        days = 30
        break
      case "3m":
        days = 90
        break
      case "1y":
        days = 365
        break
    }

    // Generate a starting rate between 0.5 and 2
    const baseRate = 0.5 + Math.random() * 1.5

    // Generate data points with some random fluctuation
    for (let i = days; i >= 0; i--) {
      const date = new Date(now)
      date.setDate(date.getDate() - i)

      // Add some random fluctuation to create a realistic looking chart
      const fluctuation = (Math.random() - 0.5) * 0.1
      const rate = baseRate + fluctuation * (i / 10)

      data.push({
        date: date.toISOString().split("T")[0],
        rate: Number.parseFloat(rate.toFixed(4)),
      })
    }

    return data
  }

  const formatDate = (dateStr: string) => {
    const date = new Date(dateStr)
    return date.toLocaleDateString("en-US", { month: "short", day: "numeric" })
  }

  const CustomTooltip = ({ active, payload }: any) => {
    if (active && payload && payload.length) {
      return (
        <div className="bg-white/10 backdrop-blur-md p-3 rounded-lg border border-white/20 shadow-lg">
          <p className="text-white/80 text-xs">{formatDate(payload[0].payload.date)}</p>
          <p className="text-white font-medium">
            1 {baseCurrency} = {payload[0].value} {targetCurrency}
          </p>
        </div>
      )
    }
    return null
  }

  return (
    <div className="w-full">
      <div className="flex justify-between items-center mb-4">
        <h3 className="text-white font-medium">
          {baseCurrency}/{targetCurrency} Exchange Rate
        </h3>
        <div className="flex space-x-2">
          {(["7d", "1m", "3m", "1y"] as const).map((period) => (
            <button
              key={period}
              onClick={() => setTimeframe(period)}
              className={`px-2 py-1 text-xs rounded-md transition-colors ${
                timeframe === period ? "bg-purple-500/50 text-white" : "bg-white/5 text-white/60 hover:bg-white/10"
              }`}
            >
              {period}
            </button>
          ))}
        </div>
      </div>

      {loading ? (
        <div className="h-64 w-full flex items-center justify-center">
          <div className="animate-spin rounded-full h-8 w-8 border-t-2 border-b-2 border-purple-500"></div>
        </div>
      ) : (
        <div className="h-64 w-full">
          <ResponsiveContainer width="100%" height="100%">
            <AreaChart data={data} margin={{ top: 5, right: 5, left: 5, bottom: 5 }}>
              <defs>
                <linearGradient id="colorRate" x1="0" y1="0" x2="0" y2="1">
                  <stop offset="5%" stopColor="#8b5cf6" stopOpacity={0.8} />
                  <stop offset="95%" stopColor="#8b5cf6" stopOpacity={0} />
                </linearGradient>
              </defs>
              <XAxis
                dataKey="date"
                tick={{ fill: "rgba(255, 255, 255, 0.6)", fontSize: 12 }}
                tickFormatter={formatDate}
                axisLine={{ stroke: "rgba(255, 255, 255, 0.1)" }}
                tickLine={{ stroke: "rgba(255, 255, 255, 0.1)" }}
              />
              <YAxis
                tick={{ fill: "rgba(255, 255, 255, 0.6)", fontSize: 12 }}
                axisLine={{ stroke: "rgba(255, 255, 255, 0.1)" }}
                tickLine={{ stroke: "rgba(255, 255, 255, 0.1)" }}
                domain={["dataMin - 0.05", "dataMax + 0.05"]}
              />
              <Tooltip content={<CustomTooltip />} />
              <Area
                type="monotone"
                dataKey="rate"
                stroke="#8b5cf6"
                fillOpacity={1}
                fill="url(#colorRate)"
                strokeWidth={2}
              />
            </AreaChart>
          </ResponsiveContainer>
        </div>
      )}
    </div>
  )
}