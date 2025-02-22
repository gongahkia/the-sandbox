"use client"

import { useState } from "react"
import { Card, Title, ScatterChart, Button } from "@tremor/react"

const dataPoints = [
  "number of ice creams eaten",
  "liters of rain in the last week",
  "hours spent watching cat videos",
  "number of socks lost in the laundry",
  "cups of coffee consumed",
  "minutes spent in traffic",
  "number of times 'um' was said in a meeting",
  "number of times 'yes' was said in a meeting",
  "number of times 'maybe' was said in a meeting",
  "unread emails in inbox",
  "plants accidentally killed",
  "pizza slices eaten at 2 am",
  "number of eyes closed in sleep",
  "number of times 'no' was said in a meeting",
  "number of times 'not sure' was said in a meeting",
  "litres of coffee consumed since 2020 worldwide",
]

const generateRandomData = () => {
  const xLabel = dataPoints[Math.floor(Math.random() * dataPoints.length)]
  let yLabel
  do {
    yLabel = dataPoints[Math.floor(Math.random() * dataPoints.length)]
  } while (yLabel === xLabel)

  const data = Array.from({ length: 50 }, () => ({
    x: Math.floor(Math.random() * 100),
    y: Math.floor(Math.random() * 100),
  }))

  return { xLabel, yLabel, data }
}

export default function RandomCorrelationGraph() {
  const [graphData, setGraphData] = useState(generateRandomData())

  const regenerateData = () => {
    setGraphData(generateRandomData())
  }

  return (
    <Card className="mt-4">
      <Title>
        Correlation between {graphData.xLabel} and {graphData.yLabel}
      </Title>
      <ScatterChart
        className="h-80 mt-6"
        data={graphData.data}
        category="x"
        x="x"
        y="y"
        showLegend={false}
        valueFormatter={{
          x: (value) => `${value} ${graphData.xLabel}`,
          y: (value) => `${value} ${graphData.yLabel}`,
        }}
      />
      <div className="mt-4">
        <Button onClick={regenerateData}>Generate more statistics</Button>
      </div>
    </Card>
  )
}