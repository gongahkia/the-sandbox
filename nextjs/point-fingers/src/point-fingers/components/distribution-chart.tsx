"use client"

import { useEffect, useRef } from "react"
import * as d3 from "d3"

interface DistributionChartProps {
  data: {
    x: number
    y: number
  }[]
  userPosition?: number
}

export function DistributionChart({ data, userPosition }: DistributionChartProps) {
  const svgRef = useRef<SVGSVGElement>(null)

  useEffect(() => {
    if (!data || data.length === 0 || !svgRef.current) return

    // Clear previous chart
    d3.select(svgRef.current).selectAll("*").remove()

    const margin = { top: 20, right: 30, bottom: 40, left: 40 }
    const width = svgRef.current.clientWidth - margin.left - margin.right
    const height = svgRef.current.clientHeight - margin.top - margin.bottom

    const svg = d3.select(svgRef.current).append("g").attr("transform", `translate(${margin.left},${margin.top})`)

    // Create scales
    const xScale = d3.scaleLinear().domain([0, 100]).range([0, width])

    const yScale = d3
      .scaleLinear()
      .domain([0, d3.max(data, (d) => d.y) || 0])
      .range([height, 0])

    // Create the area generator
    const area = d3
      .area<{ x: number; y: number }>()
      .x((d) => xScale(d.x))
      .y0(height)
      .y1((d) => yScale(d.y))
      .curve(d3.curveBasis)

    // Create the line generator
    const line = d3
      .line<{ x: number; y: number }>()
      .x((d) => xScale(d.x))
      .y((d) => yScale(d.y))
      .curve(d3.curveBasis)

    // Add the area path
    svg.append("path").datum(data).attr("fill", "rgba(0, 0, 0, 0.2)").attr("d", area)

    // Add the line path
    svg.append("path").datum(data).attr("fill", "none").attr("stroke", "black").attr("stroke-width", 2).attr("d", line)

    // Add the x-axis
    svg
      .append("g")
      .attr("transform", `translate(0,${height})`)
      .call(d3.axisBottom(xScale).ticks(5))
      .append("text")
      .attr("x", width / 2)
      .attr("y", 30)
      .attr("fill", "black")
      .attr("text-anchor", "middle")
      .text("UNIQUENESS SCORE")

    // Add the y-axis
    svg
      .append("g")
      .call(d3.axisLeft(yScale).ticks(5))
      .append("text")
      .attr("transform", "rotate(-90)")
      .attr("y", -30)
      .attr("x", -height / 2)
      .attr("fill", "black")
      .attr("text-anchor", "middle")
      .text("FREQUENCY")

    // Add user position if available
    if (userPosition !== undefined) {
      // Find the y value at the user's position
      const userYValue = data.find((d) => d.x === userPosition)?.y || 0

      // Add a vertical line at the user's position
      svg
        .append("line")
        .attr("x1", xScale(userPosition))
        .attr("x2", xScale(userPosition))
        .attr("y1", height)
        .attr("y2", yScale(userYValue))
        .attr("stroke", "black")
        .attr("stroke-width", 2)
        .attr("stroke-dasharray", "5,5")

      // Add a circle at the user's position on the curve
      svg
        .append("circle")
        .attr("cx", xScale(userPosition))
        .attr("cy", yScale(userYValue))
        .attr("r", 6)
        .attr("fill", "black")
        .attr("stroke", "white")
        .attr("stroke-width", 2)

      // Add "You" label
      svg
        .append("text")
        .attr("x", xScale(userPosition))
        .attr("y", yScale(userYValue) - 10)
        .attr("text-anchor", "middle")
        .attr("fill", "black")
        .attr("font-weight", "bold")
        .text("YOU 👉")
    }
  }, [data, userPosition])

  return <svg ref={svgRef} className="h-full w-full overflow-visible" preserveAspectRatio="xMidYMid meet" />
}