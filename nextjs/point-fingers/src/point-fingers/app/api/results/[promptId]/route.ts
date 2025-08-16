import { NextResponse } from "next/server"
import { createClient } from "@/utils/supabase/server"

export async function GET(request: Request, { params }: { params: { promptId: string } }) {
  try {
    // This would typically fetch processed results from the FastAPI backend
    // For now, we'll simulate this by fetching from Supabase and doing simple calculations

    const supabase = await createClient()

    // Get all responses for this prompt
    const { data: responses, error } = await supabase.from("responses").select("*").eq("prompt_id", params.promptId)

    if (error) {
      throw error
    }

    // Generate a normal distribution based on the responses
    // This is a simplified version - the real analysis would be done by FastAPI
    const totalResponses = responses.length

    if (totalResponses === 0) {
      return NextResponse.json({
        totalResponses: 0,
        distribution: [],
        averageScore: 0,
        mostCommon: null,
        mostUnique: null,
      })
    }

    // Create a simple distribution (this would be more sophisticated in the real app)
    const distribution = Array.from({ length: 101 }, (_, i) => ({
      x: i,
      y: Math.exp(-0.5 * Math.pow((i - 50) / 15, 2)) / (15 * Math.sqrt(2 * Math.PI)),
    }))

    // Calculate some basic stats
    const averageScore = responses.reduce((sum, r) => sum + (r.percentile || 0), 0) / totalResponses

    // Group responses by answer to find most common
    const answerCounts = responses.reduce(
      (acc, r) => {
        acc[r.answer] = (acc[r.answer] || 0) + 1
        return acc
      },
      {} as Record<string, number>,
    )

    const mostCommon = Object.entries(answerCounts)
      .sort((a, b) => b[1] - a[1])
      .map(([answer]) => answer)[0]

    const mostUnique = responses.sort((a, b) => (b.percentile || 0) - (a.percentile || 0)).map((r) => r.answer)[0]

    return NextResponse.json({
      totalResponses,
      distribution,
      averageScore,
      mostCommon,
      mostUnique,
    })
  } catch (error) {
    console.error("Error fetching results:", error)
    return NextResponse.json({ error: "Failed to fetch results" }, { status: 500 })
  }
}
