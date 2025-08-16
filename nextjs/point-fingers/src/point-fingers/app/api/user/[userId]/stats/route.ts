import { NextResponse } from "next/server"
import { createClient } from "@/utils/supabase/server"

export async function GET(request: Request, { params }: { params: { userId: string } }) {
  try {
    const supabase = await createClient()

    // Get all of the user's responses
    const { data: responses, error } = await supabase
      .from("responses")
      .select(`
        *,
        prompt:prompts(*)
      `)
      .eq("user_id", params.userId)

    if (error) {
      throw error
    }

    // Calculate stats
    const totalAnswered = responses.length

    if (totalAnswered === 0) {
      return NextResponse.json({
        totalAnswered: 0,
        averageUniqueness: 0,
        weirdnessScore: 0,
        weirdnessRank: "N/A",
        mostUniqueCategory: "N/A",
      })
    }

    const averageUniqueness = responses.reduce((sum, r) => sum + (r.percentile || 0), 0) / totalAnswered

    // Calculate weirdness score (0-100)
    const weirdnessScore = Math.min(100, Math.round(averageUniqueness * 1.2))

    // Group responses by category to find most unique category
    const categoryScores = responses.reduce(
      (acc, r) => {
        const category = r.prompt?.category || "Uncategorized"
        if (!acc[category]) {
          acc[category] = { total: 0, count: 0 }
        }
        acc[category].total += r.percentile || 0
        acc[category].count += 1
        return acc
      },
      {} as Record<string, { total: number; count: number }>,
    )

    const categoryAverages = Object.entries(categoryScores).map(([category, { total, count }]) => ({
      category,
      average: total / count,
    }))

    const mostUniqueCategory =
      categoryAverages.length > 0 ? categoryAverages.sort((a, b) => b.average - a.average)[0].category : "N/A"

    // Determine rank based on weirdness score
    let weirdnessRank = "Average"
    if (weirdnessScore >= 90) weirdnessRank = "Extremely Unique"
    else if (weirdnessScore >= 75) weirdnessRank = "Very Unique"
    else if (weirdnessScore >= 60) weirdnessRank = "Quite Unique"
    else if (weirdnessScore >= 40) weirdnessRank = "Somewhat Unique"
    else if (weirdnessScore < 25) weirdnessRank = "Very Common"

    return NextResponse.json({
      totalAnswered,
      averageUniqueness,
      weirdnessScore,
      weirdnessRank,
      mostUniqueCategory,
    })
  } catch (error) {
    console.error("Error fetching user stats:", error)
    return NextResponse.json({ error: "Failed to fetch user stats" }, { status: 500 })
  }
}