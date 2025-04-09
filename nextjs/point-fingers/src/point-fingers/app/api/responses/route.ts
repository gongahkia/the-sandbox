import { NextResponse } from "next/server"
import { createClient } from "@/utils/supabase/server"

export async function POST(request: Request) {
  try {
    const supabase = await createClient()
    const { promptId, answer, userId } = await request.json()

    // Store the user's response
    const { data, error } = await supabase
      .from("responses")
      .insert([
        {
          prompt_id: promptId,
          user_id: userId,
          answer,
        },
      ])
      .select()

    if (error) {
      throw error
    }

    // Now we need to analyze this response compared to others
    // This would typically be done by the FastAPI backend
    // For now, we'll make a request to our FastAPI service
    const analysisResponse = await fetch(`${process.env.FASTAPI_URL}/analyze-response`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        promptId,
        answer,
        userId,
      }),
    })

    if (!analysisResponse.ok) {
      throw new Error("Failed to analyze response")
    }

    const analysisData = await analysisResponse.json()

    // Update the response with the analysis results
    const { error: updateError } = await supabase
      .from("responses")
      .update({
        percentile: analysisData.percentile,
        position: analysisData.position,
        cluster: analysisData.cluster,
      })
      .eq("id", data[0].id)

    if (updateError) {
      throw updateError
    }

    return NextResponse.json({ success: true, id: data[0].id })
  } catch (error) {
    console.error("Error submitting response:", error)
    return NextResponse.json({ error: "Failed to submit response" }, { status: 500 })
  }
}