import { NextResponse } from "next/server"
import { createClient } from "@/utils/supabase/server"

export async function GET() {
  try {
    const supabase = await createClient()

    // Get a random prompt from the database
    const { data, error } = await supabase.from("prompts").select("*").order("id", { ascending: false }).limit(20)

    if (error) {
      throw error
    }

    // Select a random prompt from the results
    const randomIndex = Math.floor(Math.random() * data.length)
    const randomPrompt = data[randomIndex]

    return NextResponse.json(randomPrompt)
  } catch (error) {
    console.error("Error fetching random prompt:", error)
    return NextResponse.json({ error: "Failed to fetch random prompt" }, { status: 500 })
  }
}