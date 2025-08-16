import { NextResponse } from "next/server"
import { createClient } from "@/utils/supabase/server"

export async function GET(request: Request, { params }: { params: { id: string } }) {
  try {
    const supabase = await createClient()

    // Get the prompt by ID
    const { data, error } = await supabase.from("prompts").select("*").eq("id", params.id).single()

    if (error) {
      throw error
    }

    return NextResponse.json(data)
  } catch (error) {
    console.error("Error fetching prompt:", error)
    return NextResponse.json({ error: "Failed to fetch prompt" }, { status: 500 })
  }
}