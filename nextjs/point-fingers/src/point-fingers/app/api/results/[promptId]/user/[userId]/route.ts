import { NextResponse } from "next/server"
import { createClient } from "@/utils/supabase/server"

export async function GET(request: Request, { params }: { params: { promptId: string; userId: string } }) {
  try {
    const supabase = await createClient()

    // Get the user's response for this prompt
    const { data, error } = await supabase
      .from("responses")
      .select("*")
      .eq("prompt_id", params.promptId)
      .eq("user_id", params.userId)
      .single()

    if (error) {
      if (error.code === "PGRST116") {
        // No response found
        return NextResponse.json(null)
      }
      throw error
    }

    return NextResponse.json(data)
  } catch (error) {
    console.error("Error fetching user result:", error)
    return NextResponse.json({ error: "Failed to fetch user result" }, { status: 500 })
  }
}