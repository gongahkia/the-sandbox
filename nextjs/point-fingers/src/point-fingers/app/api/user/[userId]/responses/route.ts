import { NextResponse } from "next/server"
import { createClient } from "@/utils/supabase/server"

export async function GET(request: Request, { params }: { params: { userId: string } }) {
  try {
    const { searchParams } = new URL(request.url)
    const limit = Number.parseInt(searchParams.get("limit") || "10")

    const supabase = await createClient()

    // Get the user's responses with prompt information
    const { data, error } = await supabase
      .from("responses")
      .select(`
        *,
        prompt:prompts(*)
      `)
      .eq("user_id", params.userId)
      .order("created_at", { ascending: false })
      .limit(limit)

    if (error) {
      throw error
    }

    return NextResponse.json(data)
  } catch (error) {
    console.error("Error fetching user responses:", error)
    return NextResponse.json({ error: "Failed to fetch user responses" }, { status: 500 })
  }
}