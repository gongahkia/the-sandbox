"use client"

import type React from "react"

import { useState, useEffect } from "react"
import { useRouter } from "next/navigation"
import { createClient } from "@/utils/supabase/client"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card"
import { Input } from "@/components/ui/input"
import { Textarea } from "@/components/ui/textarea"
import { toast } from "@/components/ui/use-toast"
import { Loader2 } from "lucide-react"

export default function PromptsPage() {
  const [currentPrompt, setCurrentPrompt] = useState<any>(null)
  const [answer, setAnswer] = useState("")
  const [loading, setLoading] = useState(true)
  const [submitting, setSubmitting] = useState(false)
  const [user, setUser] = useState<any>(null)
  const router = useRouter()
  const supabase = createClient()

  useEffect(() => {
    const checkAuth = async () => {
      const {
        data: { session },
      } = await supabase.auth.getSession()
      if (!session) {
        router.push("/login")
        return
      }
      setUser(session.user)
      fetchRandomPrompt()
    }

    checkAuth()
  }, [router, supabase])

  const fetchRandomPrompt = async () => {
    setLoading(true)
    try {
      // Fetch a random prompt from our backend
      const response = await fetch("/api/prompts/random")
      if (!response.ok) throw new Error("Failed to fetch prompt")

      const data = await response.json()
      setCurrentPrompt(data)
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to load a prompt. Please try again.",
        variant: "destructive",
      })
    } finally {
      setLoading(false)
    }
  }

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    if (!answer.trim()) return

    setSubmitting(true)
    try {
      // Submit the answer to our backend
      const response = await fetch("/api/responses", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          promptId: currentPrompt.id,
          answer,
          userId: user.id,
        }),
      })

      if (!response.ok) throw new Error("Failed to submit response")

      toast({
        title: "Response submitted",
        description: "Your answer has been recorded. Let's see how you compare!",
      })

      // Redirect to results page
      router.push(`/results/${currentPrompt.id}`)
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to submit your response. Please try again.",
        variant: "destructive",
      })
    } finally {
      setSubmitting(false)
    }
  }

  const handleSkip = () => {
    setAnswer("")
    fetchRandomPrompt()
  }

  if (loading) {
    return (
      <div className="container flex h-screen items-center justify-center bg-white">
        <Loader2 className="h-8 w-8 animate-spin text-black" />
      </div>
    )
  }

  return (
    <div className="container max-w-2xl py-10 bg-white">
      <Card className="card-brutalist">
        <CardHeader className="border-b-2 border-black">
          <CardTitle className="uppercase">Answer this prompt 👉</CardTitle>
          <CardDescription className="text-black">
            Share your thoughts and see how you compare to others
          </CardDescription>
        </CardHeader>
        <form onSubmit={handleSubmit}>
          <CardContent className="space-y-4 pt-6">
            <div className="rounded-none bg-black text-white p-4">
              <h3 className="text-lg font-medium uppercase">{currentPrompt?.text || "Loading prompt..."}</h3>
              <p className="text-sm mt-2">{currentPrompt?.description || ""}</p>
            </div>

            {currentPrompt?.type === "text" ? (
              <Textarea
                placeholder="Type your answer here..."
                value={answer}
                onChange={(e) => setAnswer(e.target.value)}
                className="min-h-[120px] input-brutalist"
                required
              />
            ) : (
              <Input
                type="text"
                placeholder="Type your answer here..."
                value={answer}
                onChange={(e) => setAnswer(e.target.value)}
                className="input-brutalist"
                required
              />
            )}
          </CardContent>
          <CardFooter className="flex justify-between border-t-2 border-black">
            <Button type="button" variant="outline" onClick={handleSkip} className="btn-brutalist">
              Skip this prompt
            </Button>
            <Button type="submit" disabled={submitting} className="btn-brutalist">
              {submitting ? (
                <>
                  <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                  Submitting...
                </>
              ) : (
                "Submit Answer"
              )}
            </Button>
          </CardFooter>
        </form>
      </Card>
    </div>
  )
}
