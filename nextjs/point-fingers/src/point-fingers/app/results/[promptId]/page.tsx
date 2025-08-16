"use client"

import { CardDescription } from "@/components/ui/card"

import { useState, useEffect } from "react"
import { useParams, useRouter } from "next/navigation"
import { createClient } from "@/utils/supabase/client"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardHeader, CardTitle, CardFooter } from "@/components/ui/card"
import { toast } from "@/components/ui/use-toast"
import { Loader2 } from "lucide-react"
import { DistributionChart } from "@/components/distribution-chart"

export default function ResultsPage() {
  const params = useParams()
  const promptId = params.promptId as string
  const [loading, setLoading] = useState(true)
  const [prompt, setPrompt] = useState<any>(null)
  const [results, setResults] = useState<any>(null)
  const [userResult, setUserResult] = useState<any>(null)
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

      fetchResults(session.user.id)
    }

    checkAuth()
  }, [supabase, router])

  const fetchResults = async (userId: string) => {
    setLoading(true)
    try {
      // Fetch the prompt details
      const promptResponse = await fetch(`/api/prompts/${promptId}`)
      if (!promptResponse.ok) throw new Error("Failed to fetch prompt")
      const promptData = await promptResponse.json()
      setPrompt(promptData)

      // Fetch the distribution results
      const resultsResponse = await fetch(`/api/results/${promptId}`)
      if (!resultsResponse.ok) throw new Error("Failed to fetch results")
      const resultsData = await resultsResponse.json()
      setResults(resultsData)

      // Fetch the user's specific result
      const userResultResponse = await fetch(`/api/results/${promptId}/user/${userId}`)
      if (userResultResponse.ok) {
        const userResultData = await userResultResponse.json()
        setUserResult(userResultData)
      }
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to load results. Please try again.",
        variant: "destructive",
      })
    } finally {
      setLoading(false)
    }
  }

  const handleTryAnother = () => {
    router.push("/prompts")
  }

  if (loading) {
    return (
      <div className="container flex h-screen items-center justify-center bg-white">
        <Loader2 className="h-8 w-8 animate-spin text-black" />
      </div>
    )
  }

  return (
    <div className="container max-w-3xl py-10 bg-white">
      <Card className="card-brutalist">
        <CardHeader className="border-b-2 border-black">
          <CardTitle className="uppercase">Results: {prompt?.text || "Loading..."}</CardTitle>
          <CardDescription className="text-black">See how your answer compares to others</CardDescription>
        </CardHeader>
        <CardContent className="space-y-6 pt-6">
          {userResult && (
            <div className="rounded-none bg-black text-white p-4">
              <h3 className="text-lg font-medium uppercase">Your answer:</h3>
              <p className="mt-2">{userResult.answer}</p>
              <div className="mt-4">
                <p className="text-sm font-medium">
                  Your answer is <span className="font-bold">{userResult.percentile}%</span> different from the average
                  response.
                </p>
                {userResult.percentile > 80 && (
                  <p className="text-sm mt-1">You're quite unique! Your answer is very different from most people.</p>
                )}
                {userResult.percentile > 50 && userResult.percentile <= 80 && (
                  <p className="text-sm mt-1">You're somewhat different from the average person.</p>
                )}
                {userResult.percentile <= 50 && (
                  <p className="text-sm mt-1">Your answer is fairly common and similar to many others.</p>
                )}
              </div>
            </div>
          )}

          <div className="h-64 w-full border-2 border-black">
            <h3 className="text-lg font-medium mb-4 uppercase">Distribution of Responses</h3>
            {results ? (
              <DistributionChart data={results.distribution} userPosition={userResult?.position} />
            ) : (
              <div className="flex h-full items-center justify-center">
                <p className="text-muted-foreground">No data available yet</p>
              </div>
            )}
          </div>

          <div>
            <h3 className="text-lg font-medium mb-2 uppercase">Statistics</h3>
            <ul className="grid grid-cols-2 gap-4 sm:grid-cols-4">
              <li className="flex flex-col items-center justify-center border-2 border-black p-4">
                <span className="text-sm text-black uppercase">Total Responses</span>
                <span className="text-2xl font-bold">{results?.totalResponses || 0}</span>
              </li>
              <li className="flex flex-col items-center justify-center border-2 border-black p-4">
                <span className="text-sm text-black uppercase">Average Score</span>
                <span className="text-2xl font-bold">{results?.averageScore?.toFixed(1) || 0}</span>
              </li>
              <li className="flex flex-col items-center justify-center border-2 border-black p-4">
                <span className="text-sm text-black uppercase">Most Common</span>
                <span className="text-2xl font-bold truncate max-w-full" title={results?.mostCommon || "N/A"}>
                  {results?.mostCommon || "N/A"}
                </span>
              </li>
              <li className="flex flex-col items-center justify-center border-2 border-black p-4">
                <span className="text-sm text-black uppercase">Most Unique</span>
                <span className="text-2xl font-bold truncate max-w-full" title={results?.mostUnique || "N/A"}>
                  {results?.mostUnique || "N/A"}
                </span>
              </li>
            </ul>
          </div>
        </CardContent>
        <CardFooter className="flex justify-between border-t-2 border-black">
          <Button variant="outline" onClick={() => router.push("/dashboard")} className="btn-brutalist">
            Back to Dashboard
          </Button>
          <Button onClick={handleTryAnother} className="btn-brutalist">
            Try Another Prompt
          </Button>
        </CardFooter>
      </Card>
    </div>
  )
}