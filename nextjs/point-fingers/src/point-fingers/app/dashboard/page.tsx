"use client"

import { useState, useEffect } from "react"
import Link from "next/link"
import { useRouter } from "next/navigation"
import { createClient } from "@/utils/supabase/client"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card"
import { toast } from "@/components/ui/use-toast"
import { Loader2, LogOut } from "lucide-react"

export default function DashboardPage() {
  const [loading, setLoading] = useState(true)
  const [user, setUser] = useState<any>(null)
  const [recentResponses, setRecentResponses] = useState<any[]>([])
  const [stats, setStats] = useState<any>(null)
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
      fetchUserData(session.user.id)
    }

    checkAuth()
  }, [router, supabase])

  const fetchUserData = async (userId: string) => {
    setLoading(true)
    try {
      // Fetch user's recent responses
      const responsesResponse = await fetch(`/api/user/${userId}/responses?limit=5`)
      if (responsesResponse.ok) {
        const responsesData = await responsesResponse.json()
        setRecentResponses(responsesData)
      }

      // Fetch user stats
      const statsResponse = await fetch(`/api/user/${userId}/stats`)
      if (statsResponse.ok) {
        const statsData = await statsResponse.json()
        setStats(statsData)
      }
    } catch (error) {
      toast({
        title: "Error",
        description: "Failed to load your data. Please try again.",
        variant: "destructive",
      })
    } finally {
      setLoading(false)
    }
  }

  const handleLogout = async () => {
    await supabase.auth.signOut()
    router.push("/")
    router.refresh()
  }

  if (loading) {
    return (
      <div className="container flex h-screen items-center justify-center bg-white">
        <Loader2 className="h-8 w-8 animate-spin text-black" />
      </div>
    )
  }

  return (
    <div className="container py-10 bg-white">
      <div className="flex items-center justify-between mb-8">
        <div>
          <h1 className="text-3xl font-bold uppercase">Dashboard 👉</h1>
          <p className="text-black">Welcome back, {user?.user_metadata?.username || "User"}</p>
        </div>
        <Button variant="outline" onClick={handleLogout} className="btn-brutalist">
          <LogOut className="mr-2 h-4 w-4" />
          Logout
        </Button>
      </div>

      <div className="grid gap-6 md:grid-cols-2 lg:grid-cols-4 mb-8">
        <Card className="card-brutalist">
          <CardHeader className="pb-2 border-b-2 border-black">
            <CardTitle className="text-sm font-medium uppercase">Total Prompts Answered</CardTitle>
          </CardHeader>
          <CardContent className="pt-4">
            <div className="text-3xl font-bold">{stats?.totalAnswered || 0}</div>
          </CardContent>
        </Card>
        <Card className="card-brutalist">
          <CardHeader className="pb-2 border-b-2 border-black">
            <CardTitle className="text-sm font-medium uppercase">Average Uniqueness</CardTitle>
          </CardHeader>
          <CardContent className="pt-4">
            <div className="text-3xl font-bold">{stats?.averageUniqueness?.toFixed(1) || 0}%</div>
          </CardContent>
        </Card>
        <Card className="card-brutalist">
          <CardHeader className="pb-2 border-b-2 border-black">
            <CardTitle className="text-sm font-medium uppercase">Weirdness Rank</CardTitle>
          </CardHeader>
          <CardContent className="pt-4">
            <div className="text-3xl font-bold">{stats?.weirdnessRank || "N/A"}</div>
          </CardContent>
        </Card>
        <Card className="card-brutalist">
          <CardHeader className="pb-2 border-b-2 border-black">
            <CardTitle className="text-sm font-medium uppercase">Most Unique Category</CardTitle>
          </CardHeader>
          <CardContent className="pt-4">
            <div className="text-3xl font-bold">{stats?.mostUniqueCategory || "N/A"}</div>
          </CardContent>
        </Card>
      </div>

      <div className="grid gap-6 md:grid-cols-3">
        <Card className="md:col-span-2 card-brutalist">
          <CardHeader className="border-b-2 border-black">
            <CardTitle className="uppercase">Recent Responses</CardTitle>
            <CardDescription className="text-black">Your most recent prompt answers</CardDescription>
          </CardHeader>
          <CardContent className="pt-6">
            {recentResponses.length > 0 ? (
              <div className="space-y-4">
                {recentResponses.map((response) => (
                  <div key={response.id} className="border-b-2 border-black pb-4 last:border-0">
                    <h3 className="font-medium uppercase">{response.prompt.text}</h3>
                    <p className="text-sm text-black">Your answer: {response.answer}</p>
                    <div className="mt-2 flex items-center text-xs">
                      <span className="text-black">{new Date(response.created_at).toLocaleDateString()}</span>
                      <span className="mx-2">•</span>
                      <span className={response.percentile > 70 ? "text-black font-bold" : "text-black"}>
                        {response.percentile}% unique
                      </span>
                      <Link href={`/results/${response.prompt_id}`} className="ml-auto text-black font-bold underline">
                        View Results
                      </Link>
                    </div>
                  </div>
                ))}
              </div>
            ) : (
              <div className="flex h-40 items-center justify-center">
                <p className="text-black">You haven't answered any prompts yet</p>
              </div>
            )}
          </CardContent>
          <CardFooter className="border-t-2 border-black">
            <Button asChild className="btn-brutalist">
              <Link href="/prompts">Answer New Prompt</Link>
            </Button>
          </CardFooter>
        </Card>

        <Card className="card-brutalist">
          <CardHeader className="border-b-2 border-black">
            <CardTitle className="uppercase">Your Weirdness Score</CardTitle>
            <CardDescription className="text-black">How unique are your responses?</CardDescription>
          </CardHeader>
          <CardContent className="flex flex-col items-center justify-center pt-6">
            <div className="relative h-40 w-40">
              <div className="absolute inset-0 flex items-center justify-center">
                <div className="text-4xl font-bold">{stats?.weirdnessScore || 0}%</div>
              </div>
              <svg className="h-full w-full" viewBox="0 0 100 100">
                <circle
                  cx="50"
                  cy="50"
                  r="45"
                  fill="none"
                  stroke="black"
                  strokeWidth="10"
                  strokeLinecap="square"
                  strokeDasharray={`${(stats?.weirdnessScore || 0) * 2.83} 283`}
                  strokeDashoffset="0"
                  transform="rotate(-90 50 50)"
                />
                <circle cx="50" cy="50" r="45" fill="none" stroke="black" strokeWidth="10" strokeOpacity="0.2" />
              </svg>
            </div>
            <p className="mt-4 text-center text-sm uppercase font-bold">
              {stats?.weirdnessScore >= 80
                ? "You're extremely unique! 👉"
                : stats?.weirdnessScore >= 60
                  ? "You're quite unique! 👉"
                  : stats?.weirdnessScore >= 40
                    ? "You have a balanced mix 👉"
                    : "You're pretty normal 👉"}
            </p>
          </CardContent>
          <CardFooter className="border-t-2 border-black">
            <Button variant="outline" className="w-full btn-brutalist" asChild>
              <Link href="/leaderboard">View Leaderboard</Link>
            </Button>
          </CardFooter>
        </Card>
      </div>
      <footer className="mt-10 pt-6 border-t-2 border-black">
        <p className="text-center text-sm">
          Made with ❤️ by{" "}
          <a
            href="https://github.com/gongahkia"
            target="_blank"
            rel="noopener noreferrer"
            className="underline font-bold"
          >
            Gabriel Ong
          </a>
        </p>
      </footer>
    </div>
  )
}