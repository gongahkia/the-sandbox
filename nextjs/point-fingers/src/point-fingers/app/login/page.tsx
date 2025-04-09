"use client"

import type React from "react"

import { useState } from "react"
import Link from "next/link"
import { useRouter } from "next/navigation"
import { createClient } from "@/utils/supabase/client"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card"
import { Input } from "@/components/ui/input"
import { Label } from "@/components/ui/label"
import { toast } from "@/components/ui/use-toast"

export default function LoginPage() {
  const [email, setEmail] = useState("")
  const [password, setPassword] = useState("")
  const [loading, setLoading] = useState(false)
  const router = useRouter()
  const supabase = createClient()

  const handleLogin = async (e: React.FormEvent) => {
    e.preventDefault()
    setLoading(true)

    try {
      // Check if we're at capacity first
      const { data: userCount, error: countError } = await supabase.from("active_users").select("count").single()

      if (countError) {
        throw countError
      }

      if (userCount && userCount.count >= 20) {
        toast({
          title: "Maximum users reached",
          description: "Please try again later. We limit to 20 concurrent users to avoid overloading our servers.",
          variant: "destructive",
        })
        setLoading(false)
        return
      }

      const { error } = await supabase.auth.signInWithPassword({
        email,
        password,
      })

      if (error) {
        throw error
      }

      router.push("/dashboard")
      router.refresh()
    } catch (error: any) {
      toast({
        title: "Login failed",
        description: error.message || "Please check your credentials and try again.",
        variant: "destructive",
      })
    } finally {
      setLoading(false)
    }
  }

  return (
    <div className="container flex h-screen w-screen flex-col items-center justify-center bg-white">
      <Link
        href="/"
        className="absolute left-4 top-4 md:left-8 md:top-8 border-2 border-black px-4 py-2 hover:bg-black hover:text-white transition-colors"
      >
        Back
      </Link>
      <Card className="w-full max-w-md card-brutalist">
        <CardHeader className="space-y-1 border-b-2 border-black">
          <CardTitle className="text-2xl uppercase">Login</CardTitle>
          <CardDescription className="text-black">Enter your email and password to access your account</CardDescription>
        </CardHeader>
        <form onSubmit={handleLogin}>
          <CardContent className="grid gap-4 pt-6">
            <div className="grid gap-2">
              <Label htmlFor="email" className="uppercase">
                Email
              </Label>
              <Input
                id="email"
                type="email"
                placeholder="m@example.com"
                value={email}
                onChange={(e) => setEmail(e.target.value)}
                required
                className="input-brutalist"
              />
            </div>
            <div className="grid gap-2">
              <Label htmlFor="password" className="uppercase">
                Password
              </Label>
              <Input
                id="password"
                type="password"
                value={password}
                onChange={(e) => setPassword(e.target.value)}
                required
                className="input-brutalist"
              />
            </div>
          </CardContent>
          <CardFooter className="flex flex-col border-t-2 border-black">
            <Button className="w-full btn-brutalist" type="submit" disabled={loading}>
              {loading ? "Logging in..." : "Login"}
            </Button>
            <p className="mt-4 text-center text-sm">
              Don&apos;t have an account?{" "}
              <Link href="/signup" className="text-black font-bold underline">
                Sign up
              </Link>
            </p>
          </CardFooter>
        </form>
      </Card>
    </div>
  )
}