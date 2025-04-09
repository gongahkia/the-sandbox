"use client"

import { CardFooter } from "@/components/ui/card"

import { useState } from "react"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Button } from "@/components/ui/button"
import { Progress } from "@/components/ui/progress"
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs"
import { Play, Pause, Clock, Database, AlertCircle } from "lucide-react"

export default function ScraperStatus() {
  const [isRunning, setIsRunning] = useState(false)
  const [progress, setProgress] = useState(0)

  const toggleScraper = () => {
    setIsRunning(!isRunning)
    if (!isRunning) {
      // Simulate progress
      setProgress(0)
      const interval = setInterval(() => {
        setProgress((prev) => {
          if (prev >= 100) {
            clearInterval(interval)
            setIsRunning(false)
            return 100
          }
          return prev + 5
        })
      }, 500)
    }
  }

  return (
    <Card>
      <CardHeader className="pb-3">
        <div className="flex items-center justify-between">
          <div>
            <CardTitle>Scraper Status</CardTitle>
            <CardDescription>Monitor and control the data scraping process</CardDescription>
          </div>
          <Button onClick={toggleScraper} variant={isRunning ? "destructive" : "default"}>
            {isRunning ? (
              <>
                <Pause className="mr-2 h-4 w-4" />
                Stop Scraper
              </>
            ) : (
              <>
                <Play className="mr-2 h-4 w-4" />
                Start Scraper
              </>
            )}
          </Button>
        </div>
      </CardHeader>
      <CardContent>
        <Tabs defaultValue="overview">
          <TabsList className="mb-4">
            <TabsTrigger value="overview">Overview</TabsTrigger>
            <TabsTrigger value="logs">Logs</TabsTrigger>
          </TabsList>

          <TabsContent value="overview">
            <div className="grid gap-4 md:grid-cols-3">
              <div className="flex flex-col gap-1">
                <div className="text-sm font-medium text-muted-foreground">Status</div>
                <div className="flex items-center gap-2">
                  <div className={`h-2 w-2 rounded-full ${isRunning ? "bg-green-500" : "bg-gray-300"}`}></div>
                  <span className="font-medium">{isRunning ? "Running" : "Idle"}</span>
                </div>
              </div>

              <div className="flex flex-col gap-1">
                <div className="text-sm font-medium text-muted-foreground">Last Run</div>
                <div className="flex items-center gap-2">
                  <Clock className="h-4 w-4 text-muted-foreground" />
                  <span>2 hours ago</span>
                </div>
              </div>

              <div className="flex flex-col gap-1">
                <div className="text-sm font-medium text-muted-foreground">Records</div>
                <div className="flex items-center gap-2">
                  <Database className="h-4 w-4 text-muted-foreground" />
                  <span>1,245 total</span>
                </div>
              </div>
            </div>

            {isRunning && (
              <div className="mt-4">
                <div className="flex items-center justify-between mb-2">
                  <span className="text-sm font-medium">Current Progress</span>
                  <span className="text-sm text-muted-foreground">{progress}%</span>
                </div>
                <Progress value={progress} className="h-2" />
              </div>
            )}
          </TabsContent>

          <TabsContent value="logs">
            <div className="rounded-md border bg-muted/50 p-4 font-mono text-sm h-[200px] overflow-auto">
              {isRunning ? (
                <>
                  <p className="text-green-500">[INFO] Scraper started at {new Date().toLocaleTimeString()}</p>
                  <p>[INFO] Connecting to IPOS Trademarks database...</p>
                  <p>[INFO] Successfully connected to IPOS Trademarks</p>
                  <p>[INFO] Fetching recent trademark applications...</p>
                  <p>[INFO] Retrieved 25 new trademark records</p>
                  <p>[INFO] Connecting to IPOS Patents database...</p>
                  <p>[INFO] Successfully connected to IPOS Patents</p>
                  <p>[INFO] Fetching recent patent applications...</p>
                </>
              ) : (
                <>
                  <p className="text-yellow-500">[INFO] Scraper idle</p>
                  <p>[INFO] Last run completed at 07:30:15</p>
                  <p>[INFO] Retrieved 25 trademark records</p>
                  <p>[INFO] Retrieved 18 patent records</p>
                  <p>[INFO] Retrieved 12 design records</p>
                  <p>[INFO] Total records in database: 1,245</p>
                  <p>[INFO] Next scheduled run: 11:30:00</p>
                </>
              )}
            </div>
          </TabsContent>
        </Tabs>
      </CardContent>
      <CardFooter className="border-t pt-4">
        <div className="flex items-center text-sm text-muted-foreground">
          <AlertCircle className="mr-2 h-4 w-4" />
          <p>Data is scraped for educational purposes only. Use responsibly and respect IPOS terms of service.</p>
        </div>
      </CardFooter>
    </Card>
  )
}