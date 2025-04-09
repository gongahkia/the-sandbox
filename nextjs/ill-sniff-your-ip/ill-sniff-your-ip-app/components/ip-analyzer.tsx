"use client"

import type React from "react"

import { useState, useEffect } from "react"
import { MapPin, Globe, Shield, Cpu, Wifi, AlertTriangle, Terminal } from "lucide-react"
import { Card, CardContent, CardDescription, CardHeader, CardTitle, CardFooter } from "@/components/ui/card"
import { Input } from "@/components/ui/input"
import { Button } from "@/components/ui/button"
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs"
import { Alert, AlertDescription, AlertTitle } from "@/components/ui/alert"
import { Badge } from "@/components/ui/badge"
import { Progress } from "@/components/ui/progress"
import IPMap from "@/components/ip-map"

export default function IPAnalyzer() {
  const [ipAddress, setIpAddress] = useState("")
  const [ipData, setIpData] = useState<any>(null)
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const [scanProgress, setScanProgress] = useState(0)

  useEffect(() => {
    if (loading) {
      const interval = setInterval(() => {
        setScanProgress((prev) => {
          if (prev >= 100) {
            clearInterval(interval)
            return 100
          }
          return prev + 5
        })
      }, 100)

      return () => {
        clearInterval(interval)
        setScanProgress(0)
      }
    }
  }, [loading])

  const analyzeIP = async () => {
    if (!ipAddress) {
      setError("Please enter an IP address")
      return
    }

    try {
      setLoading(true)
      setError(null)
      setScanProgress(0)

      // Simulate a more technical scanning process
      await new Promise((resolve) => setTimeout(resolve, 2000))

      const response = await fetch(`/api/ip-info?ip=${ipAddress}`)

      if (!response.ok) {
        throw new Error("Failed to fetch IP information")
      }

      const data = await response.json()
      setIpData(data)
    } catch (err) {
      setError("Error fetching IP information. Please try again.")
      console.error(err)
    } finally {
      setLoading(false)
      setScanProgress(100)
    }
  }

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === "Enter") {
      analyzeIP()
    }
  }

  return (
    <div className="space-y-6">
      <Card className="border border-[#1e2738] bg-[#0d121f] shadow-[0_0_15px_rgba(0,255,157,0.1)]">
        <CardHeader className="border-b border-[#1e2738]">
          <CardTitle className="flex items-center gap-2 text-[#00ff9d] font-mono">
            <Terminal className="h-5 w-5" />
            IP:SNIFFER
          </CardTitle>
          <CardDescription className="text-[#8a94a8] font-mono text-xs">
            &gt; ENTER TARGET IP ADDRESS TO INITIATE NETWORK RECONNAISSANCE
          </CardDescription>
        </CardHeader>
        <CardContent className="pt-6">
          <div className="flex gap-2">
            <div className="relative flex-1">
              <Input
                placeholder="Enter IP address (e.g., 8.8.8.8)"
                value={ipAddress}
                onChange={(e) => setIpAddress(e.target.value)}
                onKeyDown={handleKeyDown}
                className="bg-[#161c2c] border-[#1e2738] text-[#e0e4eb] font-mono placeholder:text-[#4a5568] focus-visible:ring-[#00ff9d] focus-visible:ring-opacity-50"
              />
              {ipAddress && (
                <div className="absolute right-3 top-1/2 -translate-y-1/2 flex items-center gap-1 text-xs text-[#8a94a8]">
                  <span className="inline-block h-2 w-2 rounded-full bg-[#00ff9d] animate-pulse"></span>
                  READY
                </div>
              )}
            </div>
            <Button
              onClick={analyzeIP}
              disabled={loading}
              className="bg-[#00ff9d] text-[#0a0e17] hover:bg-[#00cc7d] font-mono"
            >
              {loading ? "SCANNING..." : "ANALYZE"}
            </Button>
          </div>

          {loading && (
            <div className="mt-4 space-y-2">
              <div className="flex justify-between text-xs font-mono text-[#8a94a8]">
                <span>SCAN IN PROGRESS</span>
                <span>{scanProgress}%</span>
              </div>
              <Progress value={scanProgress} className="h-1 bg-[#1e2738]" indicatorClassName="bg-[#00ff9d]" />
              <div className="text-xs font-mono text-[#8a94a8] animate-pulse">&gt; ANALYZING NETWORK PACKETS...</div>
            </div>
          )}

          {error && (
            <Alert variant="destructive" className="mt-4 bg-[#2d1215] border-[#ff3333] text-[#ff6666]">
              <AlertTriangle className="h-4 w-4" />
              <AlertTitle className="font-mono">ERROR:SCAN_FAILED</AlertTitle>
              <AlertDescription className="font-mono text-xs">{error}</AlertDescription>
            </Alert>
          )}
        </CardContent>
      </Card>

      {ipData && (
        <Tabs defaultValue="overview" className="w-full">
          <TabsList className="grid w-full grid-cols-3 bg-[#161c2c] p-1">
            <TabsTrigger
              value="overview"
              className="font-mono text-xs data-[state=active]:bg-[#0d121f] data-[state=active]:text-[#00ff9d]"
            >
              OVERVIEW
            </TabsTrigger>
            <TabsTrigger
              value="technical"
              className="font-mono text-xs data-[state=active]:bg-[#0d121f] data-[state=active]:text-[#00ff9d]"
            >
              TECHNICAL
            </TabsTrigger>
            <TabsTrigger
              value="map"
              className="font-mono text-xs data-[state=active]:bg-[#0d121f] data-[state=active]:text-[#00ff9d]"
            >
              GEO:MAP
            </TabsTrigger>
          </TabsList>

          <TabsContent value="overview" className="mt-4">
            <Card className="border border-[#1e2738] bg-[#0d121f] overflow-hidden">
              <CardHeader className="border-b border-[#1e2738]">
                <CardTitle className="flex items-center gap-2 text-[#00ff9d] font-mono">
                  <Globe className="h-5 w-5" />
                  GEO:LOCATION
                </CardTitle>
              </CardHeader>
              <CardContent className="pt-6">
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  <div className="space-y-4">
                    <div className="p-3 bg-[#161c2c] rounded-md border border-[#1e2738]">
                      <h3 className="text-xs font-mono text-[#8a94a8] mb-1">IP_ADDRESS</h3>
                      <p className="text-lg font-mono font-semibold text-[#e0e4eb]">{ipData.ip}</p>
                    </div>

                    <div className="p-3 bg-[#161c2c] rounded-md border border-[#1e2738]">
                      <h3 className="text-xs font-mono text-[#8a94a8] mb-1">COUNTRY</h3>
                      <div className="flex items-center gap-2">
                        <Badge variant="outline" className="bg-[#1e2738] text-[#00ff9d] border-[#00ff9d] font-mono">
                          {ipData.country}
                        </Badge>
                        <Badge className="bg-[#00ff9d] text-[#0a0e17] font-mono">{ipData.countryCode}</Badge>
                      </div>
                    </div>

                    <div className="p-3 bg-[#161c2c] rounded-md border border-[#1e2738]">
                      <h3 className="text-xs font-mono text-[#8a94a8] mb-1">REGION</h3>
                      <p className="font-mono text-[#e0e4eb]">{ipData.region || "UNKNOWN"}</p>
                    </div>
                  </div>

                  <div className="space-y-4">
                    <div className="p-3 bg-[#161c2c] rounded-md border border-[#1e2738]">
                      <h3 className="text-xs font-mono text-[#8a94a8] mb-1">CITY</h3>
                      <p className="font-mono text-[#e0e4eb]">{ipData.city || "UNKNOWN"}</p>
                    </div>

                    <div className="p-3 bg-[#161c2c] rounded-md border border-[#1e2738]">
                      <h3 className="text-xs font-mono text-[#8a94a8] mb-1">TIMEZONE</h3>
                      <p className="font-mono text-[#e0e4eb]">{ipData.timezone || "UNKNOWN"}</p>
                    </div>

                    <div className="p-3 bg-[#161c2c] rounded-md border border-[#1e2738]">
                      <h3 className="text-xs font-mono text-[#8a94a8] mb-1">COORDINATES</h3>
                      <div className="flex items-center gap-2">
                        <MapPin className="h-4 w-4 text-[#00ff9d]" />
                        <p className="font-mono text-[#e0e4eb]">
                          {ipData.latitude}, {ipData.longitude}
                        </p>
                      </div>
                    </div>
                  </div>
                </div>
              </CardContent>
              <CardFooter className="border-t border-[#1e2738] px-6 py-3">
                <div className="w-full flex justify-between items-center text-xs font-mono text-[#8a94a8]">
                  <span>DATA_CONFIDENCE: HIGH</span>
                  <span className="flex items-center gap-1">
                    <span className="inline-block h-2 w-2 rounded-full bg-[#00ff9d]"></span>
                    SCAN_COMPLETE
                  </span>
                </div>
              </CardFooter>
            </Card>
          </TabsContent>

          <TabsContent value="technical" className="mt-4">
            <Card className="border border-[#1e2738] bg-[#0d121f] overflow-hidden">
              <CardHeader className="border-b border-[#1e2738]">
                <CardTitle className="flex items-center gap-2 text-[#00ff9d] font-mono">
                  <Cpu className="h-5 w-5" />
                  NET:INTELLIGENCE
                </CardTitle>
              </CardHeader>
              <CardContent className="pt-6">
                <div className="space-y-4">
                  <div className="p-3 bg-[#161c2c] rounded-md border border-[#1e2738]">
                    <div className="flex justify-between">
                      <h3 className="text-xs font-mono text-[#8a94a8] mb-1">ISP</h3>
                      <Badge
                        variant="outline"
                        className="bg-transparent border-[#1e2738] text-[#8a94a8] font-mono text-[10px]"
                      >
                        PROVIDER
                      </Badge>
                    </div>
                    <p className="font-mono text-[#e0e4eb]">{ipData.isp || "UNKNOWN"}</p>
                  </div>

                  <div className="p-3 bg-[#161c2c] rounded-md border border-[#1e2738]">
                    <div className="flex justify-between">
                      <h3 className="text-xs font-mono text-[#8a94a8] mb-1">ORGANIZATION</h3>
                      <Badge
                        variant="outline"
                        className="bg-transparent border-[#1e2738] text-[#8a94a8] font-mono text-[10px]"
                      >
                        ENTITY
                      </Badge>
                    </div>
                    <p className="font-mono text-[#e0e4eb]">{ipData.org || "UNKNOWN"}</p>
                  </div>

                  <div className="p-3 bg-[#161c2c] rounded-md border border-[#1e2738]">
                    <div className="flex justify-between">
                      <h3 className="text-xs font-mono text-[#8a94a8] mb-1">ASN</h3>
                      <Badge
                        variant="outline"
                        className="bg-transparent border-[#1e2738] text-[#8a94a8] font-mono text-[10px]"
                      >
                        NETWORK_ID
                      </Badge>
                    </div>
                    <p className="font-mono text-[#e0e4eb]">{ipData.asn || "UNKNOWN"}</p>
                  </div>

                  <div className="p-3 bg-[#161c2c] rounded-md border border-[#1e2738]">
                    <div className="flex justify-between">
                      <h3 className="text-xs font-mono text-[#8a94a8] mb-1">SECURITY_ASSESSMENT</h3>
                      <Badge
                        variant="outline"
                        className="bg-transparent border-[#1e2738] text-[#8a94a8] font-mono text-[10px]"
                      >
                        THREAT_INTEL
                      </Badge>
                    </div>
                    <div className="flex flex-wrap gap-2 mt-2">
                      {ipData.proxy && (
                        <Badge variant="destructive" className="bg-[#2d1215] text-[#ff6666] border-[#ff3333] font-mono">
                          PROXY_DETECTED
                        </Badge>
                      )}
                      {ipData.tor && (
                        <Badge variant="destructive" className="bg-[#2d1215] text-[#ff6666] border-[#ff3333] font-mono">
                          TOR_EXIT_NODE
                        </Badge>
                      )}
                      {ipData.vpn && (
                        <Badge variant="destructive" className="bg-[#2d1215] text-[#ff6666] border-[#ff3333] font-mono">
                          VPN_DETECTED
                        </Badge>
                      )}
                      {!ipData.proxy && !ipData.tor && !ipData.vpn && (
                        <Badge className="bg-[#0d2318] text-[#00ff9d] border-[#00cc7d] font-mono">
                          <Shield className="h-3 w-3 mr-1" /> CLEAN_IP
                        </Badge>
                      )}
                    </div>
                  </div>
                </div>
              </CardContent>
              <CardFooter className="border-t border-[#1e2738] px-6 py-3">
                <div className="w-full flex justify-between items-center text-xs font-mono text-[#8a94a8]">
                  <span>PACKETS_ANALYZED: 1,024</span>
                  <span className="flex items-center gap-1">
                    <Wifi className="h-3 w-3" />
                    NETWORK_STABLE
                  </span>
                </div>
              </CardFooter>
            </Card>
          </TabsContent>

          <TabsContent value="map" className="mt-4">
            <Card className="border border-[#1e2738] bg-[#0d121f] overflow-hidden">
              <CardHeader className="border-b border-[#1e2738]">
                <CardTitle className="flex items-center gap-2 text-[#00ff9d] font-mono">
                  <MapPin className="h-5 w-5" />
                  GEO:TRACKER
                </CardTitle>
              </CardHeader>
              <CardContent className="h-[400px] p-0">
                <IPMap latitude={ipData.latitude} longitude={ipData.longitude} />
              </CardContent>
              <CardFooter className="border-t border-[#1e2738] px-6 py-3">
                <div className="w-full flex justify-between items-center text-xs font-mono text-[#8a94a8]">
                  <span>
                    COORDINATES: {ipData.latitude.toFixed(4)}, {ipData.longitude.toFixed(4)}
                  </span>
                  <span className="flex items-center gap-1">
                    <span className="inline-block h-2 w-2 rounded-full bg-[#00ff9d] animate-ping"></span>
                    TRACKING_ACTIVE
                  </span>
                </div>
              </CardFooter>
            </Card>
          </TabsContent>
        </Tabs>
      )}
    </div>
  )
}