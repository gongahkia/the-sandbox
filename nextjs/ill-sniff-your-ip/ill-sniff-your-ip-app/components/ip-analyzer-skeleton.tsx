import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Skeleton } from "@/components/ui/skeleton"
import { Terminal } from "lucide-react"

export default function IPAnalyzerSkeleton() {
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
            <Skeleton className="h-10 flex-1 bg-[#161c2c]" />
            <Skeleton className="h-10 w-24 bg-[#161c2c]" />
          </div>
        </CardContent>
      </Card>

      <Card className="border border-[#1e2738] bg-[#0d121f]">
        <CardHeader className="border-b border-[#1e2738]">
          <Skeleton className="h-6 w-48 bg-[#161c2c]" />
        </CardHeader>
        <CardContent className="pt-6">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div className="space-y-4">
              <div className="p-3 bg-[#161c2c] rounded-md border border-[#1e2738]">
                <Skeleton className="h-4 w-24 mb-2 bg-[#1e2738]" />
                <Skeleton className="h-6 w-32 bg-[#1e2738]" />
              </div>

              <div className="p-3 bg-[#161c2c] rounded-md border border-[#1e2738]">
                <Skeleton className="h-4 w-24 mb-2 bg-[#1e2738]" />
                <div className="flex gap-2">
                  <Skeleton className="h-6 w-20 bg-[#1e2738]" />
                  <Skeleton className="h-6 w-12 bg-[#1e2738]" />
                </div>
              </div>

              <div className="p-3 bg-[#161c2c] rounded-md border border-[#1e2738]">
                <Skeleton className="h-4 w-24 mb-2 bg-[#1e2738]" />
                <Skeleton className="h-6 w-40 bg-[#1e2738]" />
              </div>
            </div>

            <div className="space-y-4">
              <div className="p-3 bg-[#161c2c] rounded-md border border-[#1e2738]">
                <Skeleton className="h-4 w-24 mb-2 bg-[#1e2738]" />
                <Skeleton className="h-6 w-36 bg-[#1e2738]" />
              </div>

              <div className="p-3 bg-[#161c2c] rounded-md border border-[#1e2738]">
                <Skeleton className="h-4 w-24 mb-2 bg-[#1e2738]" />
                <Skeleton className="h-6 w-48 bg-[#1e2738]" />
              </div>

              <div className="p-3 bg-[#161c2c] rounded-md border border-[#1e2738]">
                <Skeleton className="h-4 w-24 mb-2 bg-[#1e2738]" />
                <Skeleton className="h-6 w-40 bg-[#1e2738]" />
              </div>
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  )
}