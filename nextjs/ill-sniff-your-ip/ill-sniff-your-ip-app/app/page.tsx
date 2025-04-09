import { Suspense } from "react"
import IPAnalyzer from "@/components/ip-analyzer"
import IPAnalyzerSkeleton from "@/components/ip-analyzer-skeleton"

export default function Home() {
  return (
    <main className="flex min-h-screen flex-col items-center justify-between p-4 md:p-24 bg-[#0a0e17] text-[#e0e4eb]">
      <div className="w-full max-w-5xl relative">
        <div className="absolute inset-0 bg-[url('/grid-pattern.svg')] bg-center opacity-5 pointer-events-none"></div>

        <div className="relative">
          <div className="flex items-center justify-center mb-2">
            <div className="h-1 w-16 bg-[#00ff9d] mr-4"></div>
            <h1 className="text-4xl font-bold text-center font-mono tracking-tight text-[#00ff9d]">
              I'LL SNIFF YOUR IP!
            </h1>
            <div className="h-1 w-16 bg-[#00ff9d] ml-4"></div>
          </div>

          <p className="text-center text-[#8a94a8] mb-12 font-mono text-sm">
            &lt;SYSTEM:READY&gt; // NETWORK RECONNAISSANCE AND INTELLIGENCE GATHERING TOOL
          </p>

          <Suspense fallback={<IPAnalyzerSkeleton />}>
            <IPAnalyzer />
          </Suspense>
        </div>
      </div>
    </main>
  )
}