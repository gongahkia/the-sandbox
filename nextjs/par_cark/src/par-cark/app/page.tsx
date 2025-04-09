import CarparkGrid from "@/components/carpark-grid"
import { getCarparkData } from "@/lib/api"
import { Suspense } from "react"
import { Loader2 } from "lucide-react"

export default async function Home() {
  return (
    <main className="min-h-screen p-8 bg-gray-100">
      <h1 className="text-4xl font-bold mb-8 text-center text-gray-800">Singapore Carpark Availability</h1>
      <div className="max-w-7xl mx-auto">
        <Suspense fallback={<LoadingSpinner />}>
          <CarparkContent />
        </Suspense>
      </div>
    </main>
  )
}

async function CarparkContent() {
  const carparkData = await getCarparkData()
  return <CarparkGrid data={carparkData} />
}

function LoadingSpinner() {
  return (
    <div className="flex justify-center items-center h-64">
      <Loader2 className="w-12 h-12 animate-spin text-gray-600" />
    </div>
  )
}