import dynamic from "next/dynamic"
import Header from "@/components/Header"
import Footer from "@/components/Footer"

const TrafficGrid = dynamic(() => import("@/components/TrafficGrid"), {
  ssr: false,
})

export default function Home() {
  return (
    <main className="min-h-screen bg-gray-100 p-4">
      <Header />
      <TrafficGrid />
      <Footer />
    </main>
  )
}