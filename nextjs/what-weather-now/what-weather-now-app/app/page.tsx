import WeatherDashboard from "@/components/weather-dashboard"

export const metadata = {
  title: "What Weather Now 🌦️",
  description: "A beautiful weather forecast application",
}

export default function Home() {
  return (
    <main className="min-h-screen bg-gradient-to-b from-slate-900 to-slate-800">
      <WeatherDashboard />
    </main>
  )
}