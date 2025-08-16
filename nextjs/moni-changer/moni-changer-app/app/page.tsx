import CurrencyConverter from "@/components/currency-converter"

export default function Home() {
  return (
    <main className="min-h-screen bg-gradient-to-br from-violet-900 via-purple-800 to-indigo-900 p-4 md:p-8 flex items-center justify-center">
      <CurrencyConverter />
    </main>
  )
}