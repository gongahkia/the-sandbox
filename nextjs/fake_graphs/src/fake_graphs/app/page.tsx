import RandomCorrelationGraph from "../components/RandomCorrelationGraph"

export default function Home() {
  return (
    <main className="p-4 md:p-10 mx-auto max-w-7xl">
      <h1 className="text-3xl font-bold mb-4">Fake graphs </h1>
      <p className="mb-4">
        These are all fake graphs.
        </p>
      <RandomCorrelationGraph />
    </main>
  )
}