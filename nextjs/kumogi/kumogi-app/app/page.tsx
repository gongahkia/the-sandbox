import GungiGame from "@/components/gungi-game"

export default function Home() {
  return (
    <main className="flex min-h-screen flex-col items-center justify-between p-4 md:p-24">
      <div className="z-10 max-w-5xl w-full items-center justify-center font-mono text-sm">
        <h1 className="text-4xl font-bold text-center mb-8">Gungi</h1>
        <p className="text-center mb-8 text-muted-foreground">The strategic board game from Hunter x Hunter</p>
        <GungiGame />
      </div>
    </main>
  )
}