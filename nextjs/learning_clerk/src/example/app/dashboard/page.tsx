import { auth } from "@clerk/nextjs"
import { redirect } from "next/navigation"
import Image from "next/image"

export default async function Dashboard() {
  const { userId } = await auth()

  if (!userId) {
    redirect("/sign-in")
  }

  const bottles = Array(6).fill(null)

  return (
    <div className="min-h-screen bg-gray-100 p-8">
      <h1 className="text-3xl font-bold mb-8 text-center">Nalgene Dashboard</h1>
      <div className="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-3 gap-8">
        {bottles.map((_, index) => (
          <div key={index} className="bg-white p-6 rounded-lg shadow-md flex items-center justify-center">
            <div className="animate-spin">
              <Image src="/nalgene-bottle.png" alt="Spinning Nalgene Bottle" width={150} height={150} />
            </div>
          </div>
        ))}
      </div>
    </div>
  )
}