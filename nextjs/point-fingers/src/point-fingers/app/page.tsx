import Link from "next/link"
import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardFooter, CardHeader, CardTitle } from "@/components/ui/card"

export default function Home() {
  return (
    <div className="flex min-h-screen flex-col">
      <header className="sticky top-0 z-40 border-b-2 border-black bg-white">
        <div className="container flex h-16 items-center justify-between py-4">
          <div className="flex items-center gap-2">
            <h1 className="text-xl font-bold uppercase">Point Fingers 👉</h1>
          </div>
          <nav className="flex items-center gap-4">
            <Link href="/login">
              <Button variant="outline" className="btn-brutalist">
                Login
              </Button>
            </Link>
            <Link href="/signup">
              <Button className="btn-brutalist">Sign Up</Button>
            </Link>
          </nav>
        </div>
      </header>
      <main className="flex-1 bg-white">
        <section className="container grid items-center gap-6 pb-8 pt-6 md:py-10">
          <div className="flex max-w-[980px] flex-col items-center gap-2">
            <h1 className="text-center text-3xl font-extrabold leading-tight tracking-tighter uppercase md:text-4xl lg:text-5xl">
              Point Fingers 👉 at how <span className="bg-black text-white px-2">WEIRD</span> you are
            </h1>
            <p className="max-w-[700px] text-center text-black md:text-xl">
              Answer prompts. Compare with others. Find out who's the weirdo.
            </p>
          </div>
          <div className="grid gap-6 md:grid-cols-2 lg:grid-cols-3">
            <Card className="card-brutalist">
              <CardHeader className="border-b-2 border-black">
                <CardTitle className="uppercase">Answer Prompts</CardTitle>
                <CardDescription className="text-black">Respond to randomly generated questions</CardDescription>
              </CardHeader>
              <CardContent className="pt-6">
                <p>From your favorite music to your daily habits, share your preferences and see where you stand.</p>
              </CardContent>
              <CardFooter className="border-t-2 border-black">
                <Link href="/prompts" className="w-full">
                  <Button className="w-full btn-brutalist">Try a Prompt</Button>
                </Link>
              </CardFooter>
            </Card>
            <Card className="card-brutalist">
              <CardHeader className="border-b-2 border-black">
                <CardTitle className="uppercase">See Your Position</CardTitle>
                <CardDescription className="text-black">
                  Visualize where you fall on the normal distribution
                </CardDescription>
              </CardHeader>
              <CardContent className="pt-6">
                <p>Are you in the mainstream or out on the fringes? Our visualization shows exactly where you stand.</p>
              </CardContent>
              <CardFooter className="border-t-2 border-black">
                <Link href="/dashboard" className="w-full">
                  <Button variant="outline" className="w-full btn-brutalist">
                    View Dashboard
                  </Button>
                </Link>
              </CardFooter>
            </Card>
            <Card className="card-brutalist">
              <CardHeader className="border-b-2 border-black">
                <CardTitle className="uppercase">Compare with Friends</CardTitle>
                <CardDescription className="text-black">Invite friends and see who's the most unique</CardDescription>
              </CardHeader>
              <CardContent className="pt-6">
                <p>Create groups, invite friends, and discover who has the most unusual tastes in your circle.</p>
              </CardContent>
              <CardFooter className="border-t-2 border-black">
                <Link href="/groups" className="w-full">
                  <Button variant="outline" className="w-full btn-brutalist">
                    Create Group
                  </Button>
                </Link>
              </CardFooter>
            </Card>
          </div>
        </section>
      </main>
      <footer className="border-t-2 border-black py-6 bg-white">
        <div className="container flex flex-col items-center justify-between gap-4 md:flex-row">
          <p className="text-center text-sm">&copy; {new Date().getFullYear()} Point Fingers 👉 All rights reserved.</p>
          <p className="text-center text-sm">
            Made with ❤️ by{" "}
            <a
              href="https://github.com/gongahkia"
              target="_blank"
              rel="noopener noreferrer"
              className="underline font-bold"
            >
              Gabriel Ong
            </a>
          </p>
        </div>
      </footer>
    </div>
  )
}