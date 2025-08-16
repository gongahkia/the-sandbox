import Link from "next/link"
import { Heart } from "lucide-react"

export default function Footer() {
  return (
    <footer className="mt-8 text-center">
      <p className="text-lg font-semibold flex items-center justify-center">
        Made with <Heart className="text-red-500 mx-1" /> by{" "}
        <Link href="https://github.com/gongahkia" className="ml-1 text-blue-600 hover:underline">
          Gabriel Ong
        </Link>
      </p>
    </footer>
  )
}