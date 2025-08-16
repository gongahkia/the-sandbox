import Link from "next/link"

export default function Footer() {
  return (
    <footer className="bg-gray-100 py-4 text-center">
      <p className="text-sm text-gray-600">
        Made with ❤️ by{" "}
        <Link
          href="https://github.com/gongahkia"
          target="_blank"
          rel="noopener noreferrer"
          className="font-medium text-blue-600 hover:underline"
        >
          Gabriel Ong
        </Link>
      </p>
    </footer>
  )
}