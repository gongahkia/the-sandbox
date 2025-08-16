import { geolocation } from "@vercel/functions"
import { type NextRequest, NextResponse } from "next/server"

export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url)
    const ipParam = searchParams.get("ip")

    // If an IP is provided, we would normally fetch data for that IP
    // For this example, we'll use Vercel's geolocation for the current request
    const geoData = geolocation(request)

    // In a production app, you would use a real IP geolocation API
    // This is a simplified example using Vercel's built-in geolocation
    const ipData = {
      ip: ipParam || request.headers.get("x-forwarded-for") || "127.0.0.1",
      country: geoData.country || "Unknown",
      countryCode: geoData.countryCode || "XX",
      region: geoData.region || "Unknown",
      city: geoData.city || "Unknown",
      latitude: Number.parseFloat(geoData.latitude || "0"),
      longitude: Number.parseFloat(geoData.longitude || "0"),
      timezone: "UTC", // Would come from a real API
      isp: "Example ISP", // Would come from a real API
      org: "Example Organization", // Would come from a real API
      asn: "AS12345", // Would come from a real API
      proxy: false, // Would come from a real API
      vpn: false, // Would come from a real API
      tor: false, // Would come from a real API
    }

    return NextResponse.json(ipData)
  } catch (error) {
    console.error("Error processing IP information:", error)
    return NextResponse.json({ error: "Failed to process IP information" }, { status: 500 })
  }
}