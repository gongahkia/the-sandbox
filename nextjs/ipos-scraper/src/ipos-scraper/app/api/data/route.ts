import { NextResponse } from "next/server"

// This is a server-side route handler that will return the stored data
export async function GET(request: Request) {
  try {
    const { searchParams } = new URL(request.url)
    const type = searchParams.get("type") || "trademarks"
    const page = Number.parseInt(searchParams.get("page") || "1")
    const limit = Number.parseInt(searchParams.get("limit") || "10")
    const search = searchParams.get("search") || ""

    // In a real application, you would fetch this data from your database
    // This is mock data for demonstration purposes
    const mockData = {
      trademarks: [
        {
          id: "40202401234Q",
          name: "ACME SOLUTIONS",
          applicant: "Acme Corp Pte Ltd",
          class: "9, 42",
          filingDate: "2024-02-15",
          status: "Pending",
        },
        {
          id: "40202401235S",
          name: "TECHWAVE",
          applicant: "TechWave Industries",
          class: "9, 35",
          filingDate: "2024-02-14",
          status: "Published",
        },
        {
          id: "40202401236T",
          name: "GREENLEAF",
          applicant: "Green Solutions Pte Ltd",
          class: "3, 5",
          filingDate: "2024-02-14",
          status: "Registered",
        },
        {
          id: "40202401237U",
          name: "BLUEOCEAN",
          applicant: "Blue Ocean Ventures",
          class: "36, 41",
          filingDate: "2024-02-13",
          status: "Pending",
        },
        {
          id: "40202401238V",
          name: "SKYREACH",
          applicant: "Sky Technologies",
          class: "9, 38, 42",
          filingDate: "2024-02-12",
          status: "Published",
        },
      ],
      patents: [
        {
          id: "10202401234P",
          title: "Method for Quantum Computing",
          applicant: "Quantum Labs Pte Ltd",
          filingDate: "2024-02-15",
          status: "Pending",
        },
        {
          id: "10202401235Q",
          title: "Solar Energy Conversion System",
          applicant: "Green Energy Solutions",
          filingDate: "2024-02-14",
          status: "Published",
        },
        {
          id: "10202401236R",
          title: "AI-Based Medical Diagnostic Tool",
          applicant: "MedTech Innovations",
          filingDate: "2024-02-13",
          status: "Examination",
        },
        {
          id: "10202401237S",
          title: "Biodegradable Packaging Material",
          applicant: "Eco Packaging Pte Ltd",
          filingDate: "2024-02-12",
          status: "Granted",
        },
        {
          id: "10202401238T",
          title: "Wireless Power Transmission Device",
          applicant: "PowerTech Solutions",
          filingDate: "2024-02-11",
          status: "Pending",
        },
      ],
      designs: [
        {
          id: "30202401234X",
          title: "Smartphone Design",
          applicant: "Mobile Innovations Pte Ltd",
          filingDate: "2024-02-15",
          status: "Pending",
        },
        {
          id: "30202401235Y",
          title: "Furniture Design - Chair",
          applicant: "Modern Furniture Co",
          filingDate: "2024-02-14",
          status: "Registered",
        },
        {
          id: "30202401236Z",
          title: "Wearable Device",
          applicant: "Tech Wearables Pte Ltd",
          filingDate: "2024-02-13",
          status: "Published",
        },
        {
          id: "30202401237A",
          title: "Lighting Fixture",
          applicant: "Illumination Designs",
          filingDate: "2024-02-12",
          status: "Pending",
        },
        {
          id: "30202401238B",
          title: "Packaging Design",
          applicant: "Creative Packaging Solutions",
          filingDate: "2024-02-11",
          status: "Registered",
        },
      ],
    }

    // Filter data based on search term if provided
    let filteredData = mockData[type as keyof typeof mockData] || []
    if (search) {
      const searchLower = search.toLowerCase()
      filteredData = filteredData.filter((item: any) => {
        // Search in different fields based on the type
        if (type === "trademarks") {
          return (
            item.id.toLowerCase().includes(searchLower) ||
            item.name.toLowerCase().includes(searchLower) ||
            item.applicant.toLowerCase().includes(searchLower)
          )
        } else {
          return (
            item.id.toLowerCase().includes(searchLower) ||
            item.title.toLowerCase().includes(searchLower) ||
            item.applicant.toLowerCase().includes(searchLower)
          )
        }
      })
    }

    // Paginate the results
    const startIndex = (page - 1) * limit
    const endIndex = page * limit
    const paginatedData = filteredData.slice(startIndex, endIndex)

    return NextResponse.json({
      data: paginatedData,
      pagination: {
        total: filteredData.length,
        page,
        limit,
        totalPages: Math.ceil(filteredData.length / limit),
      },
    })
  } catch (error) {
    console.error("Data retrieval error:", error)
    return NextResponse.json({ error: "Failed to retrieve data" }, { status: 500 })
  }
}