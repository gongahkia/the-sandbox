import { NextResponse } from "next/server"
import * as cheerio from "cheerio"

// This is a server-side route handler that will perform the scraping
export async function POST(request: Request) {
  try {
    const { source } = await request.json()

    // Determine which source to scrape
    let data = []

    switch (source) {
      case "trademarks":
        data = await scrapeTrademarks()
        break
      case "patents":
        data = await scrapePatents()
        break
      case "designs":
        data = await scrapeDesigns()
        break
      default:
        return NextResponse.json({ error: "Invalid source" }, { status: 400 })
    }

    return NextResponse.json({ success: true, data })
  } catch (error) {
    console.error("Scraping error:", error)
    return NextResponse.json({ error: "Failed to scrape data" }, { status: 500 })
  }
}

// Function to scrape trademark data
async function scrapeTrademarks() {
  try {
    // The URL for IPOS trademark search
    const response = await fetch("https://www.ipos.gov.sg/e-services/search-ip/trademarks", {
      headers: {
        "User-Agent":
          "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
      },
    })

    const html = await response.text()
    const $ = cheerio.load(html)

    // This is a placeholder for the actual scraping logic
    // You would need to analyze the IPOS website structure and adapt this code
    const trademarks = []

    // Example scraping logic (this would need to be adapted to the actual website structure)
    $("table.results-table tr").each((i, element) => {
      if (i === 0) return // Skip header row

      const trademark = {
        id: $(element).find("td:nth-child(1)").text().trim(),
        name: $(element).find("td:nth-child(2)").text().trim(),
        applicant: $(element).find("td:nth-child(3)").text().trim(),
        class: $(element).find("td:nth-child(4)").text().trim(),
        filingDate: $(element).find("td:nth-child(5)").text().trim(),
        status: $(element).find("td:nth-child(6)").text().trim(),
      }

      trademarks.push(trademark)
    })

    return trademarks
  } catch (error) {
    console.error("Error scraping trademarks:", error)
    throw error
  }
}

// Function to scrape patent data
async function scrapePatents() {
  try {
    // The URL for IPOS patent search
    const response = await fetch("https://www.ipos.gov.sg/e-services/search-ip/patents", {
      headers: {
        "User-Agent":
          "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
      },
    })

    const html = await response.text()
    const $ = cheerio.load(html)

    // This is a placeholder for the actual scraping logic
    const patents = []

    // Example scraping logic (this would need to be adapted to the actual website structure)
    $("table.results-table tr").each((i, element) => {
      if (i === 0) return // Skip header row

      const patent = {
        id: $(element).find("td:nth-child(1)").text().trim(),
        title: $(element).find("td:nth-child(2)").text().trim(),
        applicant: $(element).find("td:nth-child(3)").text().trim(),
        filingDate: $(element).find("td:nth-child(4)").text().trim(),
        status: $(element).find("td:nth-child(5)").text().trim(),
      }

      patents.push(patent)
    })

    return patents
  } catch (error) {
    console.error("Error scraping patents:", error)
    throw error
  }
}

// Function to scrape design data
async function scrapeDesigns() {
  try {
    // The URL for IPOS design search
    const response = await fetch("https://www.ipos.gov.sg/e-services/search-ip/designs", {
      headers: {
        "User-Agent":
          "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
      },
    })

    const html = await response.text()
    const $ = cheerio.load(html)

    // This is a placeholder for the actual scraping logic
    const designs = []

    // Example scraping logic (this would need to be adapted to the actual website structure)
    $("table.results-table tr").each((i, element) => {
      if (i === 0) return // Skip header row

      const design = {
        id: $(element).find("td:nth-child(1)").text().trim(),
        title: $(element).find("td:nth-child(2)").text().trim(),
        applicant: $(element).find("td:nth-child(3)").text().trim(),
        filingDate: $(element).find("td:nth-child(4)").text().trim(),
        status: $(element).find("td:nth-child(5)").text().trim(),
      }

      designs.push(design)
    })

    return designs
  } catch (error) {
    console.error("Error scraping designs:", error)
    throw error
  }
}