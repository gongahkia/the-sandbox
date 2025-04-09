import * as cheerio from "cheerio"
import puppeteer from "puppeteer"

// This file contains the core scraping logic

// Function to scrape data using Puppeteer (for JavaScript-rendered pages)
export async function scrapeWithPuppeteer(url: string, selectors: Record<string, string>) {
  const browser = await puppeteer.launch({
    headless: "new",
    args: ["--no-sandbox", "--disable-setuid-sandbox"],
  })

  try {
    const page = await browser.newPage()

    // Set a user agent to avoid being blocked
    await page.setUserAgent(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
    )

    // Navigate to the page
    await page.goto(url, { waitUntil: "networkidle2" })

    // Wait for the content to load
    await page.waitForSelector(selectors.container)

    // Get the page content
    const content = await page.content()

    // Parse the content with Cheerio
    const $ = cheerio.load(content)

    // Extract the data based on the selectors
    const results: any[] = []

    $(selectors.items).each((i, element) => {
      const item: Record<string, string> = {}

      // Extract data for each field
      Object.entries(selectors.fields).forEach(([field, selector]) => {
        item[field] = $(element).find(selector).text().trim()
      })

      results.push(item)
    })

    return results
  } finally {
    await browser.close()
  }
}

// Function to scrape data using Cheerio (for static HTML pages)
export async function scrapeWithCheerio(url: string, selectors: Record<string, string>) {
  // Fetch the page
  const response = await fetch(url, {
    headers: {
      "User-Agent":
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36",
    },
  })

  // Get the HTML content
  const html = await response.text()

  // Parse the HTML with Cheerio
  const $ = cheerio.load(html)

  // Extract the data based on the selectors
  const results: any[] = []

  $(selectors.items).each((i, element) => {
    const item: Record<string, string> = {}

    // Extract data for each field
    Object.entries(selectors.fields).forEach(([field, selector]) => {
      item[field] = $(element).find(selector).text().trim()
    })

    results.push(item)
  })

  return results
}

// Specific scraper for IPOS Trademarks
export async function scrapeIPOSTrademarks() {
  const url = "https://www.ipos.gov.sg/e-services/search-ip/trademarks"

  // Define the selectors for the trademark data
  // Note: These selectors are placeholders and would need to be updated
  // based on the actual structure of the IPOS website
  const selectors = {
    container: "table.results-table",
    items: "table.results-table tbody tr",
    fields: {
      id: "td:nth-child(1)",
      name: "td:nth-child(2)",
      applicant: "td:nth-child(3)",
      class: "td:nth-child(4)",
      filingDate: "td:nth-child(5)",
      status: "td:nth-child(6)",
    },
  }

  // Use Puppeteer for JavaScript-rendered pages
  return await scrapeWithPuppeteer(url, selectors)
}

// Specific scraper for IPOS Patents
export async function scrapeIPOSPatents() {
  const url = "https://www.ipos.gov.sg/e-services/search-ip/patents"

  // Define the selectors for the patent data
  const selectors = {
    container: "table.results-table",
    items: "table.results-table tbody tr",
    fields: {
      id: "td:nth-child(1)",
      title: "td:nth-child(2)",
      applicant: "td:nth-child(3)",
      filingDate: "td:nth-child(4)",
      status: "td:nth-child(5)",
    },
  }

  return await scrapeWithPuppeteer(url, selectors)
}

// Specific scraper for IPOS Designs
export async function scrapeIPOSDesigns() {
  const url = "https://www.ipos.gov.sg/e-services/search-ip/designs"

  // Define the selectors for the design data
  const selectors = {
    container: "table.results-table",
    items: "table.results-table tbody tr",
    fields: {
      id: "td:nth-child(1)",
      title: "td:nth-child(2)",
      applicant: "td:nth-child(3)",
      filingDate: "td:nth-child(4)",
      status: "td:nth-child(5)",
    },
  }

  return await scrapeWithPuppeteer(url, selectors)
}