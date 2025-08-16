import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { Button } from "@/components/ui/button"
import { Input } from "@/components/ui/input"
import { Search, RefreshCw, Download, Filter } from "lucide-react"
import DataTable from "@/components/data-table"
import ScraperStatus from "@/components/scraper-status"

export default function Home() {
  return (
    <main className="flex min-h-screen flex-col p-6 md:p-10 max-w-7xl mx-auto">
      <div className="flex flex-col gap-4 md:gap-8">
        <div className="flex flex-col gap-2">
          <h1 className="text-3xl font-bold tracking-tight">IPOS Scraper</h1>
          <p className="text-muted-foreground">
            Monitor and search intellectual property applications from Singapore&apos;s IPOS
          </p>
        </div>

        <ScraperStatus />

        <div className="flex flex-col gap-4">
          <Tabs defaultValue="trademarks" className="w-full">
            <div className="flex flex-col md:flex-row md:items-center justify-between gap-4 mb-4">
              <TabsList>
                <TabsTrigger value="trademarks">Trademarks</TabsTrigger>
                <TabsTrigger value="patents">Patents</TabsTrigger>
                <TabsTrigger value="designs">Designs</TabsTrigger>
              </TabsList>

              <div className="flex items-center gap-2">
                <div className="relative flex-1 md:w-64">
                  <Search className="absolute left-2.5 top-2.5 h-4 w-4 text-muted-foreground" />
                  <Input type="search" placeholder="Search records..." className="pl-8" />
                </div>
                <Button variant="outline" size="icon">
                  <Filter className="h-4 w-4" />
                  <span className="sr-only">Filter</span>
                </Button>
                <Button variant="outline" size="icon">
                  <Download className="h-4 w-4" />
                  <span className="sr-only">Download</span>
                </Button>
                <Button variant="outline" size="icon">
                  <RefreshCw className="h-4 w-4" />
                  <span className="sr-only">Refresh</span>
                </Button>
              </div>
            </div>

            <TabsContent value="trademarks" className="mt-0">
              <Card>
                <CardHeader className="pb-2">
                  <CardTitle>Trademark Applications</CardTitle>
                  <CardDescription>Recent trademark applications filed with IPOS</CardDescription>
                </CardHeader>
                <CardContent>
                  <DataTable type="trademarks" />
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="patents" className="mt-0">
              <Card>
                <CardHeader className="pb-2">
                  <CardTitle>Patent Applications</CardTitle>
                  <CardDescription>Recent patent applications filed with IPOS</CardDescription>
                </CardHeader>
                <CardContent>
                  <DataTable type="patents" />
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="designs" className="mt-0">
              <Card>
                <CardHeader className="pb-2">
                  <CardTitle>Design Applications</CardTitle>
                  <CardDescription>Recent design applications filed with IPOS</CardDescription>
                </CardHeader>
                <CardContent>
                  <DataTable type="designs" />
                </CardContent>
              </Card>
            </TabsContent>
          </Tabs>
        </div>
      </div>

      <footer className="mt-auto pt-8 pb-4 text-center text-sm text-muted-foreground">
        Made with ❤️ by <a href="https://github.com/gongahkia" className="font-medium text-blue-600 hover:underline">Gabriel Ong</a>
      </footer>
    </main>
  )
}