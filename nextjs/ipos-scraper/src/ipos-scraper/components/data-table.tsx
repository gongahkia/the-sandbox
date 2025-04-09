"use client"

import { useState } from "react"
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from "@/components/ui/table"
import { Badge } from "@/components/ui/badge"
import { Button } from "@/components/ui/button"
import { ChevronLeft, ChevronRight, ExternalLink } from "lucide-react"
import { cn } from "@/lib/utils"

type DataTableProps = {
  type: "trademarks" | "patents" | "designs"
}

export default function DataTable({ type }: DataTableProps) {
  const [page, setPage] = useState(1)

  // This would be replaced with actual data from your API
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

  const data = mockData[type]

  const getStatusColor = (status: string) => {
    switch (status.toLowerCase()) {
      case "pending":
        return "bg-yellow-100 text-yellow-800 hover:bg-yellow-100/80"
      case "published":
        return "bg-blue-100 text-blue-800 hover:bg-blue-100/80"
      case "registered":
      case "granted":
        return "bg-green-100 text-green-800 hover:bg-green-100/80"
      case "examination":
        return "bg-purple-100 text-purple-800 hover:bg-purple-100/80"
      default:
        return "bg-gray-100 text-gray-800 hover:bg-gray-100/80"
    }
  }

  return (
    <div className="flex flex-col gap-4">
      <div className="rounded-md border">
        <Table>
          <TableHeader>
            {type === "trademarks" && (
              <TableRow>
                <TableHead>Application No.</TableHead>
                <TableHead>Mark</TableHead>
                <TableHead className="hidden md:table-cell">Applicant</TableHead>
                <TableHead className="hidden md:table-cell">Class</TableHead>
                <TableHead className="hidden md:table-cell">Filing Date</TableHead>
                <TableHead>Status</TableHead>
                <TableHead className="w-[50px]"></TableHead>
              </TableRow>
            )}
            {type === "patents" && (
              <TableRow>
                <TableHead>Application No.</TableHead>
                <TableHead>Title</TableHead>
                <TableHead className="hidden md:table-cell">Applicant</TableHead>
                <TableHead className="hidden md:table-cell">Filing Date</TableHead>
                <TableHead>Status</TableHead>
                <TableHead className="w-[50px]"></TableHead>
              </TableRow>
            )}
            {type === "designs" && (
              <TableRow>
                <TableHead>Application No.</TableHead>
                <TableHead>Title</TableHead>
                <TableHead className="hidden md:table-cell">Applicant</TableHead>
                <TableHead className="hidden md:table-cell">Filing Date</TableHead>
                <TableHead>Status</TableHead>
                <TableHead className="w-[50px]"></TableHead>
              </TableRow>
            )}
          </TableHeader>
          <TableBody>
            {type === "trademarks" &&
              data.map((item: any) => (
                <TableRow key={item.id}>
                  <TableCell className="font-medium">{item.id}</TableCell>
                  <TableCell>{item.name}</TableCell>
                  <TableCell className="hidden md:table-cell">{item.applicant}</TableCell>
                  <TableCell className="hidden md:table-cell">{item.class}</TableCell>
                  <TableCell className="hidden md:table-cell">{item.filingDate}</TableCell>
                  <TableCell>
                    <Badge variant="outline" className={cn(getStatusColor(item.status))}>
                      {item.status}
                    </Badge>
                  </TableCell>
                  <TableCell>
                    <Button variant="ghost" size="icon" className="h-8 w-8">
                      <ExternalLink className="h-4 w-4" />
                      <span className="sr-only">View details</span>
                    </Button>
                  </TableCell>
                </TableRow>
              ))}
            {type === "patents" &&
              data.map((item: any) => (
                <TableRow key={item.id}>
                  <TableCell className="font-medium">{item.id}</TableCell>
                  <TableCell>{item.title}</TableCell>
                  <TableCell className="hidden md:table-cell">{item.applicant}</TableCell>
                  <TableCell className="hidden md:table-cell">{item.filingDate}</TableCell>
                  <TableCell>
                    <Badge variant="outline" className={cn(getStatusColor(item.status))}>
                      {item.status}
                    </Badge>
                  </TableCell>
                  <TableCell>
                    <Button variant="ghost" size="icon" className="h-8 w-8">
                      <ExternalLink className="h-4 w-4" />
                      <span className="sr-only">View details</span>
                    </Button>
                  </TableCell>
                </TableRow>
              ))}
            {type === "designs" &&
              data.map((item: any) => (
                <TableRow key={item.id}>
                  <TableCell className="font-medium">{item.id}</TableCell>
                  <TableCell>{item.title}</TableCell>
                  <TableCell className="hidden md:table-cell">{item.applicant}</TableCell>
                  <TableCell className="hidden md:table-cell">{item.filingDate}</TableCell>
                  <TableCell>
                    <Badge variant="outline" className={cn(getStatusColor(item.status))}>
                      {item.status}
                    </Badge>
                  </TableCell>
                  <TableCell>
                    <Button variant="ghost" size="icon" className="h-8 w-8">
                      <ExternalLink className="h-4 w-4" />
                      <span className="sr-only">View details</span>
                    </Button>
                  </TableCell>
                </TableRow>
              ))}
          </TableBody>
        </Table>
      </div>

      <div className="flex items-center justify-end space-x-2">
        <Button variant="outline" size="sm" onClick={() => setPage(page - 1)} disabled={page === 1}>
          <ChevronLeft className="h-4 w-4" />
          <span className="sr-only">Previous page</span>
        </Button>
        <div className="text-sm text-muted-foreground">Page {page} of 10</div>
        <Button variant="outline" size="sm" onClick={() => setPage(page + 1)} disabled={page === 10}>
          <ChevronRight className="h-4 w-4" />
          <span className="sr-only">Next page</span>
        </Button>
      </div>
    </div>
  )
}