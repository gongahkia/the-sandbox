import { Button } from "@/components/ui/button"
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card"
import { ArrowLeft, Download, ExternalLink } from "lucide-react"
import Link from "next/link"

type Props = {
  params: {
    type: string
    id: string
  }
}

export default function DetailsPage({ params }: Props) {
  const { type, id } = params

  // This would be replaced with actual data fetching from your API
  const mockData = {
    trademarks: {
      "40202401234Q": {
        id: "40202401234Q",
        name: "ACME SOLUTIONS",
        applicant: "Acme Corp Pte Ltd",
        applicantAddress: "123 Business Park Road, #01-01, Singapore 123456",
        class: "9, 42",
        filingDate: "2024-02-15",
        status: "Pending",
        goods: "Computer software; mobile applications; artificial intelligence software",
        services: "Software as a service; computer programming; cloud computing services",
        agent: "IP Law Firm Pte Ltd",
        priority: "None",
      },
    },
    patents: {
      "10202401234P": {
        id: "10202401234P",
        title: "Method for Quantum Computing",
        applicant: "Quantum Labs Pte Ltd",
        applicantAddress: "456 Science Park Drive, #05-12, Singapore 234567",
        inventors: "Dr. Jane Smith, Dr. John Doe",
        filingDate: "2024-02-15",
        status: "Pending",
        abstract:
          "A method for implementing quantum algorithms using a novel qubit arrangement that reduces decoherence and improves computational stability.",
        ipcClass: "G06N 10/00",
        agent: "Patent Attorneys LLP",
        priority: "US 63/123,456 filed on 2023-02-16",
      },
    },
    designs: {
      "30202401234X": {
        id: "30202401234X",
        title: "Smartphone Design",
        applicant: "Mobile Innovations Pte Ltd",
        applicantAddress: "789 Tech Boulevard, #10-05, Singapore 345678",
        designers: "Sarah Lee, Michael Wong",
        filingDate: "2024-02-15",
        status: "Pending",
        locarnoClass: "14-03",
        description: "The design consists of a smartphone with a curved display and minimalist button layout.",
        agent: "Design IP Services",
        priority: "None",
      },
    },
  }

  const typeData = mockData[type as keyof typeof mockData] || {}
  const itemData = typeData[id as keyof typeof typeData]

  if (!itemData) {
    return (
      <div className="flex min-h-screen flex-col p-6 md:p-10 max-w-3xl mx-auto">
        <Link href="/" className="flex items-center text-sm mb-6 hover:underline">
          <ArrowLeft className="mr-2 h-4 w-4" />
          Back to dashboard
        </Link>
        <Card>
          <CardHeader>
            <CardTitle>Record Not Found</CardTitle>
            <CardDescription>
              The requested {type} record with ID {id} could not be found.
            </CardDescription>
          </CardHeader>
          <CardContent>
            <Button asChild>
              <Link href="/">Return to Dashboard</Link>
            </Button>
          </CardContent>
        </Card>
      </div>
    )
  }

  return (
    <div className="flex min-h-screen flex-col p-6 md:p-10 max-w-3xl mx-auto">
      <Link href="/" className="flex items-center text-sm mb-6 hover:underline">
        <ArrowLeft className="mr-2 h-4 w-4" />
        Back to dashboard
      </Link>

      <div className="flex flex-col gap-6">
        <Card>
          <CardHeader>
            <div className="flex flex-col md:flex-row md:items-center md:justify-between gap-4">
              <div>
                <CardDescription className="text-sm">
                  {type.charAt(0).toUpperCase() + type.slice(0, -1)} Application
                </CardDescription>
                <CardTitle className="text-2xl">{type === "trademarks" ? itemData.name : itemData.title}</CardTitle>
              </div>
              <div className="flex gap-2">
                <Button variant="outline" size="sm">
                  <Download className="mr-2 h-4 w-4" />
                  Export
                </Button>
                <Button size="sm" asChild>
                  <a
                    href={`https://www.ipos.gov.sg/e-services/search-ip/${type}/${id}`}
                    target="_blank"
                    rel="noopener noreferrer"
                  >
                    <ExternalLink className="mr-2 h-4 w-4" />
                    View on IPOS
                  </a>
                </Button>
              </div>
            </div>
          </CardHeader>
          <CardContent>
            <div className="grid gap-4">
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <div className="space-y-1">
                  <p className="text-sm font-medium text-muted-foreground">Application Number</p>
                  <p>{itemData.id}</p>
                </div>
                <div className="space-y-1">
                  <p className="text-sm font-medium text-muted-foreground">Filing Date</p>
                  <p>{itemData.filingDate}</p>
                </div>
              </div>

              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <div className="space-y-1">
                  <p className="text-sm font-medium text-muted-foreground">Status</p>
                  <p>{itemData.status}</p>
                </div>
                <div className="space-y-1">
                  <p className="text-sm font-medium text-muted-foreground">Agent</p>
                  <p>{itemData.agent}</p>
                </div>
              </div>

              <div className="space-y-1">
                <p className="text-sm font-medium text-muted-foreground">Applicant</p>
                <p>{itemData.applicant}</p>
              </div>

              <div className="space-y-1">
                <p className="text-sm font-medium text-muted-foreground">Applicant Address</p>
                <p>{itemData.applicantAddress}</p>
              </div>

              {type === "trademarks" && (
                <>
                  <div className="space-y-1">
                    <p className="text-sm font-medium text-muted-foreground">Class</p>
                    <p>{itemData.class}</p>
                  </div>
                  <div className="space-y-1">
                    <p className="text-sm font-medium text-muted-foreground">Goods</p>
                    <p>{itemData.goods}</p>
                  </div>
                  <div className="space-y-1">
                    <p className="text-sm font-medium text-muted-foreground">Services</p>
                    <p>{itemData.services}</p>
                  </div>
                </>
              )}

              {type === "patents" && (
                <>
                  <div className="space-y-1">
                    <p className="text-sm font-medium text-muted-foreground">Inventors</p>
                    <p>{itemData.inventors}</p>
                  </div>
                  <div className="space-y-1">
                    <p className="text-sm font-medium text-muted-foreground">IPC Class</p>
                    <p>{itemData.ipcClass}</p>
                  </div>
                  <div className="space-y-1">
                    <p className="text-sm font-medium text-muted-foreground">Abstract</p>
                    <p>{itemData.abstract}</p>
                  </div>
                </>
              )}

              {type === "designs" && (
                <>
                  <div className="space-y-1">
                    <p className="text-sm font-medium text-muted-foreground">Designers</p>
                    <p>{itemData.designers}</p>
                  </div>
                  <div className="space-y-1">
                    <p className="text-sm font-medium text-muted-foreground">Locarno Class</p>
                    <p>{itemData.locarnoClass}</p>
                  </div>
                  <div className="space-y-1">
                    <p className="text-sm font-medium text-muted-foreground">Description</p>
                    <p>{itemData.description}</p>
                  </div>
                </>
              )}

              <div className="space-y-1">
                <p className="text-sm font-medium text-muted-foreground">Priority</p>
                <p>{itemData.priority}</p>
              </div>
            </div>
          </CardContent>
        </Card>
      </div>

      <footer className="mt-auto pt-8 pb-4 text-center text-sm text-muted-foreground">
        Made with ❤️ by <a href="https://github.com/gongahkia" className="font-medium text-blue-600 hover:underline">Gabriel Ong</a>
      </footer>
    </div>
  )
}