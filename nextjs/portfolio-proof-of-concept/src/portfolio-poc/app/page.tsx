"use client"

import type React from "react"

import Link from "next/link"
import { ArrowRight, Github, Linkedin, Mail } from "lucide-react"
import { useState } from "react"
import { smoothScroll } from "./utils/smoothScroll"

export default function Home() {
  const [name, setName] = useState("")
  const [email, setEmail] = useState("")
  const [message, setMessage] = useState("")

  const handleSubmit = async (e: React.FormEvent<HTMLFormElement>) => {
    e.preventDefault()
    // This is a placeholder for form submission
    // In a real application, you'd send this data to your server or a form service
    console.log("Form submitted", { name, email, message })
    alert("Thank you for your message! We will get back to you soon.")
    setName("")
    setEmail("")
    setMessage("")
  }

  return (
    <div className="min-h-screen bg-gruvbox-bg text-gruvbox-fg">
      {/* Navigation */}
      <nav className="p-6 flex justify-between items-center border-b-4 border-gruvbox-fg">
        <Link href="/" className="font-black text-2xl tracking-tighter">
          GABRIEL.DEV
        </Link>
        <div className="flex gap-4">
          <Link
            href="#projects"
            onClick={smoothScroll}
            className="px-4 py-2 bg-gruvbox-yellow text-gruvbox-bg border-2 border-gruvbox-fg shadow-[4px_4px_0px_0px_rgba(235,219,178,1)] hover:shadow-[2px_2px_0px_0px_rgba(235,219,178,1)] hover:translate-x-[2px] hover:translate-y-[2px] transition-all"
          >
            Projects
          </Link>
          <Link
            href="#about"
            onClick={smoothScroll}
            className="px-4 py-2 bg-gruvbox-aqua text-gruvbox-bg border-2 border-gruvbox-fg shadow-[4px_4px_0px_0px_rgba(235,219,178,1)] hover:shadow-[2px_2px_0px_0px_rgba(235,219,178,1)] hover:translate-x-[2px] hover:translate-y-[2px] transition-all"
          >
            About
          </Link>
          <Link
            href="#contact"
            onClick={smoothScroll}
            className="px-4 py-2 bg-gruvbox-blue text-gruvbox-bg border-2 border-gruvbox-fg shadow-[4px_4px_0px_0px_rgba(235,219,178,1)] hover:shadow-[2px_2px_0px_0px_rgba(235,219,178,1)] hover:translate-x-[2px] hover:translate-y-[2px] transition-all"
          >
            Contact
          </Link>
        </div>
      </nav>

      {/* Hero Section */}
      <section className="px-6 py-20 md:py-32 border-b-4 border-gruvbox-fg relative overflow-hidden">
        <div className="absolute top-10 right-10 w-32 h-32 bg-gruvbox-yellow border-4 border-gruvbox-fg rotate-12"></div>
        <div className="absolute bottom-10 left-10 w-24 h-24 bg-gruvbox-aqua border-4 border-gruvbox-fg -rotate-12"></div>

        <div className="max-w-5xl mx-auto relative">
          <h1 className="text-6xl md:text-8xl font-black mb-6 tracking-tighter">
            CREATIVE <br />
            <span className="bg-gruvbox-yellow text-gruvbox-bg px-2">DEVELOPER</span> & <br />
            <span className="bg-gruvbox-aqua text-gruvbox-bg px-2">DESIGNER</span>
          </h1>
          <p className="text-xl md:text-2xl max-w-2xl mb-8 border-l-4 border-gruvbox-fg pl-4">
            I build bold, functional websites and applications that make an impact. Let's create something amazing
            together.
          </p>
          <Link
            href="#contact"
            onClick={smoothScroll}
            className="inline-flex items-center gap-2 px-6 py-3 bg-gruvbox-orange text-gruvbox-bg text-xl font-bold hover:bg-gruvbox-red transition-all border-2 border-gruvbox-fg shadow-[4px_4px_0px_0px_rgba(235,219,178,1)] hover:shadow-none hover:translate-y-1 hover:-translate-x-1"
          >
            Let's Talk <ArrowRight size={20} />
          </Link>
        </div>
      </section>

      {/* Projects Section */}
      <section id="projects" className="px-6 py-20 border-b-4 border-gruvbox-fg">
        <div className="max-w-5xl mx-auto">
          <h2 className="text-4xl md:text-5xl font-black mb-12 tracking-tighter">
            <span className="bg-gruvbox-blue text-gruvbox-bg px-2">SELECTED PROJECTS</span>
          </h2>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
            {projects.map((project, index) => (
              <div
                key={index}
                className="border-4 border-gruvbox-fg bg-gruvbox-bg p-4 hover:translate-x-[-4px] hover:translate-y-[-4px] hover:shadow-[8px_8px_0px_0px_rgba(235,219,178,1)] transition-all"
              >
                <div className="h-64 bg-gruvbox-fg border-2 border-gruvbox-fg mb-4 overflow-hidden">
                  <img
                    src={project.image || "/placeholder.svg"}
                    alt={project.title}
                    className="w-full h-full object-cover"
                  />
                </div>
                <h3 className="text-2xl font-bold mb-2">{project.title}</h3>
                <p className="mb-4">{project.description}</p>
                <div className="flex flex-wrap gap-2 mb-4">
                  {project.tags.map((tag, tagIndex) => (
                    <span
                      key={tagIndex}
                      className="px-2 py-1 bg-gruvbox-yellow text-gruvbox-bg border border-gruvbox-fg text-sm font-medium"
                    >
                      {tag}
                    </span>
                  ))}
                </div>
                <a
                  href={project.link}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="inline-flex items-center gap-2 px-4 py-2 bg-gruvbox-green text-gruvbox-bg font-bold hover:bg-gruvbox-aqua transition-all border-2 border-gruvbox-fg shadow-[4px_4px_0px_0px_rgba(235,219,178,1)] hover:shadow-none hover:translate-y-1 hover:-translate-x-1"
                >
                  View Project <ArrowRight size={16} />
                </a>
              </div>
            ))}
          </div>
        </div>
      </section>

      {/* About Section */}
      <section id="about" className="px-6 py-20 border-b-4 border-gruvbox-fg">
        <div className="max-w-5xl mx-auto">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-12">
            <div>
              <h2 className="text-4xl md:text-5xl font-black mb-6 tracking-tighter">
                <span className="bg-gruvbox-aqua text-gruvbox-bg px-2">ABOUT ME</span>
              </h2>
              <p className="text-lg mb-4">
                I'm a frontend developer and designer with 5+ years of experience creating bold, functional websites and
                applications.
              </p>
              <p className="text-lg mb-4">
                My approach combines clean code with striking visuals to create memorable digital experiences that stand
                out from the crowd.
              </p>
              <p className="text-lg mb-6">
                When I'm not coding, you can find me experimenting with new design trends, playing video games, or
                hiking with my dog.
              </p>

              <h3 className="text-2xl font-bold mb-4">Skills</h3>
              <div className="flex flex-wrap gap-2 mb-6">
                {skills.map((skill, index) => (
                  <span
                    key={index}
                    className="px-3 py-1 bg-gruvbox-blue text-gruvbox-bg border-2 border-gruvbox-fg font-medium"
                  >
                    {skill}
                  </span>
                ))}
              </div>
            </div>

            <div className="relative">
              <div className="w-full h-[400px] border-4 border-gruvbox-fg bg-gruvbox-fg relative z-10">
                <img
                  src="/placeholder.svg?height=400&width=400"
                  alt="Portrait"
                  className="w-full h-full object-cover"
                />
              </div>
              <div className="absolute top-6 left-6 w-full h-[400px] border-4 border-gruvbox-fg bg-gruvbox-yellow -z-10"></div>
            </div>
          </div>
        </div>
      </section>

      {/* Contact Section */}
      <section id="contact" className="px-6 py-20">
        <div className="max-w-5xl mx-auto">
          <h2 className="text-4xl md:text-5xl font-black mb-12 tracking-tighter">
            <span className="bg-gruvbox-blue text-gruvbox-bg px-2">GET IN TOUCH</span>
          </h2>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-12">
            <div>
              <p className="text-xl mb-6">
                Have a project in mind or just want to say hello? I'd love to hear from you!
              </p>

              <div className="flex flex-col gap-4">
                <a href="mailto:hello@gabriel.dev" className="inline-flex items-center gap-3 text-lg hover:underline">
                  <Mail size={24} className="text-gruvbox-aqua" /> hello@gabriel.dev
                </a>
                <a
                  href="https://github.com/gabrieldev"
                  target="_blank"
                  rel="noopener noreferrer"
                  className="inline-flex items-center gap-3 text-lg hover:underline"
                >
                  <Github size={24} className="text-gruvbox-aqua" /> @gabrieldev
                </a>
                <a
                  href="https://linkedin.com/in/gabrieldev"
                  target="_blank"
                  rel="noopener noreferrer"
                  className="inline-flex items-center gap-3 text-lg hover:underline"
                >
                  <Linkedin size={24} className="text-gruvbox-aqua" /> @gabrieldev
                </a>
              </div>
            </div>

            <div>
              <form onSubmit={handleSubmit} className="space-y-4">
                <div>
                  <label htmlFor="name" className="block text-lg font-bold mb-2">
                    Name
                  </label>
                  <input
                    type="text"
                    id="name"
                    value={name}
                    onChange={(e) => setName(e.target.value)}
                    required
                    className="w-full px-4 py-3 border-4 border-gruvbox-fg bg-gruvbox-bg text-gruvbox-fg focus:outline-none focus:ring-2 focus:ring-gruvbox-yellow"
                    placeholder="Your name"
                  />
                </div>
                <div>
                  <label htmlFor="email" className="block text-lg font-bold mb-2">
                    Email
                  </label>
                  <input
                    type="email"
                    id="email"
                    value={email}
                    onChange={(e) => setEmail(e.target.value)}
                    required
                    className="w-full px-4 py-3 border-4 border-gruvbox-fg bg-gruvbox-bg text-gruvbox-fg focus:outline-none focus:ring-2 focus:ring-gruvbox-yellow"
                    placeholder="your@email.com"
                  />
                </div>
                <div>
                  <label htmlFor="message" className="block text-lg font-bold mb-2">
                    Message
                  </label>
                  <textarea
                    id="message"
                    rows={5}
                    value={message}
                    onChange={(e) => setMessage(e.target.value)}
                    required
                    className="w-full px-4 py-3 border-4 border-gruvbox-fg bg-gruvbox-bg text-gruvbox-fg focus:outline-none focus:ring-2 focus:ring-gruvbox-yellow"
                    placeholder="Tell me about your project..."
                  ></textarea>
                </div>
                <button
                  type="submit"
                  className="px-6 py-3 bg-gruvbox-orange text-gruvbox-bg text-xl font-bold hover:bg-gruvbox-red transition-colors border-2 border-gruvbox-fg shadow-[4px_4px_0px_0px_rgba(235,219,178,1)] hover:shadow-none hover:translate-x-[4px] hover:translate-y-[4px]"
                >
                  Send Message
                </button>
              </form>
            </div>
          </div>
        </div>
      </section>

      {/* Footer */}
      <footer className="px-6 py-8 border-t-4 border-gruvbox-fg">
        <div className="max-w-5xl mx-auto flex justify-center items-center">
          <div className="font-black text-xl tracking-tighter">GABRIEL.DEV © 2023-{new Date().getFullYear()}</div>
        </div>
      </footer>
    </div>
  )
}

const projects = [
  {
    title: "E-commerce Redesign",
    description: "A complete redesign of an e-commerce platform with improved UX and conversion rates.",
    image: "/placeholder.svg?height=300&width=400",
    tags: ["React", "Next.js", "Tailwind CSS", "Stripe"],
    link: "https://example-ecommerce.gabriel.dev",
  },
  {
    title: "Finance Dashboard",
    description: "An interactive dashboard for financial data visualization and analysis.",
    image: "/placeholder.svg?height=300&width=400",
    tags: ["TypeScript", "D3.js", "Firebase", "Material UI"],
    link: "https://example-finance.gabriel.dev",
  },
  {
    title: "Travel Blog Platform",
    description: "A custom CMS for travel bloggers with advanced media management.",
    image: "/placeholder.svg?height=300&width=400",
    tags: ["Vue.js", "Node.js", "MongoDB", "AWS"],
    link: "https://example-travelblog.gabriel.dev",
  },
  {
    title: "Fitness Tracking App",
    description: "Mobile-first web application for tracking workouts and nutrition.",
    image: "/placeholder.svg?height=300&width=400",
    tags: ["React Native", "GraphQL", "Redux", "Styled Components"],
    link: "https://example-fitness.gabriel.dev",
  },
]

const skills = [
  "JavaScript",
  "TypeScript",
  "React",
  "Next.js",
  "Vue.js",
  "HTML/CSS",
  "Tailwind CSS",
  "UI/UX Design",
  "Figma",
  "Node.js",
  "GraphQL",
  "Firebase",
  "AWS",
]