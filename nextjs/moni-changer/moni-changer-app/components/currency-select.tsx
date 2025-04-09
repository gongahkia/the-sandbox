"use client"

import { useState, useRef, useEffect } from "react"
import { ChevronDown } from "lucide-react"
import { getCurrencyFlag, getCurrencyName } from "@/lib/currency-utils"

interface CurrencySelectProps {
  value: string
  onChange: (value: string) => void
  currencies: string[]
}

export default function CurrencySelect({ value, onChange, currencies }: CurrencySelectProps) {
  const [isOpen, setIsOpen] = useState(false)
  const ref = useRef<HTMLDivElement>(null)

  useEffect(() => {
    function handleClickOutside(event: MouseEvent) {
      if (ref.current && !ref.current.contains(event.target as Node)) {
        setIsOpen(false)
      }
    }

    document.addEventListener("mousedown", handleClickOutside)
    return () => {
      document.removeEventListener("mousedown", handleClickOutside)
    }
  }, [])

  const handleSelect = (currency: string) => {
    onChange(currency)
    setIsOpen(false)
  }

  return (
    <div className="relative" ref={ref}>
      <button
        type="button"
        onClick={() => setIsOpen(!isOpen)}
        className="w-full bg-white/10 border border-white/20 rounded-xl p-4 text-white flex items-center justify-between focus:outline-none focus:ring-2 focus:ring-purple-400 transition-all"
      >
        <div className="flex items-center">
          <span className="mr-2 text-xl">{getCurrencyFlag(value)}</span>
          <span>{value}</span>
        </div>
        <ChevronDown className={`w-5 h-5 transition-transform ${isOpen ? "rotate-180" : ""}`} />
      </button>

      {isOpen && (
        <div className="absolute z-10 mt-1 w-full bg-gradient-to-b from-purple-900/90 to-indigo-900/90 backdrop-blur-lg rounded-xl shadow-xl border border-white/10 max-h-60 overflow-auto">
          <div className="p-1">
            {currencies.map((currency) => (
              <button
                key={currency}
                onClick={() => handleSelect(currency)}
                className={`w-full text-left px-4 py-3 flex items-center rounded-lg transition-colors ${
                  value === currency ? "bg-white/20 text-white" : "text-white/80 hover:bg-white/10"
                }`}
              >
                <span className="mr-2 text-xl">{getCurrencyFlag(currency)}</span>
                <div className="flex flex-col">
                  <span>{currency}</span>
                  <span className="text-xs text-white/60">{getCurrencyName(currency)}</span>
                </div>
              </button>
            ))}
          </div>
        </div>
      )}
    </div>
  )
}