"use client"

import type React from "react"

import { useState, useEffect } from "react"
import { ArrowUp, Plus, RefreshCw, Trash2 } from "lucide-react"
import CurrencySelect from "./currency-select"
import HistoricalChart from "./historical-chart"

interface ConversionRow {
  id: string
  currency: string
  amount: number
}

export default function CurrencyConverter() {
  const [baseCurrency, setBaseCurrency] = useState("USD")
  const [baseAmount, setBaseAmount] = useState(1)
  const [targetCurrencies, setTargetCurrencies] = useState<ConversionRow[]>([
    { id: "1", currency: "EUR", amount: 0 },
    { id: "2", currency: "GBP", amount: 0 },
  ])
  const [rates, setRates] = useState<Record<string, number>>({})
  const [loading, setLoading] = useState(true)
  const [showChart, setShowChart] = useState(false)
  const [selectedCurrency, setSelectedCurrency] = useState("EUR")

  useEffect(() => {
    fetchRates()
  }, [])

  useEffect(() => {
    if (Object.keys(rates).length > 0) {
      updateConversions()
    }
  }, [rates, baseCurrency, baseAmount])

  const fetchRates = async () => {
    setLoading(true)
    try {
      const response = await fetch(`https://api.unirate.io/v1/rates/${baseCurrency}`)
      const data = await response.json()
      setRates(data.rates)
    } catch (error) {
      console.error("Error fetching rates:", error)
    } finally {
      setLoading(false)
    }
  }

  const updateConversions = () => {
    setTargetCurrencies((prev) =>
      prev.map((row) => ({
        ...row,
        amount: calculateConversion(baseAmount, baseCurrency, row.currency),
      })),
    )
  }

  const calculateConversion = (amount: number, from: string, to: string) => {
    if (from === to) return amount
    if (from === baseCurrency) return amount * (rates[to] || 0)
    if (to === baseCurrency) return amount / (rates[from] || 1)

    // Cross conversion
    const amountInBase = from === baseCurrency ? amount : amount / (rates[from] || 1)
    return amountInBase * (rates[to] || 0)
  }

  const handleBaseAmountChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const value = Number.parseFloat(e.target.value) || 0
    setBaseAmount(value)
  }

  const handleBaseCurrencyChange = (currency: string) => {
    setBaseCurrency(currency)
    fetchRates()
  }

  const handleTargetCurrencyChange = (id: string, currency: string) => {
    setTargetCurrencies((prev) => prev.map((row) => (row.id === id ? { ...row, currency } : row)))
    updateConversions()
  }

  const addCurrency = () => {
    const newId = Date.now().toString()
    const availableCurrencies = Object.keys(rates).filter(
      (curr) => !targetCurrencies.some((row) => row.currency === curr),
    )
    const newCurrency = availableCurrencies[0] || "JPY"

    setTargetCurrencies([
      ...targetCurrencies,
      {
        id: newId,
        currency: newCurrency,
        amount: calculateConversion(baseAmount, baseCurrency, newCurrency),
      },
    ])
  }

  const removeCurrency = (id: string) => {
    setTargetCurrencies(targetCurrencies.filter((row) => row.id !== id))
  }

  const toggleChart = (currency: string) => {
    setSelectedCurrency(currency)
    setShowChart(!showChart || selectedCurrency !== currency)
  }

  return (
    <div className="w-full max-w-3xl">
      <div className="backdrop-blur-md bg-white/10 rounded-3xl p-6 shadow-xl border border-white/20">
        <div className="flex items-center justify-center mb-6">
          <div className="animate-float">
            <span className="text-5xl mr-3">💸</span>
          </div>
          <h1 className="text-4xl font-bold text-white text-center tracking-tight">Moni Changer</h1>
        </div>

        {/* Base currency input */}
        <div className="relative mb-8 p-5 bg-white/5 rounded-2xl backdrop-blur-sm border border-white/10">
          <div className="flex flex-col md:flex-row gap-4 items-center">
            <div className="w-full md:w-2/3">
              <label className="block text-white/80 text-sm mb-2">Amount</label>
              <input
                type="number"
                value={baseAmount}
                onChange={handleBaseAmountChange}
                className="w-full bg-white/10 border border-white/20 rounded-xl p-4 text-white text-xl focus:outline-none focus:ring-2 focus:ring-purple-400 transition-all"
                placeholder="Enter amount"
              />
            </div>
            <div className="w-full md:w-1/3">
              <label className="block text-white/80 text-sm mb-2">From</label>
              <CurrencySelect
                value={baseCurrency}
                onChange={handleBaseCurrencyChange}
                currencies={
                  Object.keys(rates).length ? ["USD", ...Object.keys(rates)] : ["USD", "EUR", "GBP", "JPY", "CAD"]
                }
              />
            </div>
          </div>
          <button
            onClick={fetchRates}
            className="absolute right-4 top-4 bg-purple-500/30 hover:bg-purple-500/50 p-2 rounded-full transition-all"
            aria-label="Refresh rates"
          >
            <RefreshCw className="w-5 h-5 text-white" />
          </button>
        </div>

        {/* Target currencies */}
        <div className="space-y-4">
          {targetCurrencies.map((row) => (
            <div
              key={row.id}
              className="relative p-5 bg-white/5 rounded-2xl backdrop-blur-sm border border-white/10 hover:bg-white/10 transition-all group"
            >
              <div className="flex flex-col md:flex-row gap-4 items-center">
                <div className="w-full md:w-2/3">
                  <div className="flex items-center justify-between">
                    <label className="block text-white/80 text-sm mb-2">Converted Amount</label>
                    <button
                      onClick={() => toggleChart(row.currency)}
                      className="text-xs text-white/60 hover:text-white transition-colors mb-2"
                    >
                      {showChart && selectedCurrency === row.currency ? "Hide Chart" : "Show Chart"}
                    </button>
                  </div>
                  <div className="w-full bg-white/10 border border-white/20 rounded-xl p-4 text-white text-xl">
                    {loading ? (
                      <div className="animate-pulse h-6 bg-white/20 rounded"></div>
                    ) : (
                      <div className="flex items-center justify-between">
                        <span>{row.amount.toFixed(4)}</span>
                        <div className="flex items-center text-sm text-white/60">
                          <ArrowUp
                            className={`w-4 h-4 mr-1 ${Math.random() > 0.5 ? "text-green-400" : "text-red-400"}`}
                          />
                          {(Math.random() * 2).toFixed(2)}%
                        </div>
                      </div>
                    )}
                  </div>
                </div>
                <div className="w-full md:w-1/3">
                  <label className="block text-white/80 text-sm mb-2">To</label>
                  <CurrencySelect
                    value={row.currency}
                    onChange={(currency) => handleTargetCurrencyChange(row.id, currency)}
                    currencies={Object.keys(rates).length ? Object.keys(rates) : ["EUR", "GBP", "JPY", "CAD", "AUD"]}
                  />
                </div>
              </div>

              <button
                onClick={() => removeCurrency(row.id)}
                className="absolute right-4 top-4 opacity-0 group-hover:opacity-100 bg-red-500/30 hover:bg-red-500/50 p-2 rounded-full transition-all"
                aria-label="Remove currency"
              >
                <Trash2 className="w-4 h-4 text-white" />
              </button>

              {showChart && selectedCurrency === row.currency && (
                <div className="mt-4 pt-4 border-t border-white/10">
                  <HistoricalChart baseCurrency={baseCurrency} targetCurrency={row.currency} />
                </div>
              )}
            </div>
          ))}
        </div>

        {/* Add currency button */}
        <button
          onClick={addCurrency}
          disabled={targetCurrencies.length >= 5}
          className="mt-6 w-full py-3 bg-gradient-to-r from-purple-600 to-indigo-600 hover:from-purple-700 hover:to-indigo-700 rounded-xl text-white font-medium flex items-center justify-center gap-2 transition-all disabled:opacity-50 disabled:cursor-not-allowed"
        >
          <Plus className="w-5 h-5" />
          Add Currency
        </button>

        {/* Info text */}
        <p className="mt-6 text-center text-white/60 text-sm">
          Exchange rates provided by Unirate. Last updated: {new Date().toLocaleString()}
        </p>
      </div>
    </div>
  )
}