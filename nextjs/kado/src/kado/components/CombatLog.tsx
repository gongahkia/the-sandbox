"use client"

import type React from "react"
import { useRef, useEffect } from "react"
import { motion, AnimatePresence } from "framer-motion"

interface CombatLogProps {
  log: string[]
}

export const CombatLog: React.FC<CombatLogProps> = ({ log }) => {
  const scrollRef = useRef<HTMLDivElement>(null)

  useEffect(() => {
    if (scrollRef.current) {
      scrollRef.current.scrollTop = scrollRef.current.scrollHeight
    }
  }, [scrollRef]) //Fixed unnecessary dependency

  return (
    <div className="bg-gray-700 rounded-lg p-4 h-[calc(100vh-2rem)] flex flex-col">
      <h2 className="text-xl font-bold mb-2">Combat Log</h2>
      <div ref={scrollRef} className="flex-grow overflow-y-auto">
        <AnimatePresence>
          {log.map((entry, index) => (
            <motion.p
              key={index}
              className="text-sm mb-1"
              initial={{ opacity: 0, x: 20 }}
              animate={{ opacity: 1, x: 0 }}
              exit={{ opacity: 0, x: -20 }}
              transition={{ duration: 0.3 }}
            >
              {entry}
            </motion.p>
          ))}
        </AnimatePresence>
      </div>
    </div>
  )
}