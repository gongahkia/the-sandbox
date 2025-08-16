"use client"

import type React from "react"
import { useState, useEffect } from "react"
import { motion, AnimatePresence } from "framer-motion"
import { DamageNumber } from "./DamageNumber"

interface PlayerHealthProps {
  health: number
  maxHealth: number
  lastDamageTaken: number
}

export const PlayerHealth: React.FC<PlayerHealthProps> = ({ health, maxHealth, lastDamageTaken }) => {
  const [showDamage, setShowDamage] = useState(false)
  const [shake, setShake] = useState(false)

  useEffect(() => {
    if (lastDamageTaken > 0) {
      setShowDamage(true)
      setShake(true)
      const damageTimer = setTimeout(() => setShowDamage(false), 1000)
      const shakeTimer = setTimeout(() => setShake(false), 500)
      return () => {
        clearTimeout(damageTimer)
        clearTimeout(shakeTimer)
      }
    }
  }, [lastDamageTaken])

  return (
    <motion.div
      className="bg-gray-700 rounded-lg p-4 relative"
      animate={
        shake
          ? {
              x: [-5, 5, -5, 5, 0],
              transition: { duration: 0.5 },
            }
          : {}
      }
    >
      <p className="text-lg font-bold">
        Player Health: {health}/{maxHealth}
      </p>
      <AnimatePresence>{showDamage && <DamageNumber damage={lastDamageTaken} isEnemy={false} />}</AnimatePresence>
    </motion.div>
  )
}