"use client"

import type React from "react"
import { useState, useEffect } from "react"
import { motion, AnimatePresence } from "framer-motion"
import type { EnemyType } from "../utils/gameUtils"
import { getEnemyArt } from "./EnemyLowPolyArt"
//import { GoblinArt, OrcArt, DragonArt } from "./EnemyLowPolyArt"
import { DamageNumber } from "./DamageNumber"

interface EnemyProps {
  enemy: EnemyType
  lastDamageTaken: number
}

// const enemyArt: { [key: string]: React.FC } = {
//   Goblin: GoblinArt,
//   Orc: OrcArt,
//   Dragon: DragonArt,
// }

export const Enemy: React.FC<EnemyProps> = ({ enemy, lastDamageTaken }) => {
  const ArtComponent = getEnemyArt(enemy.name)
  const [showDamage, setShowDamage] = useState(false)

  useEffect(() => {
    if (lastDamageTaken > 0) {
      setShowDamage(true)
      const timer = setTimeout(() => setShowDamage(false), 1000)
      return () => clearTimeout(timer)
    }
  }, [lastDamageTaken])

  return (
    <motion.div
      className="bg-gray-700 rounded-lg p-4 w-64 relative"
      initial={{ opacity: 0, y: -50 }}
      animate={{ opacity: 1, y: 0 }}
      transition={{ duration: 0.5 }}
    >
      <motion.div
        className="text-center font-bold mb-2"
        initial={{ scale: 0 }}
        animate={{ scale: 1 }}
        transition={{ delay: 0.3, type: "spring", stiffness: 260, damping: 20 }}
      >
        {enemy.name}
      </motion.div>
      <motion.div
        className="h-32 bg-gray-800 rounded-md mb-2 relative"
        initial={{ opacity: 0 }}
        animate={{ opacity: 1 }}
        transition={{ delay: 0.5 }}
      >
        <ArtComponent />
        <AnimatePresence>{showDamage && <DamageNumber damage={lastDamageTaken} />}</AnimatePresence>
      </motion.div>
      <motion.div className="text-sm" initial={{ opacity: 0 }} animate={{ opacity: 1 }} transition={{ delay: 0.7 }}>
        <p>
          Health: {enemy.health}/{enemy.maxHealth}
        </p>
        <p>Intent: {enemy.intent}</p>
        <p>Damage: {enemy.damage}</p>
      </motion.div>
    </motion.div>
  )
}