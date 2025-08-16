"use client"
import { motion, AnimatePresence } from "framer-motion"
import { useGameState } from "../hooks/useGameState"
import { Card } from "../components/Card"
import { Enemy } from "../components/Enemy"
import { CombatLog } from "../components/CombatLog"
import { PlayerHealth } from "../components/PlayerHealth"

export default function Game() {
  const {
    hand,
    enemies,
    playerHealth,
    playerMaxHealth,
    playerEnergy,
    playerMaxEnergy,
    combatLog,
    playCard,
    endTurn,
    playerLastDamageTaken,
    enemiesLastDamageTaken,
  } = useGameState()

  return (
    <div className="min-h-screen bg-gray-800 text-white p-4">
      <div className="max-w-6xl mx-auto flex">
        <div className="flex-grow mr-4">
          <motion.h1
            className="text-4xl font-bold mb-4"
            initial={{ opacity: 0, y: -50 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5 }}
          >
            Kado (カド)
          </motion.h1>
          <div className="flex justify-between mb-4">
            <motion.div
              initial={{ opacity: 0, x: -50 }}
              animate={{ opacity: 1, x: 0 }}
              transition={{ duration: 0.5, delay: 0.2 }}
            >
              <PlayerHealth health={playerHealth} maxHealth={playerMaxHealth} lastDamageTaken={playerLastDamageTaken} />
              <p className="mt-2">
                Energy: {playerEnergy}/{playerMaxEnergy}
              </p>
            </motion.div>
            <div className="flex space-x-4">
              <AnimatePresence>
                {enemies.map((enemy, index) => (
                  <Enemy key={enemy.id} enemy={enemy} lastDamageTaken={enemiesLastDamageTaken[index]} />
                ))}
              </AnimatePresence>
            </div>
          </div>
          <motion.div
            className="flex flex-wrap gap-4 mb-4"
            initial={{ opacity: 0, y: 50 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5, delay: 0.4 }}
          >
            {hand.map((card) => (
              <Card key={card.id} card={card} onPlay={(card) => playCard(card, 0)} />
            ))}
          </motion.div>
          <motion.button
            className="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
            onClick={endTurn}
            whileHover={{ scale: 1.05 }}
            whileTap={{ scale: 0.95 }}
          >
            End Turn
          </motion.button>
        </div>
        <div className="w-64">
          <CombatLog log={combatLog} />
        </div>
      </div>
    </div>
  )
}
