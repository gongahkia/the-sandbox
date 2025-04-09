import type React from "react"
import { motion } from "framer-motion"
import type { CardType } from "../utils/gameUtils"
import { getCardArt } from "./LowPolyArt"

interface CardProps {
  card: CardType
  onPlay: (card: CardType) => void
}

export const Card: React.FC<CardProps> = ({ card, onPlay }) => {
  const ArtComponent = getCardArt(card.name)

  return (
    <motion.div
      className="bg-gray-700 rounded-lg p-2 w-32 h-48 flex flex-col justify-between cursor-pointer"
      whileHover={{ scale: 1.05, boxShadow: "0px 0px 8px rgb(255,255,255)" }}
      whileTap={{ scale: 0.95 }}
      onClick={() => onPlay(card)}
    >
      <motion.div
        className="text-center font-bold"
        initial={{ opacity: 0 }}
        animate={{ opacity: 1 }}
        transition={{ delay: 0.2 }}
      >
        {card.name}
      </motion.div>
      <motion.div
        className="h-16 bg-gray-800 rounded-md mb-2"
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ delay: 0.4 }}
      >
        <ArtComponent />
      </motion.div>
      <motion.div className="text-sm" initial={{ opacity: 0 }} animate={{ opacity: 1 }} transition={{ delay: 0.6 }}>
        <p>Damage: {card.damage}</p>
        <p>Defense: {card.defense}</p>
        <p>Energy: {card.energy}</p>
        {card.effect && <p className="text-yellow-300">{card.effect}</p>}
      </motion.div>
    </motion.div>
  )
}