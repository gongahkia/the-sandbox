import type React from "react"
import { motion } from "framer-motion"

interface DamageNumberProps {
  damage: number
  isEnemy?: boolean
}

export const DamageNumber: React.FC<DamageNumberProps> = ({ damage, isEnemy = true }) => {
  const positionClass = isEnemy ? "top-0 right-0" : "bottom-0 left-0"

  return (
    <motion.div
      className={`absolute ${positionClass} text-2xl font-bold`}
      initial={{ opacity: 1, y: 0 }}
      animate={{ opacity: 0, y: isEnemy ? -50 : 50 }}
      exit={{ opacity: 0 }}
      transition={{ duration: 1 }}
    >
      {isEnemy ? `-${damage}` : damage}
    </motion.div>
  )
}