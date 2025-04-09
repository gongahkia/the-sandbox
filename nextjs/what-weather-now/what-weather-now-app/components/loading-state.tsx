"use client"

import { motion } from "framer-motion"

export default function LoadingState() {
  return (
    <div className="flex flex-col items-center justify-center py-20">
      <motion.div
        className="relative w-20 h-20"
        animate={{ rotate: 360 }}
        transition={{ duration: 2, repeat: Number.POSITIVE_INFINITY, ease: "linear" }}
      >
        <motion.div
          className="absolute top-0 left-0 w-full h-full rounded-full border-t-4 border-blue-400"
          animate={{ opacity: [0.2, 1, 0.2] }}
          transition={{ duration: 1.5, repeat: Number.POSITIVE_INFINITY, ease: "easeInOut" }}
        />
        <motion.div
          className="absolute top-0 left-0 w-full h-full rounded-full border-r-4 border-indigo-400"
          animate={{ opacity: [0.2, 1, 0.2] }}
          transition={{ duration: 1.5, repeat: Number.POSITIVE_INFINITY, ease: "easeInOut", delay: 0.2 }}
        />
        <motion.div
          className="absolute top-0 left-0 w-full h-full rounded-full border-b-4 border-teal-400"
          animate={{ opacity: [0.2, 1, 0.2] }}
          transition={{ duration: 1.5, repeat: Number.POSITIVE_INFINITY, ease: "easeInOut", delay: 0.4 }}
        />
      </motion.div>
      <motion.p
        className="mt-6 text-blue-100 text-lg"
        animate={{ opacity: [0.5, 1, 0.5] }}
        transition={{ duration: 2, repeat: Number.POSITIVE_INFINITY, ease: "easeInOut" }}
      >
        Loading weather data...
      </motion.p>
    </div>
  )
}