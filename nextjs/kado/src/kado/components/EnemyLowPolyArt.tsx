import type React from "react"

export const GoblinArt: React.FC = () => (
  <svg viewBox="0 0 100 100" className="w-full h-full">
    <polygon points="50,10 30,40 70,40" fill="#4caf50" /> {/* Head */}
    <rect x="40" y="40" width="20" height="30" fill="#81c784" /> {/* Body */}
    <polygon points="40,70 30,100 50,100" fill="#4caf50" /> {/* Left Leg */}
    <polygon points="60,70 50,100 70,100" fill="#4caf50" /> {/* Right Leg */}
    <circle cx="45" cy="30" r="5" fill="#ffffff" /> {/* Left Eye */}
    <circle cx="55" cy="30" r="5" fill="#ffffff" /> {/* Right Eye */}
    <polygon points="40,20 60,20 50,30" fill="#ff9800" /> {/* Horn */}
  </svg>
)

export const OrcArt: React.FC = () => (
  <svg viewBox="0 0 100 100" className="w-full h-full">
    <polygon points="50,10 20,40 80,40" fill="#795548" /> {/* Head */}
    <rect x="35" y="40" width="30" height="40" fill="#8d6e63" /> {/* Body */}
    <polygon points="35,80 25,100 45,100" fill="#795548" /> {/* Left Leg */}
    <polygon points="65,80 55,100 75,100" fill="#795548" /> {/* Right Leg */}
    <polygon points="20,50 0,70 20,70" fill="#8d6e63" /> {/* Left Arm */}
    <polygon points="80,50 100,70 80,70" fill="#8d6e63" /> {/* Right Arm */}
    <circle cx="40" cy="30" r="5" fill="#ff0000" /> {/* Left Eye */}
    <circle cx="60" cy="30" r="5" fill="#ff0000" /> {/* Right Eye */}
    <rect x="35" y="50" width="30" height="5" fill="#4e342e" /> {/* Belt */}
  </svg>
)

export const DragonArt: React.FC = () => (
  <svg viewBox="0 0 100 100" className="w-full h-full">
    <polygon points="50,10 30,30 70,30" fill="#f44336" /> {/* Head */}
    <polygon points="30,30 10,50 90,50 70,30" fill="#d32f2f" /> {/* Body */}
    <polygon points="10,50 0,70 40,70" fill="#d32f2f" /> {/* Left Wing */}
    <polygon points="90,50 100,70 60,70" fill="#d32f2f" /> {/* Right Wing */}
    <polygon points="40,70 30,100 50,100" fill="#f44336" /> {/* Left Leg */}
    <polygon points="60,70 50,100 70,100" fill="#f44336" /> {/* Right Leg */}
    <circle cx="40" cy="20" r="5" fill="#ffeb3b" /> {/* Left Eye */}
    <circle cx="60" cy="20" r="5" fill="#ffeb3b" /> {/* Right Eye */}
    <polygon points="45,25 55,25 50,35" fill="#ffeb3b" /> {/* Horn */}
  </svg>
)

export const getEnemyArt = (enemyName: string): React.FC => {
  switch (enemyName) {
    case "Goblin (小鬼)":
      return GoblinArt
    case "Orc (鬼)":
      return OrcArt
    case "Dragon (龍)":
      return DragonArt
    // Add more cases for other enemies
    default:
      return GoblinArt
  }
}