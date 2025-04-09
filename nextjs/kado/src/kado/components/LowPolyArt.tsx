import type React from "react"

export const SlashArt: React.FC = () => (
  <svg viewBox="0 0 100 100" className="w-full h-full">
    <polygon points="10,90 40,10 60,10 90,90" fill="#ff4136" />
    <line x1="40" y1="10" x2="90" y2="90" stroke="#ffffff" strokeWidth="2" />
    <polygon points="30,50 70,50 50,90" fill="#ffffff" opacity="0.5" />
  </svg>
)

export const BlockArt: React.FC = () => (
  <svg viewBox="0 0 100 100" className="w-full h-full">
    <rect x="10" y="10" width="80" height="80" fill="#0074d9" />
    <rect x="20" y="20" width="60" height="60" fill="#7fdbff" />
    <polygon points="50,10 10,50 50,90 90,50" fill="#ffffff" opacity="0.3" />
  </svg>
)

export const FireballArt: React.FC = () => (
  <svg viewBox="0 0 100 100" className="w-full h-full">
    <polygon points="50,10 20,90 80,90" fill="#ff851b" />
    <circle cx="50" cy="50" r="20" fill="#ffdc00" />
    <path d="M50,30 Q60,50 50,70 Q40,50 50,30" fill="#ff4136" />
  </svg>
)

export const IceShieldArt: React.FC = () => (
  <svg viewBox="0 0 100 100" className="w-full h-full">
    <polygon points="50,10 10,50 50,90 90,50" fill="#39cccc" />
    <polygon points="50,20 20,50 50,80 80,50" fill="#7fdbff" />
    <path d="M30,50 L70,50 M50,30 L50,70" stroke="#ffffff" strokeWidth="2" />
  </svg>
)

export const LightningArt: React.FC = () => (
  <svg viewBox="0 0 100 100" className="w-full h-full">
    <polygon points="60,10 30,50 50,50 40,90 70,50 50,50" fill="#ffdc00" />
    <polygon points="55,15 35,45 45,45 40,80 60,45 50,45" fill="#ffffff" opacity="0.5" />
  </svg>
)

export const getCardArt = (cardName: string): React.FC => {
  switch (cardName) {
    case "Slash":
      return SlashArt
    case "Block":
      return BlockArt
    case "Fireball":
      return FireballArt
    case "Ice Shield":
      return IceShieldArt
    case "Lightning Strike":
      return LightningArt
    // Add more cases for other cards
    default:
      return SlashArt
  }
}