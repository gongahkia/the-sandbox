export type CardType = {
  id: number
  name: string
  damage: number
  defense: number
  energy: number
  effect?: string
  sprite: string
  synergy?: string
}

export type EnemyType = {
  id: number
  name: string
  health: number
  maxHealth: number
  intent: string
  damage: number
  sprite: string
  group?: string
}

export const generateDeck = (): CardType[] => {
  const deck: CardType[] = [
    { id: 1, name: "Slash", damage: 6, defense: 0, energy: 1, sprite: "/katana-slash.png" },
    { id: 2, name: "Block", damage: 0, defense: 5, energy: 1, sprite: "/samurai-shield.png" },
    {
      id: 3,
      name: "Fireball",
      damage: 8,
      defense: 0,
      energy: 2,
      effect: "Apply 2 Burn",
      sprite: "/oni-fireball.png",
      synergy: "Fire",
    },
    {
      id: 4,
      name: "Ice Shield",
      damage: 0,
      defense: 8,
      energy: 2,
      effect: "Apply 1 Frost",
      sprite: "/yuki-onna-shield.png",
      synergy: "Ice",
    },
    {
      id: 5,
      name: "Lightning Strike",
      damage: 10,
      defense: 0,
      energy: 2,
      effect: "Chain to next card",
      sprite: "/raijin-bolt.png",
      synergy: "Lightning",
    },
    {
      id: 6,
      name: "Inferno",
      damage: 15,
      defense: 0,
      energy: 3,
      effect: "Apply 3 Burn",
      sprite: "/kitsune-fire.png",
      synergy: "Fire",
    },
    {
      id: 7,
      name: "Frost Nova",
      damage: 5,
      defense: 5,
      energy: 2,
      effect: "Apply 2 Frost to all enemies",
      sprite: "/yukionna-blizzard.png",
      synergy: "Ice",
    },
    {
      id: 8,
      name: "Thunder Clap",
      damage: 12,
      defense: 0,
      energy: 2,
      effect: "Stun enemy for 1 turn",
      sprite: "/raijin-drums.png",
      synergy: "Lightning",
    },
    {
      id: 9,
      name: "Vampiric Strike",
      damage: 8,
      defense: 0,
      energy: 2,
      effect: "Heal for half the damage dealt",
      sprite: "/oni-drain.png",
      synergy: "Blood",
    },
    {
      id: 10,
      name: "Poison Dart",
      damage: 4,
      defense: 0,
      energy: 1,
      effect: "Apply 3 Poison",
      sprite: "/ninja-dart.png",
      synergy: "Poison",
    },
    {
      id: 11,
      name: "Earthquake",
      damage: 20,
      defense: 0,
      energy: 4,
      effect: "Damage all enemies",
      sprite: "/oni-stomp.png",
      synergy: "Earth",
    },
    {
      id: 12,
      name: "Healing Light",
      damage: 0,
      defense: 0,
      energy: 2,
      effect: "Heal 10 HP",
      sprite: "/amaterasu-light.png",
      synergy: "Light",
    },
    {
      id: 13,
      name: "Shadow Blade",
      damage: 7,
      defense: 0,
      energy: 1,
      effect: "Draw a card",
      sprite: "/ninja-shadow.png",
      synergy: "Shadow",
    },
    {
      id: 14,
      name: "Whirlwind",
      damage: 3,
      defense: 3,
      energy: 1,
      effect: "Hit all enemies twice",
      sprite: "/kamaitachi-wind.png",
      synergy: "Wind",
    },
    {
      id: 15,
      name: "Arcane Missile",
      damage: 2,
      defense: 0,
      energy: 1,
      effect: "Hit 3 times",
      sprite: "/onmyoji-spell.png",
      synergy: "Arcane",
    },
    {
      id: 16,
      name: "Nature's Embrace",
      damage: 0,
      defense: 12,
      energy: 2,
      effect: "Gain 1 energy",
      sprite: "/kodama-shield.png",
      synergy: "Nature",
    },
    {
      id: 17,
      name: "Time Warp",
      damage: 0,
      defense: 0,
      energy: 3,
      effect: "Take an extra turn",
      sprite: "/tsukuyomi-moon.png",
      synergy: "Time",
    },
    {
      id: 18,
      name: "Chaos Bolt",
      damage: 8,
      defense: 0,
      energy: 2,
      effect: "Random additional effect",
      sprite: "/tengu-chaos.png",
      synergy: "Chaos",
    },
    {
      id: 19,
      name: "Mirror Image",
      damage: 0,
      defense: 7,
      energy: 2,
      effect: "Duplicate your next card",
      sprite: "/tanuki-illusion.png",
      synergy: "Illusion",
    },
    {
      id: 20,
      name: "Soul Drain",
      damage: 6,
      defense: 0,
      energy: 2,
      effect: "Enemy loses 1 energy",
      sprite: "/shinigami-scythe.png",
      synergy: "Dark",
    },
    {
      id: 21,
      name: "Elemental Fusion",
      damage: 0,
      defense: 0,
      energy: 1,
      effect: "Combine effects of your next 2 cards",
      sprite: "/five-elements.png",
      synergy: "Elemental",
    },
    {
      id: 22,
      name: "Mana Surge",
      damage: 0,
      defense: 0,
      energy: 0,
      effect: "Gain 2 energy",
      sprite: "/magatama-glow.png",
      synergy: "Arcane",
    },
    {
      id: 23,
      name: "Berserk",
      damage: 15,
      defense: -5,
      energy: 2,
      effect: "Take 5 damage",
      sprite: "/oni-rage.png",
      synergy: "Rage",
    },
    {
      id: 24,
      name: "Holy Smite",
      damage: 12,
      defense: 0,
      energy: 2,
      effect: "Heal 4 HP",
      sprite: "/kami-blessing.png",
      synergy: "Light",
    },
    {
      id: 25,
      name: "Void Rift",
      damage: 0,
      defense: 0,
      energy: 3,
      effect: "Banish an enemy",
      sprite: "/yokai-portal.png",
      synergy: "Void",
    },
  ]
  return shuffleDeck([...deck, ...deck, ...deck]) // Create a deck with 75 cards (3 of each)
}

export const shuffleDeck = (deck: CardType[]): CardType[] => {
  return deck.sort(() => Math.random() - 0.5)
}

export const drawCards = (deck: CardType[], count: number): [CardType[], CardType[]] => {
  const drawnCards = deck.slice(0, count)
  const remainingDeck = deck.slice(count)
  return [remainingDeck, drawnCards]
}

export const generateEnemy = (): EnemyType => {
  const enemies = [
    {
      id: 1,
      name: "Goblin (小鬼)",
      health: 30,
      maxHealth: 30,
      intent: "Attack",
      damage: 5,
      sprite: "/goblin-oni.png",
      group: "Horde",
    },
    {
      id: 2,
      name: "Orc (鬼)",
      health: 40,
      maxHealth: 40,
      intent: "Defend",
      damage: 8,
      sprite: "/orc-oni.png",
      group: "Horde",
    },
    {
      id: 3,
      name: "Dragon (龍)",
      health: 50,
      maxHealth: 50,
      intent: "Special",
      damage: 12,
      sprite: "/japanese-dragon.png",
    },
    {
      id: 4,
      name: "Skeleton Archer (骨弓手)",
      health: 25,
      maxHealth: 25,
      intent: "Attack",
      damage: 7,
      sprite: "/gashadokuro-archer.png",
      group: "Undead",
    },
    {
      id: 5,
      name: "Necromancer (死霊術師)",
      health: 35,
      maxHealth: 35,
      intent: "Special",
      damage: 6,
      sprite: "/onmyoji-necromancer.png",
      group: "Undead",
    },
    {
      id: 6,
      name: "Troll (山姥)",
      health: 60,
      maxHealth: 60,
      intent: "Defend",
      damage: 10,
      sprite: "/yamamba-troll.png",
      group: "Giant",
    },
    {
      id: 7,
      name: "Harpy (鳥女)",
      health: 30,
      maxHealth: 30,
      intent: "Attack",
      damage: 8,
      sprite: "/tengu-harpy.png",
      group: "Flying",
    },
    {
      id: 8,
      name: "Minotaur (牛鬼)",
      health: 45,
      maxHealth: 45,
      intent: "Attack",
      damage: 11,
      sprite: "/ushi-oni.png",
      group: "Beast",
    },
    {
      id: 9,
      name: "Bandit (山賊)",
      health: 28,
      maxHealth: 28,
      intent: "Attack",
      damage: 6,
      sprite: "/ninja-bandit.png",
      group: "Human",
    },
    {
      id: 10,
      name: "Witch (魔女)",
      health: 32,
      maxHealth: 32,
      intent: "Special",
      damage: 9,
      sprite: "/onibaba-witch.png",
      group: "Human",
    },
    {
      id: 11,
      name: "Golem (土偶)",
      health: 70,
      maxHealth: 70,
      intent: "Defend",
      damage: 7,
      sprite: "/dogu-golem.png",
      group: "Construct",
    },
    {
      id: 12,
      name: "Imp (小悪魔)",
      health: 20,
      maxHealth: 20,
      intent: "Special",
      damage: 4,
      sprite: "/koakuma-imp.png",
      group: "Demon",
    },
    {
      id: 13,
      name: "Werewolf (狼男)",
      health: 38,
      maxHealth: 38,
      intent: "Attack",
      damage: 9,
      sprite: "/okami-otoko.png",
      group: "Beast",
    },
  ]
  return enemies[Math.floor(Math.random() * enemies.length)]
}

export const generateEnemyGroup = (): EnemyType[] => {
  const groupTypes = ["Horde", "Undead", "Flying", "Human", "Beast", "Mixed"]
  const selectedGroup = groupTypes[Math.floor(Math.random() * groupTypes.length)]

  let groupSize = Math.floor(Math.random() * 3) + 1 // 1 to 3 enemies
  const group: EnemyType[] = []

  while (groupSize > 0) {
    const enemy = generateEnemy()
    if (selectedGroup === "Mixed" || enemy.group === selectedGroup) {
      group.push(enemy)
      groupSize--
    }
  }

  return group
}