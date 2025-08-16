"use client"

import { useState, useEffect } from "react"
import {
  type CardType,
  type EnemyType,
  generateDeck,
  drawCards,
  generateEnemyGroup,
  shuffleDeck,
} from "../utils/gameUtils"

export const useGameState = () => {
  const [deck, setDeck] = useState<CardType[]>([])
  const [hand, setHand] = useState<CardType[]>([])
  const [discardPile, setDiscardPile] = useState<CardType[]>([])
  const [enemies, setEnemies] = useState<EnemyType[]>([])
  const [playerHealth, setPlayerHealth] = useState(50)
  const [playerMaxHealth, setPlayerMaxHealth] = useState(50)
  const [playerEnergy, setPlayerEnergy] = useState(3)
  const [playerMaxEnergy, setPlayerMaxEnergy] = useState(3)
  const [combatLog, setCombatLog] = useState<string[]>([])
  const [animationTrigger, setAnimationTrigger] = useState(0)
  const [playerLastDamageTaken, setPlayerLastDamageTaken] = useState(0)
  const [enemiesLastDamageTaken, setEnemiesLastDamageTaken] = useState<number[]>([])

  useEffect(() => {
    startNewGame()
  }, [])

  const startNewGame = () => {
    const newDeck = generateDeck()
    const [remainingDeck, newHand] = drawCards(newDeck, 5)
    setDeck(remainingDeck)
    setHand(newHand)
    setDiscardPile([])
    const newEnemies = generateEnemyGroup()
    setEnemies(newEnemies)
    setPlayerHealth(50)
    setPlayerMaxHealth(50)
    setPlayerEnergy(3)
    setPlayerMaxEnergy(3)
    setCombatLog(["A new group of enemies appears!"])
    setEnemiesLastDamageTaken(new Array(newEnemies.length).fill(0))
  }

  const playCard = (card: CardType, targetEnemyIndex: number) => {
    if (playerEnergy < card.energy) {
      setCombatLog([...combatLog, "Not enough energy to play this card!"])
      return
    }

    setPlayerEnergy(playerEnergy - card.energy)
    setHand(hand.filter((c) => c.id !== card.id))
    setDiscardPile([...discardPile, card])

    const damage = card.damage
    let newEnemies = [...enemies]
    let newPlayerHealth = playerHealth

    // Apply card effects
    if (card.effect) {
      // ... (implement card effects here)
    }

    // Deal damage to the target enemy
    newEnemies[targetEnemyIndex].health -= damage
    const newEnemiesLastDamageTaken = [...enemiesLastDamageTaken]
    newEnemiesLastDamageTaken[targetEnemyIndex] = damage
    setEnemiesLastDamageTaken(newEnemiesLastDamageTaken)

    setCombatLog([...combatLog, `${card.name} deals ${damage} damage to ${newEnemies[targetEnemyIndex].name}!`])

    // Remove defeated enemies
    newEnemies = newEnemies.filter((enemy) => enemy.health > 0)
    setEnemies(newEnemies)

    if (newEnemies.length === 0) {
      setCombatLog([...combatLog, "All enemies defeated! A new group approaches..."])
      setTimeout(() => {
        const newEnemies = generateEnemyGroup()
        setEnemies(newEnemies)
        setEnemiesLastDamageTaken(new Array(newEnemies.length).fill(0))
        setCombatLog([
          ...combatLog,
          "All enemies defeated! A new group approaches...",
          "A new group of enemies appears!",
        ])
      }, 1500)
    } else {
      // Enemies attack
      let totalEnemyDamage = 0
      newEnemies.forEach((enemy) => {
        const enemyDamage = Math.max(0, enemy.damage - card.defense)
        totalEnemyDamage += enemyDamage
      })
      newPlayerHealth -= totalEnemyDamage
      setPlayerLastDamageTaken(totalEnemyDamage)
      setCombatLog([...combatLog, `Enemies attack for ${totalEnemyDamage} total damage!`])
    }

    setPlayerHealth(newPlayerHealth)

    if (newPlayerHealth <= 0) {
      setCombatLog([...combatLog, "Game Over! You have been defeated."])
      setTimeout(startNewGame, 3000)
    }

    // Trigger animation
    setAnimationTrigger(Date.now())
  }

  const endTurn = () => {
    setDiscardPile([...discardPile, ...hand])
    let newDeck = deck
    if (newDeck.length < 5) {
      newDeck = [...newDeck, ...shuffleDeck(discardPile)]
      setDiscardPile([])
    }
    const [remainingDeck, newHand] = drawCards(newDeck, 5)
    setDeck(remainingDeck)
    setHand(newHand)
    setPlayerEnergy(playerMaxEnergy)
    setCombatLog([...combatLog, "Your turn ends. Draw 5 new cards."])

    // Trigger animation
    setAnimationTrigger(Date.now())
  }

  return {
    deck,
    hand,
    discardPile,
    enemies,
    playerHealth,
    playerMaxHealth,
    playerEnergy,
    playerMaxEnergy,
    combatLog,
    playCard,
    endTurn,
    animationTrigger,
    playerLastDamageTaken,
    enemiesLastDamageTaken,
  }
}