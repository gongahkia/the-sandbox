<template>
  <div class="higher-lower-game">
    <h2>Learning Vue by making a higher lower guessing game</h2>
    <p>Guess a number between 1 and 100</p>
    <input v-model.number="guess" type="number" placeholder="Enter your guess">
    <button @click="checkGuess">Submit Guess</button>
    <p>{{ message }}</p>
    <p>Attempts: {{ attempts }}</p>
    <button @click="resetGame" v-if="gameOver">Play Again</button>
  </div>
</template>

<script>
export default {
  name: 'HigherLowerGame',
  data() {
    return {
      targetNumber: 0,
      guess: null,
      message: '',
      attempts: 0,
      gameOver: false
    }
  },
  methods: {
    generateTargetNumber() {
      this.targetNumber = Math.floor(Math.random() * 100) + 1;
    },
    checkGuess() {
      if (this.guess === null) return;
      
      this.attempts++;
      
      if (this.guess === this.targetNumber) {
        this.message = `Congratulations! You guessed the number in ${this.attempts} attempts.`;
        this.gameOver = true;
      } else if (this.guess < this.targetNumber) {
        this.message = 'Too low! Try a higher number.';
      } else {
        this.message = 'Too high! Try a lower number.';
      }
      
      this.guess = null;
    },
    resetGame() {
      this.generateTargetNumber();
      this.guess = null;
      this.message = '';
      this.attempts = 0;
      this.gameOver = false;
    }
  },
  created() {
    this.generateTargetNumber();
  }
}
</script>

<style scoped>
.higher-lower-game {
  font-family: Arial, sans-serif;
  text-align: center;
  margin-top: 20px;
}
input, button {
  margin: 10px;
  padding: 5px;
}
</style>