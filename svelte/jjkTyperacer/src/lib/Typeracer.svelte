<script>
  import { onMount } from 'svelte';

  let completed = false;
  const givenQuote = "Are you the strongest because you're Satoru Gojo, or are you Satoru Gojo because you're the strongest?";

  let untyped;
  let typed;

  onMount(() => {
    const localUntyped = localStorage.getItem("UNTYPED");
    untyped = localUntyped === null ? givenQuote : JSON.parse(localUntyped);

    const localTyped = localStorage.getItem("TYPED");
    typed = localTyped === null ? "" : JSON.parse(localTyped);
  });

  $: {
    localStorage.setItem("TYPED", JSON.stringify(typed));
    localStorage.setItem("UNTYPED", JSON.stringify(untyped));
  }

  function generateUntyped(value) {
    untyped = givenQuote.substring(value.length);
  }

  function checkProgress(event) {
    const value = event.target.value;
    typed = value;
    generateUntyped(value);
    if (value === givenQuote) {
      completed = true;
    } else {
      completed = false;
    }
  }

  function resetPage() {
    typed = "";
    untyped = givenQuote;
    completed = false;
    localStorage.removeItem("TYPED");
    localStorage.removeItem("UNTYPED");
  }
</script>

<style>
  .header {
    font-size: 24px;
    text-align: center;
  }
  .flexbox {
    display: flex;
    justify-content: center;
    margin: 10px;
  }
  .progressTyped {
    color: green;
  }
  .progressUntyped {
    color: grey;
  }
  .userInput {
    width: 100%;
    height: 100px;
  }
</style>

<h1 class="header">a simple typeracer game</h1>
<hr>

<div class="flexbox">
  <span class="progressTyped">{typed}</span>
  <span class="progressUntyped">{untyped}</span>
</div>
<hr>

<div class="flexbox">
  {#if !completed}
    <textarea 
      class="userInput" 
      bind:value={typed} 
      on:input={checkProgress}
    ></textarea>
  {:else}
    <img src="https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQHOa3ICPGUWbzdws_YMvOgHDzoG4ZSMagVOw&s" alt="Completed Image">
  {/if}
</div>

<div class="flexbox">
  {#if completed}
    <button on:click={resetPage}>try again!</button>
  {/if}
</div>
