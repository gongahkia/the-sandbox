<!-- make something a component that we seperate into its own file -->

<script>

  let count = 0;
  $: tripled = count * 3; // specifies that tripled is DEPENDANT on the count variable

  // anonymous function call
  const increment = () => {
    count += 1
  }

</script>

<button on:click={increment}>
    count is {count}<br>
    triple the count is {tripled}
</button>
