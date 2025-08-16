<script>
  export let piece;
  export let onClick;

  const { name, ability } = piece;
</script>

<div class="piece" on:click={onClick}>
  <img src={`/pieces/${name}.png`} alt={name} />
  <div class="description">
    <strong>{name}</strong>
    <p>{ability}</p>
  </div>
</div>

<style>
  .piece {
    cursor: pointer;
    text-align: center;
  }

  .piece img {
    width: 50px;
    height: 50px;
  }

  .description {
    margin-top: 5px;
  }
</style>
