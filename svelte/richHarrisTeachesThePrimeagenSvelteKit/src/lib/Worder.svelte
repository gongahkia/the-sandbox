<script>

    let name='';

</script>

<input type="text" placeholder='shit the bed' bind:value={name}/>

<p>watermelon sugar balls {name || 'stranger'}! </p> <!-- second option after || evaluates if left empty -->