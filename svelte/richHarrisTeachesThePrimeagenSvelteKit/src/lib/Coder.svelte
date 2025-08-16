<script>

    let userCode = '';

</script>

<input bind:value={userCode} />

<div>

    <code style="display: flex;justify-content: center;">
        {userCode || 'console.log("balls")'}
    </code>

</div>