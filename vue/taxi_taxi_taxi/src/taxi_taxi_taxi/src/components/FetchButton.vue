<template>
  <q-btn color="primary" label="Fetch Taxi Data" @click="$emit('fetch')" />
</template>

<script>
export default {
  name: 'FetchButton'
}
</script>