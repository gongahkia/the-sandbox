<template>
  <div>
    <h2 class="text-h5">Available Taxis: {{ taxiData.length }}</h2>
    <q-list bordered>
      <q-item v-for="taxi in taxiData.slice(0, 10)" :key="taxi.timestamp">
        <q-item-section>
          Latitude: {{ taxi[1] }}, Longitude: {{ taxi[0] }}
        </q-item-section>
      </q-item>
    </q-list>
  </div>
</template>

<script>
export default {
  name: 'TaxiList',
  props: {
    taxiData: {
      type: Array,
      required: true
    }
  }
}
</script>