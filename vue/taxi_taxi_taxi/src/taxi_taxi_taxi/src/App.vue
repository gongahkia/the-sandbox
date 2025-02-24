<template>
  <q-layout view="lHh Lpr lFf">
    <q-page-container>
      <q-page class="flex flex-center">
        <div>
          <h1 class="text-h4 q-mb-md">Singapore Taxi Availability</h1>
          <fetch-button @fetch="fetchTaxiData" />
          <taxi-list v-if="taxiData" :taxiData="taxiData" class="q-mt-md" />
        </div>
      </q-page>
    </q-page-container>
  </q-layout>
</template>

<script>
import axios from 'axios';
import TaxiList from './components/TaxiList.vue';
import FetchButton from './components/FetchButton.vue';

export default {
  name: 'App',
  components: {
    TaxiList,
    FetchButton
  },
  data() {
    return {
      taxiData: null,
    };
  },
  methods: {
    async fetchTaxiData() {
      try {
        const response = await axios.get('https://api.data.gov.sg/v1/transport/taxi-availability');
        this.taxiData = response.data.features[0].geometry.coordinates;
      } catch (error) {
        console.error('Error fetching taxi data:', error);
      }
    },
  },
};
</script>