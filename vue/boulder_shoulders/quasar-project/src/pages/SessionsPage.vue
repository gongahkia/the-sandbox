<template>
  <q-page class="nord-page q-pa-md">
    <h1 class="nord-title">Boulder Tracker</h1>
    
    <q-form @submit.prevent="addSession" class="nord-form">
      <q-select 
        v-model="session.gym" 
        :options="GYMS" 
        label="Select Gym" 
        outlined 
        class="nord-select"
      />
      <q-input 
        v-model="session.difficulty" 
        label="Difficulty" 
        outlined 
        class="nord-input"
      />
      <q-input 
        v-model="session.tries" 
        type="number" 
        label="Number of Tries" 
        outlined 
        class="nord-input"
      />
      <q-input 
        v-model="session.description" 
        label="Description" 
        outlined 
        class="nord-input"
      />
      <q-select 
        v-model="session.terrain" 
        :options="terrains" 
        label="Terrain Type" 
        outlined 
        class="nord-select"
      />

      <q-btn
        :disable="!session.gym || !session.difficulty || !session.tries || !session.terrain"
        type="submit"
        label="Add Session"
        color="primary"
        class="nord-button q-mt-md"
      />
    </q-form>

    <div class="nord-sessions q-mt-xl">
      <h2 class="nord-subtitle">Logged Sessions</h2>
      <q-list bordered class="nord-list">
        <q-item v-for="(item, index) in sessions" :key="index" class="nord-item">
          <q-item-section>
            <div class="nord-item-content">
              <span class="gym">{{ item.gym }}</span>
              <span class="difficulty">{{ item.difficulty }}</span>
              <span class="terrain">{{ item.terrain }}</span>
              <span class="tries">{{ item.tries }} tries</span>
            </div>
            <div class="description">{{ item.description }}</div>
          </q-item-section>
        </q-item>
      </q-list>
    </div>
  </q-page>
</template>

<script lang="ts">
export default {
  data() {
    return {
      GYMS: [
        "Adventure HQ (Rockpass)", "Ark Bloc", "BFF Climb (Bendemeer)",
        "BFF Climb (Tampines OTH)", "BFF Climb (Tampines yo:HA)", "Boruda",
        "Boulder Movement (Bugis+)", "Boulder Movement (OUE Downtown)",
        "Boulder Movement (Tai Seng)", "Boulder Movement (Tekka Place)",
        "Boulder+ (Aperia Mall)", "Boulder+ (The Chevrons)",
        "Boulder Planet (Sembawang)", "Boulder Planet (Tai Seng)",
        "Boulder World (Paragon)", "Boys Town Adventure Centre",
        "Climb@T3", "Climba Gym", "Climb Central (Katong)",
        "Climb Central (Novena)"
      ],
      terrains: ["Overhang", "Coordination", "Slab", "Straight Wall"],
      session: {
        gym: null as string | null,
        difficulty: "" as string,
        tries: null as number | null,
        description: "" as string,
        terrain: null as string | null,
      },
      sessions: [] as Array<{
        gym: string | null;
        difficulty: string;
        tries: number | null;
        description: string;
        terrain: string | null;
      }>, // Explicitly define the type here
    };
  },
  methods: {
    addSession() {
      // Validation logic
      if (!this.session.gym || !this.session.difficulty || !this.session.tries || !this.session.terrain) {
        alert("Please fill in all required fields before submitting.");
        return;
      }

      // Add session to list
      this.sessions.push({ ...this.session });

      // Reset the form
      this.session = {
        gym: null,
        difficulty: "",
        tries: null,
        description: "",
        terrain: null,
      };
    },
  },
};
</script>

<style lang="scss">
.nord-page {
  background-color: #2e3440; /* Nord background */
  font-family: 'Arial', sans-serif;
}

.nord-title {
  font-size: 3rem;
  font-weight: bold;
  color: #81a1c1; /* Nord text color */
  text-transform: uppercase;
  margin-bottom: 2rem;
  text-shadow: 3px 3px 0 #434c5e; /* Nord shadow */
}

.nord-form {
  background-color: #3b4252; /* Nord form background */
  padding: 2rem;
  border: 2px solid #4c566a; /* Nord border */
  box-shadow: 8px 8px 0 #434c5e; /* Nord shadow */
}

.nord-select, .nord-input {
  margin-bottom: 1rem;
  border: 2px solid #4c566a !important; /* Nord border */
  
  .q-field__native, .q-field__input {
    color: #81a1c1; /* Nord text color */
    font-weight: bold;
    background-color: #2e3440; /* Nord background */
  }
}

.nord-button {
  background-color: #5e81ac !important; /* Nord button background */
  color: #2e3440 !important; /* Nord button text */
  font-weight: bold;
  border: 2px solid #4c566a; /* Nord border */
  box-shadow: 4px 4px 0 #434c5e; /* Nord shadow */
  transition: all 0.3s ease;

  &:hover {
    transform: translate(-2px, -2px);
    box-shadow: 6px 6px 0 #434c5e;
  }
}

.nord-sessions {
  background-color: #434c5e; /* Nord sessions background */
  padding: 2rem;
  border: 2px solid #4c566a; /* Nord border */
  box-shadow: 8px 8px 0 #434c5e; /* Nord shadow */
}

.nord-subtitle {
  font-size: 2rem;
  font-weight: bold;
  color: #81a1c1; /* Nord text color */
  margin-bottom: 1rem;
}

.nord-list {
  border: none !important;
}

.nord-item {
  background-color: #3b4252; /* Nord item background */
  margin-bottom: 1rem;
  border: 2px solid #4c566a; /* Nord border */
  padding: 1rem;

  .nord-item-content {
    display: flex;
    justify-content: space-between;
    font-weight: bold;
    margin-bottom: 0.5rem;

    .gym { color: #5e81ac; } /* Nord blue */
    .difficulty { color: #8fbcbb; } /* Nord greenish-blue */
    .terrain { color: #88c0d0; } /* Nord light blue */
    .tries { color: #81a1c1; } /* Nord text color */
  }

  .description {
    font-style: italic;
    color: #81a1c1; /* Nord text color */
  }

}
</style>