import { createApp } from 'vue'
import { Quasar } from 'quasar'
import quasarUserOptions from './quasar-user-options'
import App from './App.vue'

createApp(App)
  .use(Quasar, quasarUserOptions)
  .mount('#app')