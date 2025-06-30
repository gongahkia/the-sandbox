package com.countdown.countdownwidget.data

import com.google.firebase.database.ktx.database
import com.google.firebase.ktx.Firebase
import kotlinx.coroutines.tasks.await

class FirebaseRepository {
    private val dbRef = Firebase.database.reference.child("countdown")

    suspend fun getCountdown(): CountdownEntity? {
        return try {
            dbRef.get().await().getValue(CountdownEntity::class.java)
        } catch (e: Exception) {
            null
        }
    }

    suspend fun setCountdown(entity: CountdownEntity) {
        dbRef.setValue(entity).await()
    }

    suspend fun clearCountdown() {
        dbRef.removeValue().await()
    }
}