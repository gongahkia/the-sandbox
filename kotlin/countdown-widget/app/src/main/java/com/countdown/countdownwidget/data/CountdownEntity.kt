package com.countdown.countdownwidget.data

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

// Firebase Realtime Database requires a no-arg constructor and primitive types.
data class CountdownEntity(
    var title: String = "",
    var targetDateTime: String = "" // Store as ISO string for Firebase compatibility
) {
    companion object {
        fun fromLocal(title: String, dateTime: LocalDateTime): CountdownEntity =
            CountdownEntity(title, dateTime.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME))
        fun toLocalDateTime(targetDateTime: String): LocalDateTime =
            LocalDateTime.parse(targetDateTime, DateTimeFormatter.ISO_LOCAL_DATE_TIME)
    }
}