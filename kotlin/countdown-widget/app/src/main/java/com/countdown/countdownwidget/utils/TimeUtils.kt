package com.countdown.countdownwidget.utils

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

object TimeUtils {
    data class TimeRemaining(
        val years: Long,
        val months: Long,
        val days: Long,
        val hours: Long,
        val minutes: Long,
        val seconds: Long
    )

    fun calculateRemaining(target: LocalDateTime): TimeRemaining {
        val now = LocalDateTime.now()
        if (now.isAfter(target)) {
            return TimeRemaining(0,0,0,0,0,0)
        }
        var temp = now
        val years = temp.until(target, ChronoUnit.YEARS)
        temp = temp.plusYears(years)
        val months = temp.until(target, ChronoUnit.MONTHS)
        temp = temp.plusMonths(months)
        val days = temp.until(target, ChronoUnit.DAYS)
        temp = temp.plusDays(days)
        val hours = temp.until(target, ChronoUnit.HOURS)
        temp = temp.plusHours(hours)
        val minutes = temp.until(target, ChronoUnit.MINUTES)
        temp = temp.plusMinutes(minutes)
        val seconds = temp.until(target, ChronoUnit.SECONDS)
        return TimeRemaining(years, months, days, hours, minutes, seconds)
    }

    fun formatTime(remaining: TimeRemaining): String {
        return buildString {
            if (remaining.years > 0) append("${remaining.years}y ")
            if (remaining.months > 0) append("${remaining.months}mo ")
            if (remaining.days > 0) append("${remaining.days}d ")
            if (remaining.hours > 0) append("${remaining.hours}h ")
            if (remaining.minutes > 0) append("${remaining.minutes}m ")
            append("${remaining.seconds}s")
        }.trim()
    }
}