package com.countdown.countdownwidget.widget

import android.content.Context
import androidx.glance.GlanceId
import androidx.glance.GlanceModifier
import androidx.glance.appwidget.GlanceAppWidget
import androidx.glance.appwidget.provideContent
import androidx.glance.layout.Alignment
import androidx.glance.layout.Box
import androidx.glance.layout.Column
import androidx.glance.layout.fillMaxSize
import androidx.glance.text.Text
import com.countdown.countdownwidget.data.FirebaseRepository
import com.countdown.countdownwidget.data.CountdownEntity
import com.countdown.countdownwidget.utils.TimeUtils
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.runBlocking

class CountdownGlanceWidget : GlanceAppWidget() {
    override suspend fun provideGlance(context: Context, id: GlanceId) {
        provideContent {
            val entity = runBlocking(Dispatchers.IO) { FirebaseRepository().getCountdown() }
            Box(
                modifier = GlanceModifier.fillMaxSize(),
                contentAlignment = Alignment.Center
            ) {
                Column(horizontalAlignment = Alignment.CenterHorizontally) {
                    if (entity != null) {
                        val remaining = TimeUtils.calculateRemaining(
                            CountdownEntity.toLocalDateTime(entity.targetDateTime)
                        )
                        Text(entity.title)
                        Text(TimeUtils.formatTime(remaining))
                    } else {
                        Text("No countdown")
                    }
                }
            }
        }
    }
}