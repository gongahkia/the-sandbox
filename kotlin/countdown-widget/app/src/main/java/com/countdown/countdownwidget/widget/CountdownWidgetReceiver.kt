package com.countdown.countdownwidget.widget

import androidx.glance.appwidget.GlanceAppWidgetReceiver

class CountdownWidgetReceiver : GlanceAppWidgetReceiver() {
    override val glanceAppWidget = CountdownGlanceWidget()
}