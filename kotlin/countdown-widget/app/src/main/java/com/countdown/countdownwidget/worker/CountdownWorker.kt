package com.countdown.countdownwidget.worker

import android.content.Context
import androidx.work.CoroutineWorker
import androidx.work.WorkerParameters
import com.countdown.countdownwidget.widget.CountdownGlanceWidget

class CountdownWorker(
    appContext: Context,
    params: WorkerParameters
) : CoroutineWorker(appContext, params) {
    override suspend fun doWork(): Result {
        CountdownGlanceWidget().update(applicationContext)
        return Result.success()
    }
}