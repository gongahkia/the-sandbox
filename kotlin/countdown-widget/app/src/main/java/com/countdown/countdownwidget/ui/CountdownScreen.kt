package com.countdown.countdownwidget.ui

import androidx.compose.foundation.layout.*
import androidx.compose.material3.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import com.countdown.countdownwidget.data.CountdownEntity
import com.countdown.countdownwidget.data.FirebaseRepository
import com.countdown.countdownwidget.utils.TimeUtils
import kotlinx.coroutines.launch
import java.time.LocalDateTime

@Composable
fun CountdownScreen() {
    val repo = remember { FirebaseRepository() }
    var countdown by remember { mutableStateOf<CountdownEntity?>(null) }
    var title by remember { mutableStateOf("") }
    var dateTime by remember { mutableStateOf(LocalDateTime.now().plusDays(1)) }
    val coroutineScope = rememberCoroutineScope()
    var isLoading by remember { mutableStateOf(false) }

    LaunchedEffect(Unit) {
        isLoading = true
        countdown = repo.getCountdown()
        isLoading = false
    }

    Column(
        modifier = Modifier
            .fillMaxSize()
            .padding(24.dp),
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.Center
    ) {
        Text("Minimalist Countdown", style = MaterialTheme.typography.titleLarge)
        Spacer(Modifier.height(16.dp))

        if (isLoading) {
            CircularProgressIndicator()
        } else {
            countdown?.let {
                val remaining = TimeUtils.calculateRemaining(
                    CountdownEntity.toLocalDateTime(it.targetDateTime)
                )
                Text(
                    text = it.title,
                    style = MaterialTheme.typography.titleMedium
                )
                Text(
                    text = TimeUtils.formatTime(remaining),
                    style = MaterialTheme.typography.bodyLarge
                )
                Spacer(Modifier.height(16.dp))
                Button(onClick = {
                    coroutineScope.launch {
                        repo.clearCountdown()
                        countdown = null
                    }
                }) {
                    Text("Clear Countdown")
                }
            } ?: run {
                OutlinedTextField(
                    value = title,
                    onValueChange = { title = it },
                    label = { Text("Event Title") }
                )
                Spacer(Modifier.height(8.dp))
                // For brevity, just use now+1 for demo; implement a real date/time picker in a real app
                Button(onClick = {
                    coroutineScope.launch {
                        val entity = CountdownEntity.fromLocal(title, dateTime)
                        repo.setCountdown(entity)
                        countdown = entity
                    }
                }, enabled = title.isNotBlank()) {
                    Text("Set Countdown")
                }
            }
        }
    }
}