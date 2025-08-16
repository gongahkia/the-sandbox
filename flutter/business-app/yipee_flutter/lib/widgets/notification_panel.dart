import 'package:flutter/material.dart';
import '../models/notification.dart';
import '../services/notification_service.dart';

class NotificationPanel extends StatelessWidget {
  final NotificationService _notificationService = NotificationService();

  @override
  Widget build(BuildContext context) {
    List<BusinessNotification> notifications = _notificationService.getNotifications();

    return Container(
      color: Colors.grey[200],
      child: Column(
        children: [
          Padding(
            padding: EdgeInsets.all(8.0),
            child: Text('Notifications', style: TextStyle(fontSize: 18, fontWeight: FontWeight.bold)),
          ),
          Expanded(
            child: ListView.builder(
              itemCount: notifications.length,
              itemBuilder: (context, index) {
                final notification = notifications[index];
                return ListTile(
                  leading: Icon(_getUrgencyIcon(notification.urgency), color: _getUrgencyColor(notification.urgency)),
                  title: Text(notification.message),
                  subtitle: Text(notification.timestamp.toString()),
                );
              },
            ),
          ),
        ],
      ),
    );
  }

  IconData _getUrgencyIcon(NotificationUrgency urgency) {
    switch (urgency) {
      case NotificationUrgency.low:
        return Icons.info_outline;
      case NotificationUrgency.medium:
        return Icons.warning_amber_rounded;
      case NotificationUrgency.high:
        return Icons.error_outline;
    }
  }

  Color _getUrgencyColor(NotificationUrgency urgency) {
    switch (urgency) {
      case NotificationUrgency.low:
        return Colors.blue;
      case NotificationUrgency.medium:
        return Colors.orange;
      case NotificationUrgency.high:
        return Colors.red;
    }
  }
}