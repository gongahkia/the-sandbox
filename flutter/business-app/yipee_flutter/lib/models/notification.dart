enum NotificationUrgency { low, medium, high }

class BusinessNotification {
  final String id;
  final String message;
  final NotificationUrgency urgency;
  final DateTime timestamp;

  BusinessNotification({
    required this.id,
    required this.message,
    required this.urgency,
    required this.timestamp,
  });
}