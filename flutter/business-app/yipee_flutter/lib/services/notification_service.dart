import '../models/notification.dart';

class NotificationService {
  List<BusinessNotification> getNotifications() {
    // In a real app, this would fetch data from a database or API
    return [
      BusinessNotification(
        id: '1',
        message: 'Your stock of pasta is running out',
        urgency: NotificationUrgency.high,
        timestamp: DateTime.now().subtract(Duration(hours: 2)),
      ),
      BusinessNotification(
        id: '2',
        message: 'Anticipated high demand for tomato sauce in the coming 2 weeks',
        urgency: NotificationUrgency.medium,
        timestamp: DateTime.now().subtract(Duration(days: 1)),
      ),
      BusinessNotification(
        id: '3',
        message: 'Shipping delays for cherries for the coming fall season',
        urgency: NotificationUrgency.low,
        timestamp: DateTime.now().subtract(Duration(days: 3)),
      ),
    ];
  }
}