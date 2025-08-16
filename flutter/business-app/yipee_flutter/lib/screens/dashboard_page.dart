import 'package:flutter/material.dart';
import 'inventory_page.dart';
import 'pos_scan_page.dart';
import 'sales_analytics_page.dart';
import '../widgets/notification_panel.dart';

class DashboardPage extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text('Dashboard')),
      body: Row(
        children: [
          Expanded(
            flex: 3,
            child: Column(
              children: [
                ElevatedButton(
                  child: Text('Inventory Management'),
                  onPressed: () => Navigator.push(
                    context,
                    MaterialPageRoute(builder: (context) => InventoryPage()),
                  ),
                ),
                ElevatedButton(
                  child: Text('Scan POS Slip'),
                  onPressed: () => Navigator.push(
                    context,
                    MaterialPageRoute(builder: (context) => POSScanPage()),
                  ),
                ),
                ElevatedButton(
                  child: Text('Sales Analytics'),
                  onPressed: () => Navigator.push(
                    context,
                    MaterialPageRoute(builder: (context) => SalesAnalyticsPage()),
                  ),
                ),
              ],
            ),
          ),
          Expanded(
            flex: 1,
            child: NotificationPanel(),
          ),
        ],
      ),
    );
  }
}