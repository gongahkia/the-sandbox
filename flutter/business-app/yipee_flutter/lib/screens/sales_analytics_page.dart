import 'package:flutter/material.dart';
import '../widgets/sales_charts.dart';
import '../services/sales_service.dart';

class SalesAnalyticsPage extends StatelessWidget {
  final SalesService _salesService = SalesService();

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text('Sales Analytics')),
      body: SingleChildScrollView(
        child: Column(
          children: [
            SalesLineChart(salesData: _salesService.getMonthlySales()),
            SalesPieChart(salesData: _salesService.getCategorySales()),
            SalesBarChart(salesData: _salesService.getWeeklySales()),
          ],
        ),
      ),
    );
  }
}