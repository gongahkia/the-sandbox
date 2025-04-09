import 'package:flutter/material.dart';
import 'package:fl_chart/fl_chart.dart';
import '../models/sales_data.dart';

class SalesLineChart extends StatelessWidget {
  final List<SalesData> salesData;

  SalesLineChart({required this.salesData});

  @override
  Widget build(BuildContext context) {
    return Container(
      height: 300,
      padding: EdgeInsets.all(16),
      child: LineChart(
        LineChartData(
          lineBarsData: [
            LineChartBarData(
              spots: salesData.map((data) => FlSpot(data.date.month.toDouble(), data.amount)).toList(),
              isCurved: true,
              colors: [Colors.blue],
              dotData: FlDotData(show: false),
            ),
          ],
          titlesData: FlTitlesData(
            bottomTitles: SideTitles(
              showTitles: true,
              getTitles: (value) {
                switch (value.toInt()) {
                  case 1: return 'Jan';
                  case 4: return 'Apr';
                  case 7: return 'Jul';
                  case 10: return 'Oct';
                  default: return '';
                }
              },
            ),
          ),
        ),
      ),
    );
  }
}

class SalesPieChart extends StatelessWidget {
  final List<SalesData> salesData;

  SalesPieChart({required this.salesData});

  @override
  Widget build(BuildContext context) {
    return Container(
      height: 300,
      padding: EdgeInsets.all(16),
      child: PieChart(
        PieChartData(
          sections: salesData.map((data) => PieChartSectionData(
            value: data.amount,
            title: data.category,
            color: Colors.primaries[salesData.indexOf(data) % Colors.primaries.length],
          )).toList(),
        ),
      ),
    );
  }
}

class SalesBarChart extends StatelessWidget {
  final List<SalesData> salesData;

  SalesBarChart({required this.salesData});

  @override
  Widget build(BuildContext context) {
    return Container(
      height: 300,
      padding: EdgeInsets.all(16),
      child: BarChart(
        BarChartData(
          alignment: BarChartAlignment.spaceAround,
          maxY: salesData.map((data) => data.amount).reduce((a, b) => a > b ? a : b),
          barTouchData: BarTouchData(enabled: false),
          titlesData: FlTitlesData(
            bottomTitles: SideTitles(
              showTitles: true,
              getTitles: (value) => salesData[value.toInt()].date.weekday.toString(),
            ),
          ),
          barGroups: salesData.asMap().entries.map((entry) {
            return BarChartGroupData(
              x: entry.key,
              barRods: [BarChartRodData(y: entry.value.amount, colors: [Colors.blue])],
            );
          }).toList(),
        ),
      ),
    );
  }
}