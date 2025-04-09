import '../models/sales_data.dart';

class SalesService {
  List<SalesData> getMonthlySales() {
    // Generate mock data for monthly sales
    return List.generate(12, (index) {
      return SalesData(
        date: DateTime(2023, index + 1, 1),
        amount: 1000 + (index * 100) + (index % 2 == 0 ? 50 : -50),
        category: 'Monthly',
      );
    });
  }

  List<SalesData> getCategorySales() {
    // Generate mock data for category sales
    return [
      SalesData(date: DateTime.now(), amount: 3000, category: 'Electronics'),
      SalesData(date: DateTime.now(), amount: 2500, category: 'Clothing'),
      SalesData(date: DateTime.now(), amount: 1500, category: 'Food'),
      SalesData(date: DateTime.now(), amount: 1000, category: 'Books'),
    ];
  }

  List<SalesData> getWeeklySales() {
    // Generate mock data for weekly sales
    return List.generate(7, (index) {
      return SalesData(
        date: DateTime.now().subtract(Duration(days: 6 - index)),
        amount: 100 + (index * 20) + (index % 2 == 0 ? 10 : -10),
        category: 'Weekly',
      );
    });
  }
}