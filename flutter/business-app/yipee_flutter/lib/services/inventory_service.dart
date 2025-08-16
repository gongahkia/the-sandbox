import '../models/inventory_item.dart';

class InventoryService {
  List<InventoryItem> getInventory() {
    // In a real app, this would fetch data from a database or API
    return [
      InventoryItem(id: '1', name: 'Pasta', quantity: 50, price: 2.5, urgent: true),
      InventoryItem(id: '2', name: 'Tomato Sauce', quantity: 30, price: 1.5),
      InventoryItem(id: '3', name: 'Cheese', quantity: 20, price: 3.0, urgent: true),
    ];
  }

  void addItem(InventoryItem item) {
    // Add item to database or API
  }

  void updateItem(InventoryItem item) {
    // Update item in database or API
  }

  void deleteItem(String id) {
    // Delete item from database or API
  }
}