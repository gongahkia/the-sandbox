import 'package:flutter/material.dart';
import '../widgets/inventory_table.dart';
import '../services/inventory_service.dart';

class InventoryPage extends StatelessWidget {
  final InventoryService _inventoryService = InventoryService();

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text('Inventory Management')),
      body: InventoryTable(inventoryService: _inventoryService),
      floatingActionButton: FloatingActionButton(
        child: Icon(Icons.add),
        onPressed: () {
          // Show dialog to add new inventory item
        },
      ),
    );
  }
}