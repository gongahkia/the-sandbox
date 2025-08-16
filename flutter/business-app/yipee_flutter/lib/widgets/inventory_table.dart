import 'package:flutter/material.dart';
import '../models/inventory_item.dart';
import '../services/inventory_service.dart';

class InventoryTable extends StatefulWidget {
  final InventoryService inventoryService;

  InventoryTable({required this.inventoryService});

  @override
  _InventoryTableState createState() => _InventoryTableState();
}

class _InventoryTableState extends State<InventoryTable> {
  late List<InventoryItem> _inventory;
  bool _sortAscending = true;
  int _sortColumnIndex = 0;

  @override
  void initState() {
    super.initState();
    _inventory = widget.inventoryService.getInventory();
  }

  void _sort<T>(Comparable<T> Function(InventoryItem i) getField, int columnIndex, bool ascending) {
    _inventory.sort((a, b) {
      final aValue = getField(a);
      final bValue = getField(b);
      return ascending ? Comparable.compare(aValue, bValue) : Comparable.compare(bValue, aValue);
    });
    setState(() {
      _sortColumnIndex = columnIndex;
      _sortAscending = ascending;
    });
  }

  @override
  Widget build(BuildContext context) {
    return SingleChildScrollView(
      scrollDirection: Axis.horizontal,
      child: DataTable(
        sortColumnIndex: _sortColumnIndex,
        sortAscending: _sortAscending,
        columns: [
          DataColumn(
            label: Text('Name'),
            onSort: (columnIndex, ascending) => _sort((i) => i.name, columnIndex, ascending),
          ),
          DataColumn(
            label: Text('Quantity'),
            numeric: true,
            onSort: (columnIndex, ascending) => _sort((i) => i.quantity, columnIndex, ascending),
          ),
          DataColumn(
            label: Text('Price'),
            numeric: true,
            onSort: (columnIndex, ascending) => _sort((i) => i.price, columnIndex, ascending),
          ),
          DataColumn(
            label: Text('Urgent'),
            onSort: (columnIndex, ascending) => _sort((i) => i.urgent ? 1 : 0, columnIndex, ascending),
          ),
        ],
        rows: _inventory.map((item) => DataRow(
          cells: [
            DataCell(Text(item.name)),
            DataCell(Text(item.quantity.toString())),
            DataCell(Text('\$${item.price.toStringAsFixed(2)}')),
            DataCell(Icon(item.urgent ? Icons.warning : Icons.check, color: item.urgent ? Colors.red : Colors.green)),
          ],
        )).toList(),
      ),
    );
  }
}