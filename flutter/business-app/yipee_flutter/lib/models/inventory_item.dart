class InventoryItem {
  final String id;
  final String name;
  int quantity;
  final double price;
  final bool urgent;

  InventoryItem({
    required this.id,
    required this.name,
    required this.quantity,
    required this.price,
    this.urgent = false,
  });
}