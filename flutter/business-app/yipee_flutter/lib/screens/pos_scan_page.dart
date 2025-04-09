import 'package:flutter/material.dart';
import 'package:image_picker/image_picker.dart';

class POSScanPage extends StatelessWidget {
  final ImagePicker _picker = ImagePicker();

  Future<void> _scanPOSSlip() async {
    final XFile? image = await _picker.pickImage(source: ImageSource.camera);
    if (image != null) {
      // Process the scanned image
      print('POS slip scanned: ${image.path}');
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text('Scan POS Slip')),
      body: Center(
        child: ElevatedButton(
          child: Icon(Icons.camera_alt),
          onPressed: _scanPOSSlip,
        ),
      ),
    );
  }
}