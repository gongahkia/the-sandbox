import 'package:flutter/material.dart';

class LegalDisclaimer extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Container(
      padding: EdgeInsets.all(8.0),
      decoration: BoxDecoration(
        border: Border.all(color: Colors.grey),
        borderRadius: BorderRadius.circular(8.0),
      ),
      child: Text(
        'By registering, you agree to our Terms of Service and Privacy Policy. '
        'You certify that all information provided is accurate and complete. '
        'You understand that false information may result in termination of services.',
        style: TextStyle(fontSize: 12),
      ),
    );
  }
}