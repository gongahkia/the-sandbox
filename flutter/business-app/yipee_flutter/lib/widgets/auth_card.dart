import 'package:flutter/material.dart';

class AuthCard extends StatelessWidget {
  final String title;
  final VoidCallback onTap;

  AuthCard({required this.title, required this.onTap});

  @override
  Widget build(BuildContext context) {
    return Card(
      elevation: 5,
      child: InkWell(
        onTap: onTap,
        child: Container(
          width: 150,
          height: 150,
          alignment: Alignment.center,
          child: Text(
            title,
            style: TextStyle(fontSize: 24, fontWeight: FontWeight.bold),
          ),
        ),
      ),
    );
  }
}