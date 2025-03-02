import 'package:flutter/material.dart';
import 'package:boulder_tracker_app/widgets/add_session_form.dart';

class AddSessionScreen extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text('Add Bouldering Session')),
      body: SingleChildScrollView(
        padding: EdgeInsets.all(16.0),
        child: AddSessionForm(),
      ),
    );
  }
}