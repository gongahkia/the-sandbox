import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'package:boulder_tracker_app/providers/session_provider.dart';
import 'package:boulder_tracker_app/widgets/session_list.dart';
import 'package:boulder_tracker_app/screens/add_session_screen.dart';

class HomeScreen extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text('Bouldering Tracker')),
      body: FutureBuilder(
        future: Provider.of<SessionProvider>(context, listen: false).loadSessions(),
        builder: (context, snapshot) {
          if (snapshot.connectionState == ConnectionState.waiting) {
            return Center(child: CircularProgressIndicator());
          }
          return SessionList();
        },
      ),
      floatingActionButton: FloatingActionButton(
        onPressed: () {
          Navigator.of(context).push(
            MaterialPageRoute(builder: (context) => AddSessionScreen()),
          );
        },
        child: Icon(Icons.add),
      ),
    );
  }
}