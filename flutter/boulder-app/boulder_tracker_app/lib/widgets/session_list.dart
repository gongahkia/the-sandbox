import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'package:bouldering_tracker/providers/session_provider.dart';
import 'package:bouldering_tracker/models/bouldering_session.dart';

class SessionList extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Consumer<SessionProvider>(
      builder: (context, sessionProvider, child) {
        final sessions = sessionProvider.sessions;
        return ListView.builder(
          itemCount: sessions.length,
          itemBuilder: (context, index) {
            final session = sessions[index];
            return ListTile(
              title: Text('${session.gym} - ${session.difficulty}'),
              subtitle: Text('Tries: ${session.tries}, Terrain: ${session.terrainType}'),
              trailing: Text(session.date.toString().split(' ')[0]),
              onTap: () {
                // Add logic to view session details
              },
            );
          },
        );
      },
    );
  }
}