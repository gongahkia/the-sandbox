import 'dart:convert';
import 'package:flutter/foundation.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:boulder_tracker_app/models/bouldering_session.dart';

class SessionProvider with ChangeNotifier {
  List<BoulderingSession> _sessions = [];

  List<BoulderingSession> get sessions => [..._sessions];

  Future<void> loadSessions() async {
    final prefs = await SharedPreferences.getInstance();
    final sessionsJson = prefs.getString('sessions');
    if (sessionsJson != null) {
      final List<dynamic> decodedList = json.decode(sessionsJson);
      _sessions = decodedList.map((item) => BoulderingSession.fromJson(item)).toList();
      notifyListeners();
    }
  }

  Future<void> addSession(BoulderingSession session) async {
    _sessions.add(session);
    await _saveSessions();
    notifyListeners();
  }

  Future<void> _saveSessions() async {
    final prefs = await SharedPreferences.getInstance();
    final sessionsJson = json.encode(_sessions.map((s) => s.toJson()).toList());
    await prefs.setString('sessions', sessionsJson);
  }
}