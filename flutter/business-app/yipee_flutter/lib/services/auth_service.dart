// ----- simulated login code -----

import 'package:flutter/foundation.dart';

class AuthService extends ChangeNotifier {
  final Map<String, String> _users = {
    'testuser': 'testpassword',
  };

  Future<bool> signIn(String username, String password) async {
    await Future.delayed(Duration(seconds: 1));
    if (_users.containsKey(username) && _users[username] == password) {
      notifyListeners();
      return true;
    }
    return false;
  }

  Future<bool> register(String email, String password) async {
    await Future.delayed(Duration(seconds: 1));
    if (_users.containsKey(email)) {
      return false; 
    }
    _users[email] = password;
    notifyListeners();
    return true;
  }

  Future<void> signOut() async {
    await Future.delayed(Duration(seconds: 1));
    notifyListeners();
  }
}

// ----- actual firebase linking code -----

// import 'package:firebase_auth/firebase_auth.dart';
// import 'package:flutter/foundation.dart';

// class AuthService extends ChangeNotifier {
//   final FirebaseAuth _auth = FirebaseAuth.instance;

//   Future<UserCredential?> signIn(String username, String password) async {
//     // return a mock usercredential for testing for now
//     // ~ gong
//     if (username == "testuser" && password == "testpassword") { 
//       return UserCredential(user: User(uid: 'test-uid'), credential: null);
//     }
//     try {
//       UserCredential userCredential = await _auth.signInWithEmailAndPassword(
//         email: username,
//         password: password,
//       );
//       notifyListeners();
//       return userCredential;
//     } on FirebaseAuthException catch (e) {
//       print(e.message);
//       return null;
//     }
//   }

//   Future<UserCredential?> register(String email, String password) async {
//     try {
//       UserCredential userCredential = await _auth.createUserWithEmailAndPassword(
//         email: email,
//         password: password,
//       );
//       notifyListeners();
//       return userCredential;
//     } on FirebaseAuthException catch (e) {
//       print(e.message);
//       return null;
//     }
//   }

//   Future<void> signOut() async {
//     await _auth.signOut();
//     notifyListeners();
//   }
// }