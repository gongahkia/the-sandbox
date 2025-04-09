// ----- simulated code -----

import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'screens/landing_page.dart';
import 'services/auth_service.dart';
import 'services/inventory_service.dart';
import 'services/notification_service.dart';
import 'services/sales_service.dart';

void main() {
  runApp(MyApp());
}

class MyApp extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return MultiProvider(
      providers: [
        ChangeNotifierProvider(create: (_) => AuthService()),
        Provider(create: (_) => InventoryService()),
        Provider(create: (_) => NotificationService()),
        Provider(create: (_) => SalesService()),
      ],
      child: MaterialApp(
        title: 'Business Auth App',
        theme: ThemeData(
          primarySwatch: Colors.blue,
        ),
        home: LandingPage(),
      ),
    );
  }
}

// ----- actual firebase linking code -----

// import 'package:flutter/material.dart';
// import 'package:firebase_core/firebase_core.dart';
// import 'package:provider/provider.dart';
// import 'firebase_options.dart';
// import 'screens/landing_page.dart';
// import 'services/auth_service.dart';
// import 'services/inventory_service.dart';
// import 'services/notification_service.dart';
// import 'services/sales_service.dart';

// void main() async {
//   WidgetsFlutterBinding.ensureInitialized();
//   await Firebase.initializeApp(
//     options: DefaultFirebaseOptions.currentPlatform,
//   );
//   runApp(MyApp());
// }

// class MyApp extends StatelessWidget {
//   @override
//   Widget build(BuildContext context) {
//     return MultiProvider(
//       providers: [
//         ChangeNotifierProvider(create: (_) => AuthService()),
//         Provider(create: (_) => InventoryService()),
//         Provider(create: (_) => NotificationService()),
//         Provider(create: (_) => SalesService()),
//       ],
//       child: MaterialApp(
//         title: 'Business Auth App',
//         theme: ThemeData(
//           primarySwatch: Colors.blue,
//         ),
//         home: LandingPage(),
//       ),
//     );
//   }
// }