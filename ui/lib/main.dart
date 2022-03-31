import 'package:swarm/components/auth_gate.dart';
import 'package:flutter/material.dart';
import 'package:firebase_core/firebase_core.dart';
import 'firebase_options.dart';

void main() => runApp(MyApp());

class MyApp extends StatelessWidget {
  MyApp();
  // Create the initialization Future outside of `build`:
  Future<FirebaseApp> _initialization() async {
    WidgetsFlutterBinding.ensureInitialized();
    return Firebase.initializeApp(
      options: DefaultFirebaseOptions.currentPlatform,
    );
  }

  @override
  Widget build(BuildContext context) {
    final ThemeData _theme = ThemeData(
        primaryColor: Color(0xff263238),
        primaryColorLight: Color(0xff4f5b62),
        primaryColorDark: Color(0xff000a12),
        appBarTheme: AppBarTheme(backgroundColor: Colors.black));
    return FutureBuilder<FirebaseApp>(
        future: _initialization(),
        builder: (context, snapshot) {
          Widget child = Center(
              child: Text('loading ...',
                  style: Theme.of(context).textTheme.bodyText2));
          if (snapshot.hasData) child = AuthGate();
          return MaterialApp(title: '~~SWARM~~', theme: _theme, home: child);
        });
  }
}
