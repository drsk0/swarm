import 'package:swarm/components/utils.dart';
import 'package:firebase_auth/firebase_auth.dart';
import 'package:flutter/material.dart';
import 'package:swarm/screens/dashboard.dart';
import 'package:swarm/screens/login.dart';

class AuthGate extends StatelessWidget {
  AuthGate();

  @override
  Widget build(BuildContext context) {
    return Scaffold(
        body: StreamBuilder(
            stream: FirebaseAuth.instance.authStateChanges().map(toMaybe),
            builder: (BuildContext ctx, AsyncSnapshot<Maybe<User>> snapshot) {
              return withSnapshot<Maybe<User>>(
                  context: ctx,
                  snapshot: snapshot,
                  builder: (Maybe<User> user) {
                    Widget _child = Login();
                    if (user is Just) {
                      _child = Dashboard((user as Just).content);
                    }
                    return _child;
                  });
            }));
  }
}
