import 'package:firebase_auth/firebase_auth.dart';
import 'package:flutter/material.dart';
import 'package:swarm/screens/signup.dart';
import 'package:google_sign_in/google_sign_in.dart';

Future<void> signInWithGoogle() async {
  // Trigger the authentication flow
  final GoogleSignInAccount? googleUser = await GoogleSignIn().signIn();

  // Obtain the auth details from the request
  final GoogleSignInAuthentication? googleAuth =
      await googleUser?.authentication;

  // Create a new credential
  final OAuthCredential credential = GoogleAuthProvider.credential(
    accessToken: googleAuth?.accessToken,
    idToken: googleAuth?.idToken,
  );

  // Once signed in, return the UserCredential
  await FirebaseAuth.instance.signInWithCredential(credential);
}

class Login extends StatefulWidget {
  Login();
  @override
  createState() {
    GoogleSignIn().signInSilently();
    return LoginState();
  }
}

class LoginState extends State<Login> {
  final TextEditingController emailController = TextEditingController();
  final TextEditingController passwordController = TextEditingController();
  final _formKey = GlobalKey<FormState>();

  @override
  Widget build(BuildContext context) {
    return Scaffold(
        body: Container(
            // decoration: BoxDecoration(
            // image: DecorationImage(
            // image: AssetImage('assets/images/login.jpg'),
            // alignment: Alignment.bottomLeft,
            // scale: 2.0,
            // fit: BoxFit.none
            // ),
            // ),
            child: Center(
      child: Form(
        key: _formKey,
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: <Widget>[
            Container(
              width: MediaQuery.of(context).size.width / 1.3,
              child: Column(
                children: <Widget>[
                  TextFormField(
                    controller: emailController,
                    decoration: InputDecoration(labelText: "Email"),
                    keyboardType: TextInputType.emailAddress,
                    validator: (value) {
                      String pattern =
                          r'^(([^<>()[\]\\.,;:\s@\"]+(\.[^<>()[\]\\.,;:\s@\"]+)*)|(\".+\"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$';
                      RegExp regex = new RegExp(pattern);
                      if (value != null && !regex.hasMatch(value))
                        return 'Enter Valid Email';
                      else
                        return null;
                    },
                  ),
                  TextFormField(
                    controller: passwordController,
                    decoration: InputDecoration(labelText: "Password"),
                    validator: (value) {
                      return value != null && value.length < 4
                          ? "Password must be at least 4 characters long"
                          : null;
                    },
                    obscureText: true,
                  ),
                ],
              ),
            ),
            ButtonBar(
              alignment: MainAxisAlignment.center,
              children: <Widget>[
                IconButton(
                    icon: Icon(Icons.login),
                    onPressed: () async {
                      if (_formKey.currentState != null &&
                          _formKey.currentState!.validate()) {
                        try {
                          await FirebaseAuth.instance
                              .signInWithEmailAndPassword(
                                  email: emailController.text,
                                  password: passwordController.text);
                        } on FirebaseAuthException catch (e) {
                          if (e.code == 'user-not-found') {
                            print('No user found for that email.');
                          } else if (e.code == 'wrong-password') {
                            print('Wrong password provided for that user.');
                          }
                        }
                      }
                    }),
                // RaisedButton(
                // child: Text('Anonymous'),
                // onPressed: () async {
                // print('signing in');
                // try {
                // _credentials = await FirebaseAuth.instance
                // .signInAnonymously();
                // } on FirebaseAuthException catch (e) {
                // print(e.message);
                // }
                // if (_credentials != null) {
                // print('loading dashboard');
                // await Navigator.push(
                // context,
                // MaterialPageRoute(
                // builder: (context) =>
                // Splash()));
                // } else {
                // print('error');
                // }
                // }),
              ],
            ),
            Padding(
              padding: const EdgeInsets.all(10.0),
              child: TextButton(
                onPressed: () async {
                  print('signing in');
                  try {
                    await signInWithGoogle();
                  } on FirebaseAuthException catch (e) {
                    print(e.message);
                  }
                },
                child: Container(
                  height: 20,
                  child: Text(
                    "Login with Google",
                    style: TextStyle(
                        decoration: TextDecoration.underline,
                        color: Colors.black),
                  ),
                ),
              ),
            ),
            Padding(
              padding: const EdgeInsets.all(10.0),
              child: TextButton(
                onPressed: () {
                  Navigator.push(context,
                      MaterialPageRoute(builder: (context) => Signup()));
                },
                child: Container(
                  height: 20,
                  child: Text(
                    "Be part of the swarm, sign up!",
                    style: TextStyle(
                        decoration: TextDecoration.underline,
                        color: Colors.black),
                  ),
                ),
              ),
            )
          ],
        ),
      ),
    )));
  }
}
