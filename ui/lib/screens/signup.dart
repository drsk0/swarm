import 'package:swarm/components/utils.dart';
import 'package:flutter/material.dart';
import 'package:firebase_auth/firebase_auth.dart';

class Signup extends StatefulWidget {
  _SignupState createState() => _SignupState();
}

class _SignupState extends State<Signup> {
  TextEditingController emailController = TextEditingController();
  TextEditingController passwordController = TextEditingController();
  final _formKey = GlobalKey<FormState>();
  bool busyView = false;

  @override
  Widget build(BuildContext context) {
    if (!busyView) {
      return Scaffold(
        body: Center(
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
                Column(
                  children: <Widget>[
                    Padding(
                      padding: const EdgeInsets.all(10.0),
                      child: TextButton(
                        onPressed: () async {
                          FormState? s = _formKey.currentState;
                          if (s == null ? false : s.validate()) {
                            setState(() {
                              busyView = true;
                            });
                            UserCredential? _userCredential;
                            try {
                              _userCredential = await FirebaseAuth.instance
                                  .createUserWithEmailAndPassword(
                                      email: emailController.text,
                                      password: passwordController.text);
                            } on FirebaseAuthException catch (e) {
                              if (e.code == 'weak-password') {
                                print('The password provided is too weak.');
                              } else if (e.code == 'email-already-in-use') {
                                print(
                                    'The account already exists for that email.');
                              }
                            } catch (e) {
                              print(e);
                              UtilFs.showToast(e.toString(), context);
                            }

                            if (_userCredential != null) {
                              UtilFs.showToast("SignUp Successful", context);
                              Navigator.pop(context);
                            } else {
                              FocusScope.of(context)
                                  .requestFocus(new FocusNode());
                              UtilFs.showToast("SignUp Failed", context);
                            }
                            setState(() {
                              busyView = false;
                            });
                          }
                        },
                        child: Container(
                          height: 20,
                          child: Text("Sign up!",
                              style: TextStyle(
                                  decoration: TextDecoration.underline,
                                  color: Colors.black)),
                        ),
                      ),
                    )
                  ],
                )
              ],
            ),
          ),
        ),
      );
    } else {
      return Scaffold(
        body: Center(
          child: CircularProgressIndicator(),
        ),
      );
    }
  }
}
