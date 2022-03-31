import 'package:flutter/cupertino.dart';
import 'package:graphql_flutter/graphql_flutter.dart';
import 'package:flutter/material.dart';
import 'package:swarm/data/gql_api.dart';

class JoinSwarmForm extends StatefulWidget {
  JoinSwarmForm();
  @override
  JoinSwarmFormState createState() {
    return JoinSwarmFormState();
  }
}

class JoinSwarmFormState extends State<JoinSwarmForm> {
  JoinSwarmFormState();
  final _formKey = GlobalKey<FormState>();
  TextEditingController _swarmNameController = TextEditingController();

  @override
  void dispose() {
    _swarmNameController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    Widget children = Text('Something went wrong.');
    final _takeInvite = GQLApi.takeInvite;
    children = Scaffold(
      appBar: AppBar(title: Text('Paste your invitiation token')),
      body: Form(
          key: _formKey,
          child: TextFormField(
            controller: _swarmNameController,
            decoration: InputDecoration(
              labelText: "Token",
            ),
            keyboardType: TextInputType.text,
            validator: (value) {
              if (value != null && value.isEmpty) {
                return 'Invalid invitation token.';
              }
              return null;
            },
          )),
      bottomNavigationBar: BottomAppBar(
          child: Row(
              mainAxisAlignment: MainAxisAlignment.center,
              mainAxisSize: MainAxisSize.max,
              children: [
            Mutation(
                options: MutationOptions(document: gql(_takeInvite)),
                builder: (RunMutation runMutation, QueryResult? result) {
                  return IconButton(
                      icon: Icon(Icons.check),
                      onPressed: () async {
                        FormState? s = _formKey.currentState;
                        if (s != null && s.validate()) {
                          runMutation({
                            'tiaToken': _swarmNameController.text,
                          });
                          Navigator.pop(context);
                        }
                      });
                })
          ])),
    );
    return children;
  }
}
