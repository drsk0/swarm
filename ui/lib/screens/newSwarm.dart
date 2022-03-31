import 'package:flutter/cupertino.dart';
import 'package:graphql_flutter/graphql_flutter.dart';
import 'package:flutter/material.dart';
import 'package:swarm/data/gql_api.dart';

class NewSwarmForm extends StatefulWidget {
  NewSwarmForm();
  @override
  NewSwarmFormState createState() {
    return NewSwarmFormState();
  }
}

class NewSwarmFormState extends State<NewSwarmForm> {
  NewSwarmFormState();
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
    final _createSwarm = GQLApi.createSwarm;
    children = Scaffold(
      appBar: AppBar(title: Text('Create a new swarm')),
      body: Form(
          key: _formKey,
          child: TextFormField(
            controller: _swarmNameController,
            decoration: InputDecoration(
              labelText: "Name",
            ),
            keyboardType: TextInputType.text,
            validator: (value) {
              if (value != null && value.isEmpty) {
                return 'Please give a for the new swarm!';
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
                options: MutationOptions(document: gql(_createSwarm)),
                builder: (RunMutation runMutation, QueryResult? result) {
                  return IconButton(
                      icon: Icon(Icons.check),
                      onPressed: () async {
                        FormState? s = _formKey.currentState;
                        if (s != null && s.validate()) {
                          runMutation({
                            'csaName': _swarmNameController.text,
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
