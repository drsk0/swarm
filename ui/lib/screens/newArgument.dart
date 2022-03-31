import 'package:flutter/cupertino.dart';
import 'package:swarm/data/gql_api.dart';
import 'package:flutter/material.dart';
import 'package:graphql_flutter/graphql_flutter.dart';
import 'package:swarm/screens/markdown_preview.dart';
import 'package:swarm/screens/viewArguments.dart';

class NewArgumentForm extends StatefulWidget {
  NewArgumentForm(this._ballotId, this._optionRef, this._proContra);
  final String _ballotId;
  final String _optionRef;
  final IProContra _proContra;

  @override
  NewArgumentFormState createState() {
    return NewArgumentFormState();
  }
}

class NewArgumentFormState extends State<NewArgumentForm> {
  NewArgumentFormState();
  final _formKey = GlobalKey<FormState>();
  TextEditingController _argumentController = TextEditingController();

  @override
  void dispose() {
    _argumentController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    Widget children = Text('Something went wrong.');
    children = Scaffold(
      appBar: AppBar(title: Text('Create a new argument')),
      body: Form(
          key: _formKey,
          child: Column(children: [
            Expanded(
                child: TextFormField(
              expands: true,
              minLines: null,
              maxLines: null,
              controller: _argumentController,
              decoration: InputDecoration(
                labelText: "Argument. Use Markdown for text formatting!",
              ),
              keyboardType: TextInputType.multiline,
              validator: (value) {
                if (value != null && value.isEmpty) {
                  return 'Please give an argument pro/contra for the option!';
                }
                return null;
              },
            )),
          ])),
      bottomNavigationBar: BottomAppBar(
        child: Row(mainAxisAlignment: MainAxisAlignment.spaceEvenly, children: [
          IconButton(
              icon: Icon(Icons.preview),
              onPressed: () {
                Navigator.push(
                    context,
                    MaterialPageRoute(
                        builder: (context) =>
                            MarkdownPreview(_argumentController.text)));
              }),
          Mutation(
              options: MutationOptions(document: gql(GQLApi.createArgument)),
              builder: (RunMutation runMutation, QueryResult? result) {
                return IconButton(
                  icon: Icon(Icons.check),
                  onPressed: () async {
                    FormState? s = _formKey.currentState;
                    if (s != null && s.validate()) {
                      runMutation({
                        'caaBallotId': widget._ballotId,
                        'caaOptionId': widget._optionRef,
                        'caaProContra': showIProContra(widget._proContra),
                        'caaUserId': 'drsk',
                        'caaText': _argumentController.text,
                      });
                      Navigator.pop(context);
                    }
                  },
                );
              })
        ]),
      ),
    );
    return children;
  }
}
