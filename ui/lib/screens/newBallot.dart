import 'package:flutter/cupertino.dart';
import 'package:swarm/data/gql_api.dart';
import 'package:flutter/material.dart';
import 'package:graphql_flutter/graphql_flutter.dart';
import 'package:swarm/screens/markdown_preview.dart';

class NewBallotForm extends StatefulWidget {
  NewBallotForm();
  @override
  NewBallotFormState createState() {
    return NewBallotFormState();
  }
}

class NewBallotFormState extends State<NewBallotForm> {
  NewBallotFormState();
  final _formKey = GlobalKey<FormState>();
  TextEditingController _titleController = TextEditingController();
  TextEditingController _descriptionController = TextEditingController();

  @override
  void dispose() {
    _descriptionController.dispose();
    _titleController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    Widget children = Text('Something went wrong.');
    children = Scaffold(
        appBar: AppBar(title: Text('Create a new ballot')),
        bottomNavigationBar: BottomAppBar(
            child: Row(
                mainAxisSize: MainAxisSize.max,
                mainAxisAlignment: MainAxisAlignment.spaceEvenly,
                children: [
              IconButton(
                  icon: Icon(Icons.preview),
                  onPressed: () {
                    Navigator.push(
                        context,
                        MaterialPageRoute(
                            builder: (context) =>
                                MarkdownPreview(_descriptionController.text)));
                  }),
              Mutation(
                  options: MutationOptions(document: gql(GQLApi.createBallot)),
                  builder: (RunMutation runMutation, QueryResult? result) {
                    return IconButton(
                        icon: Icon(Icons.check),
                        onPressed: () async {
                          FormState? s = _formKey.currentState;
                          if (s != null && s.validate()) {
                            runMutation({
                              "cbaTitle": _titleController.text,
                              "cbaDescription": _descriptionController.text,
                            });
                            Navigator.pop(context);
                          }
                        });
                  })
            ])),
        body: Form(
            key: _formKey,
            child: Column(
              children: <Widget>[
                TextFormField(
                  controller: _titleController,
                  decoration: InputDecoration(
                    labelText: "Ballot Title",
                    // border: new OutlineInputBorder(
                    // borderRadius: new BorderRadius.circular(25.0),
                    // borderSide: new BorderSide(),
                    // ),
                  ),
                  keyboardType: TextInputType.text,
                  validator: (value) {
                    if (value != null && value.isEmpty) {
                      return 'Please give a name for the ballot!';
                    }
                    return null;
                  },
                ),
                Expanded(
                    child: TextFormField(
                  controller: _descriptionController,
                  expands: true,
                  minLines: null,
                  maxLines: null,
                  decoration: InputDecoration(
                    labelText:
                        "Ballot Description. Use Markdown for text formatting!",
                    // border: new OutlineInputBorder(
                    // borderRadius: new BorderRadius.circular(25.0),
                    // borderSide: new BorderSide(),
                    // ),
                  ),
                  keyboardType: TextInputType.multiline,
                  validator: (value) {
                    if (value != null && value.isEmpty) {
                      return 'Please give a description of the ballot!';
                    }
                    return null;
                  },
                ))
              ],
            )));
    return children;
  }
}
