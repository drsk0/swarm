import 'package:flutter/cupertino.dart';
import 'package:graphql_flutter/graphql_flutter.dart';
import 'package:flutter/material.dart';
import 'package:swarm/data/gql_api.dart';
import 'package:swarm/screens/markdown_preview.dart';

class NewOptionForm extends StatefulWidget {
  final String _ballotId;
  NewOptionForm(this._ballotId);

  @override
  NewOptionFormState createState() {
    return NewOptionFormState(_ballotId);
  }
}

class NewOptionFormState extends State<NewOptionForm> {
  NewOptionFormState(this._ballotId);
  final String _ballotId;
  final _formKey = GlobalKey<FormState>();
  final TextEditingController _titleController = TextEditingController();
  final TextEditingController _descriptionController = TextEditingController();

  String get title {
    return _titleController.text;
  }

  String get description {
    return _descriptionController.text;
  }

  @override
  Widget build(BuildContext context) {
    Widget children = Text('Something went wrong.');
    children = Scaffold(
      appBar: AppBar(title: Text('Create a new option')),
      bottomNavigationBar: BottomAppBar(
          child: Row(
              mainAxisAlignment: MainAxisAlignment.spaceEvenly,
              mainAxisSize: MainAxisSize.max,
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
                options: MutationOptions(document: gql(GQLApi.createOption)),
                builder: (RunMutation runMutation, QueryResult? result) {
                  return IconButton(
                    icon: Icon(Icons.check),
                    onPressed: () async {
                      FormState? s = _formKey.currentState;
                      if (s != null && s.validate()) {
                        runMutation({
                          'coaBallotId': _ballotId,
                          'coaUserId': 'drsk',
                          'coaTitle': _titleController.text,
                          'coaDescription': _descriptionController.text
                        });
                        Navigator.pop(context);
                      }
                    },
                  );
                })
          ])),
      body: Form(
          key: _formKey,
          child: Column(children: [
            TextFormField(
                controller: _titleController,
                decoration: InputDecoration(
                  labelText: "Option Title",
                  // border: new OutlineInputBorder(
                  // borderRadius: new BorderRadius.circular(25.0),
                  // borderSide: new BorderSide(),
                  // ),
                ),
                keyboardType: TextInputType.text,
                validator: (value) {
                  if (value != null && value.isEmpty) {
                    return 'Please give a name for the option!';
                  }
                  return null;
                }),
            Expanded(
                child: TextFormField(
                    controller: _descriptionController,
                    expands: true,
                    minLines: null,
                    maxLines: null,
                    decoration: InputDecoration(
                      labelText:
                          "Option Description. Use Markdown for text formatting!",
                      // border: new OutlineInputBorder(
                      // borderRadius: new BorderRadius.circular(25.0),
                      // borderSide: new BorderSide(),
                      // ),
                    ),
                    keyboardType: TextInputType.multiline,
                    validator: (value) {
                      if (value != null && value.isEmpty) {
                        return 'Please give a description for the option!';
                      }
                      return null;
                    }))
          ])),
    );
    return children;
  } //
}
