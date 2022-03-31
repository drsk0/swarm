import 'package:swarm/data/gql_api.dart';
import 'package:flutter/material.dart';
import 'package:swarm/config/client.dart';
import 'package:graphql_flutter/graphql_flutter.dart';

class EditProfileForm extends StatefulWidget {
  final String _token;
  EditProfileForm(this._token);

  @override
  EditProfileFormState createState() {
    return EditProfileFormState(_token);
  }
}

class EditProfileFormState extends State<EditProfileForm> {
  final String _token;
  final _formKey = GlobalKey<FormState>();
  TextEditingController _controller = new TextEditingController();
  EditProfileFormState(this._token);
  @override
  Widget build(BuildContext context) {
    Widget child = Text('Something went wrong');
    child = Scaffold(
      appBar: AppBar(title: Text('Edit your profile')),
      body: Form(
          child: Column(children: [
        Expanded(
            child: Query(
                options: QueryOptions(
                    document: gql(GQLApi.queryFishByUserId),
                    variables: {'qfUserId': _token}),
                builder: (QueryResult result,
                    {VoidCallback? refetch, FetchMore? fetchMore}) {
                  Widget child = Text('Internal error');
                  if (result.hasException) {
                    child = Text(result.exception.toString());
                  }

                  if (result.isLoading) {
                    child = Text('loading');
                  }

                  if (result.data == null) {
                    child = Text('No data');
                  }
                  if (result.data != null) {
                    final _fish = result.data?['qFishByUserId'];
                    _controller = TextEditingController(
                        text: (_fish != null)
                            ? _fish['fProfile']
                            : "Write your profile here!");
                    child = TextFormField(
                        expands: true,
                        minLines: null,
                        maxLines: null,
                        key: _formKey,
                        controller: _controller,
                        keyboardType: TextInputType.multiline);
                  }
                  return child;
                }))
      ])),
      bottomNavigationBar: BottomAppBar(
          child: Row(mainAxisAlignment: MainAxisAlignment.center, children: [
        Mutation(
            options: MutationOptions(document: gql(GQLApi.updateProfile)),
            builder: (RunMutation runMutation, QueryResult? result) {
              return IconButton(
                  icon: Icon(Icons.check),
                  onPressed: () {
                    runMutation({
                      'upaBody': _controller.text,
                    });
                    Navigator.pop(context);
                  });
            })
      ])),
    );
    return child;
  }
}
