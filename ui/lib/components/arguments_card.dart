import 'dart:async';

import 'package:flutter/material.dart';
import 'package:graphql_flutter/graphql_flutter.dart';
import 'package:swarm/data/gql_api.dart';
import 'package:flutter_markdown/flutter_markdown.dart';
import 'package:swarm/components/utils.dart';

List<Widget> argumentCards(
    String _ballotId, String _optionId, dynamic _argIds) {
  return List.generate(List.from(_argIds).length,
      (index) => ArgumentCard(_ballotId, _optionId, _argIds[index]));
}

class ArgumentCard extends StatefulWidget {
  final String _ballotId;
  final String _optionId;
  final String _argId;

  ArgumentCard(this._ballotId, this._optionId, this._argId);

  @override
  createState() {
    return ArgumentCardState();
  }
}

class ArgumentCardState extends State<ArgumentCard> {
  final StreamController _streamController = new StreamController();
  ArgumentCardState();

  @override
  dispose() {
    _streamController.close();
    super.dispose();
  }

  _queryArgument(context) async {
    final _client = GraphQLProvider.of(context).value;
    final _data0 = await _client.query(
        QueryOptions(document: gql(GQLApi.queryArgumentById), variables: {
      'ballotId': widget._ballotId,
      'optionId': widget._optionId,
      'argId': widget._argId
    }));
    if (_data0.data != null && !_streamController.isClosed) {
      _streamController.add(_data0.data?['qArgumentById']);
    }
    final _subscription = _client.subscribe(SubscriptionOptions(
        document: gql(GQLApi.subscribeArgumentById),
        variables: {
          'ballotId': widget._ballotId,
          'optionId': widget._optionId,
          'argId': widget._argId
        }));
    _subscription.listen((result) {
      if (result.data != null && !_streamController.isClosed) {
        _streamController.add(result.data?['sArgumentById']);
      }
    });
  }

  @override
  build(BuildContext context) {
    _queryArgument(context);
    return StreamBuilder(
        stream: _streamController.stream,
        builder: (context, AsyncSnapshot snapshot) {
          return withSnapshot<dynamic>(
              context: context,
              snapshot: snapshot,
              builder: (_argument) {
                final _upvoted = List.from(_argument['aUpVotes']).length > 0;
                GraphQLClient _client = GraphQLProvider.of(context).value;
                return Card(
                    child: Column(children: [
                  Container(
                      padding: ButtonBarTheme.of(context).buttonPadding ??
                          EdgeInsets.only(left: 8.0),
                      child: MarkdownBody(
                        data: snapshot.data['aText'],
                        shrinkWrap: true,
                      )),
                  BottomAppBar(
                      child: Row(
                          mainAxisAlignment: MainAxisAlignment.end,
                          children: [
                        IconButton(
                            icon: _upvoted
                                ? Icon(Icons.favorite)
                                : Icon(Icons.favorite_outline),
                            onPressed: () {
                              _client.mutate(MutationOptions(
                                  document: gql(_upvoted
                                      ? GQLApi.deleteUpVote
                                      : GQLApi.createUpVote),
                                  variables: {
                                    'cuaOptionId': widget._optionId,
                                    'cuaBallotId': widget._ballotId,
                                    'cuaArgumentId': widget._argId
                                  }));
                            }),
                      ]))
                ]));
              });
        });
  }
}
