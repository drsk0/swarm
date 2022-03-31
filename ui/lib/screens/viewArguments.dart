import 'dart:async';

import 'package:flutter/material.dart';
import 'package:graphql_flutter/graphql_flutter.dart';
import 'package:swarm/data/gql_api.dart';
import 'package:swarm/components/arguments_card.dart';
import 'package:swarm/screens/newArgument.dart';
import 'package:swarm/components/utils.dart';

enum IProContra { IPro, IContra }

String showIProContra(IProContra pc) {
  if (pc == IProContra.IPro)
    return 'IPro';
  else
    return 'IContra';
}

class ArgumentsView extends StatefulWidget {
  final String _ballotId;
  final String _oId;
  final IProContra _proContra;
  ArgumentsView(this._ballotId, this._oId, this._proContra);

  @override
  createState() {
    return ArgumentsViewState();
  }
}

class ArgumentsViewState extends State<ArgumentsView> {
  final StreamController<List<dynamic>> _streamController =
      new StreamController();
  ArgumentsViewState();

  @override
  dispose() {
    _streamController.close();
    super.dispose();
  }

  _queryArguments(context) async {
    final _client = GraphQLProvider.of(context).value;
    final _data0 = await _client.query(
        QueryOptions(document: gql(GQLApi.queryAllArguments), variables: {
      'ballotId': widget._ballotId,
      'oId': widget._oId,
      'proContra': showIProContra(widget._proContra)
    }));
    if (_data0.data?['qAllArguments'] != null && !_streamController.isClosed) {
      _streamController.add(_data0.data!['qAllArguments']);
    }
    final _subscription = _client.subscribe(SubscriptionOptions(
        document: gql(GQLApi.subscribeAllArguments),
        variables: {
          'ballotId': widget._ballotId,
          'oId': widget._oId,
          'proContra': showIProContra(widget._proContra)
        }));
    _subscription.listen((result) {
      if (result.data?['sAllArguments'] != null &&
          !_streamController.isClosed) {
        _streamController.add(result.data!['sAllArguments']);
      }
    });
  }

  @override
  build(BuildContext context) {
    _queryArguments(context);
    return StreamBuilder<List<dynamic>>(
        stream: _streamController.stream,
        builder: (context, AsyncSnapshot snapshot) {
          return withSnapshot<dynamic>(
              context: context,
              snapshot: snapshot,
              builder: (_argumentIds) {
                return Scaffold(
                    appBar: AppBar(title: Text('Discussion')),
                    body: ListView.builder(
                        itemCount: _argumentIds.length,
                        itemBuilder: (context, index) {
                          return ArgumentCard(widget._ballotId, widget._oId,
                              _argumentIds[index]);
                        }),
                    bottomNavigationBar: BottomAppBar(
                        child: Row(
                            mainAxisAlignment: MainAxisAlignment.spaceEvenly,
                            mainAxisSize: MainAxisSize.max,
                            children: [
                          IconButton(
                              icon: Icon(Icons.create),
                              onPressed: () {
                                Navigator.push(
                                    context,
                                    MaterialPageRoute(
                                        builder: (ctx) => GraphQLProvider(
                                            client: GraphQLProvider.of(context),
                                            child: NewArgumentForm(
                                                widget._ballotId,
                                                widget._oId,
                                                widget._proContra))));
                              })
                        ])));
              });
        });
  }
}
