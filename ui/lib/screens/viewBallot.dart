import 'dart:async';
import 'package:flutter/material.dart';
import 'package:swarm/components/ballot_card.dart';
import 'package:graphql_flutter/graphql_flutter.dart';
import 'package:swarm/components/statistic_card.dart';
import 'package:swarm/components/options_card.dart';
import 'package:swarm/data/gql_api.dart';
import 'package:swarm/screens/newOption.dart';
import 'package:swarm/components/utils.dart';

class BallotView extends StatefulWidget {
  final String _ballotId;
  BallotView(this._ballotId);
  @override
  createState() {
    return BallotViewState();
  }
}

class BallotViewState extends State<BallotView> {
  final StreamController _streamController = new StreamController();
  BallotViewState();

  @override
  dispose() {
    print('########### calling dispose BallotView');
    _streamController.close();
    super.dispose();
  }

  _queryNrOptions(context) async {
    final _client = GraphQLProvider.of(context).value;
    final _data0 = await _client.query(QueryOptions(
        document: gql(GQLApi.queryAllOptions),
        variables: {'ballotId': widget._ballotId}));
    if (_data0.data?['qAllOptions'] != null && !_streamController.isClosed)
      _streamController.add(_data0.data!['qAllOptions']);
    final _subscription = _client.subscribe(SubscriptionOptions(
        document: gql(GQLApi.subscribeAllOptions),
        variables: {'ballotId': widget._ballotId}));
    _subscription.listen((result) {
      if (result.data != null && !_streamController.isClosed) {
        _streamController.add(result.data!['sAllOptions']);
      }
    });
  }

  @override
  build(BuildContext context) {
    _queryNrOptions(context);
    return StreamBuilder(
        stream: _streamController.stream,
        builder: (BuildContext context, AsyncSnapshot snapshot) {
          return withSnapshot<dynamic>(
              context: context,
              snapshot: snapshot,
              builder: (_ballot) {
                return Scaffold(
                  appBar: AppBar(title: Text('Ballot details')),
                  body: ListView(
                      children: (<Widget>[
                            StatisticCard(widget._ballotId),
                            BallotCard(widget._ballotId),
                          ] +
                          optionCards(widget._ballotId, _ballot))),
                  bottomNavigationBar: BottomAppBar(
                      child: Row(
                          mainAxisAlignment: MainAxisAlignment.center,
                          mainAxisSize: MainAxisSize.max,
                          children: [
                        IconButton(
                            icon: Icon(Icons.edit),
                            onPressed: () {
                              Navigator.push(
                                  context,
                                  MaterialPageRoute(
                                      builder: (ctx) => GraphQLProvider(
                                          client: GraphQLProvider.of(context),
                                          child: NewOptionForm(
                                              widget._ballotId))));
                            }),
                      ])),
                );
              });
        });
  }
}
