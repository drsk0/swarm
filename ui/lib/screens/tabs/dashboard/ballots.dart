import 'dart:async';
import 'package:swarm/data/gql_api.dart';
import 'package:flutter/material.dart';
import 'package:graphql_flutter/graphql_flutter.dart';
import 'package:swarm/components/ballot_card.dart';
import 'package:swarm/components/utils.dart';

class Ballots extends StatefulWidget {
  @override
  createState() {
    return BallotsState();
  }
}

class BallotsState extends State<Ballots> {
  final StreamController _streamController = new StreamController();
  BallotsState();
  @override
  dispose() {
    print('############ calling dispose Ballots');
    _streamController.close();
    super.dispose();
  }

  _queryBallots(context) async {
    final _client = GraphQLProvider.of(context).value;
    final _data0 = await _client
        .query(QueryOptions(document: gql(GQLApi.queryAllBallots)));
    if (_data0.data?['qAllBallots'] != null && !_streamController.isClosed) {
      print('ballots query event');
      print(_data0.data);
      _streamController.add(_data0.data!['qAllBallots']);
    }
    final _subscription = _client.subscribe(
        SubscriptionOptions(document: gql(GQLApi.subscribeAllBallots)));
    _subscription.listen((result) {
      print("ballots stream event");
      print(result.data);
      if (result.data?['sAllBallots'] != null && !_streamController.isClosed) {
        _streamController.add(result.data!['sAllBallots']);
      }
    });
  }

  @override
  Widget build(BuildContext context) {
    _queryBallots(context);
    return StreamBuilder(
        stream: _streamController.stream,
        builder: (context, AsyncSnapshot snapshot) {
          return withSnapshot(
              context: context,
              snapshot: snapshot,
              builder: (dynamic data) {
                final _ballots = List.from(data);
                print('############### _ballots');
                print(_ballots.toString());
                return new ListView.builder(
                    itemCount: _ballots.length,
                    itemBuilder: (context, index) {
                      print(_ballots[index].toString());
                      return BallotCard(_ballots[index]);
                    });
              });
        });
  }
}
