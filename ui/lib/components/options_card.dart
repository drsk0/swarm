import 'dart:async';

import 'package:flutter/material.dart';
import 'package:graphql_flutter/graphql_flutter.dart';
import 'package:swarm/data/gql_api.dart';
import 'package:flutter_rating_bar/flutter_rating_bar.dart';
import 'package:flutter_markdown/flutter_markdown.dart';
import 'package:swarm/screens/viewArguments.dart';
import 'package:swarm/components/utils.dart';

List<Widget> optionCards(String _ballotId, List<dynamic> oIds) {
  return List.from(oIds).map((oId) => OptionsCard(_ballotId, oId)).toList();
}

class OptionsCard extends StatefulWidget {
  final String _ballotId;
  final String _oId;
  OptionsCard(this._ballotId, this._oId);

  @override
  _OptionsCardState createState() => _OptionsCardState();
}

class _OptionsCardState extends State<OptionsCard> {
  Map<String, double> _stars = {};
  final StreamController _streamController = new StreamController();
  _OptionsCardState();

  @override
  dispose() {
    _streamController.close();
    super.dispose();
  }

  _queryOption(context) async {
    final _client = GraphQLProvider.of(context).value;
    final _data0 = await _client.query(QueryOptions(
        document: gql(GQLApi.queryOption),
        variables: {'ballotId': widget._ballotId, 'oId': widget._oId}));
    if (_data0.data?['qOption'] != null && !_streamController.isClosed) {
      _streamController.add(_data0.data!['qOption']);
    }
    final _subscription = _client.subscribe(SubscriptionOptions(
        document: gql(GQLApi.subscribeOption),
        variables: {'ballotId': widget._ballotId, 'oId': widget._oId}));
    _subscription.listen((result) {
      if (result.data?['sOption'] != null && !_streamController.isClosed) {
        print(result.data!['sOption']['oArguments']);
        _streamController.add(result.data!['sOption']);
      }
    });
  }

  double _nrOfStarsToDouble(String nrOfStars) {
    switch (nrOfStars) {
      case 'ZeroStars':
        return 0.0;
      case 'OneStar':
        return 1.0;
      case 'TwoStars':
        return 2.0;
      case 'ThreeStars':
        return 3.0;
      case 'FourStars':
        return 4.0;
      case 'FiveStars':
        return 5.0;
      default:
        return 0.0;
    }
  }

  String _doubleToNrOfStars(double stars) {
    switch (stars.round()) {
      case 0:
        return 'ZeroStars';
      case 1:
        return 'OneStar';
      case 2:
        return 'TwoStars';
      case 3:
        return 'ThreeStars';
      case 4:
        return 'FourStars';
      case 5:
        return 'FiveStars';
      default:
        return 'ZeroStars';
    }
  }

  @override
  build(BuildContext context) {
    _queryOption(context);
    return StreamBuilder(
        stream: _streamController.stream,
        builder: (BuildContext context, AsyncSnapshot snapshot) {
          return withSnapshot<dynamic>(
              context: context,
              snapshot: snapshot,
              builder: (_option) {
                final _votes = _option['oVotes'];
                final _rating = _stars[widget._oId] ??
                    (_votes.length == 0
                        ? 0.0
                        : _nrOfStarsToDouble(_votes[0]['value']['vStars']));
                return Card(
                    child: Column(children: [
                  ListTile(
                      contentPadding:
                          ButtonBarTheme.of(context).buttonPadding ??
                              EdgeInsets.only(left: 8.0),
                      title: Text(_option['oTitle'],
                          style: Theme.of(context).textTheme.headline6)),
                  Align(
                      alignment: Alignment.topLeft,
                      child: Container(
                          padding: ButtonBarTheme.of(context).buttonPadding ??
                              EdgeInsets.only(left: 8.0),
                          child: MarkdownBody(
                            data: _option['oDescription'],
                            shrinkWrap: true,
                          ))),
                  BottomAppBar(
                    child: Row(
                        mainAxisAlignment: MainAxisAlignment.spaceBetween,
                        children: [
                          Row(
                              mainAxisAlignment: MainAxisAlignment.start,
                              children: [
                                Mutation(
                                    options: MutationOptions(
                                        document: gql(GQLApi.deleteOption)),
                                    builder: (RunMutation runMutation,
                                        QueryResult? result) {
                                      return IconButton(
                                          icon: Icon(Icons.delete),
                                          onPressed: () {
                                            runMutation({
                                              'doaBallotId': widget._ballotId,
                                              'doaOptionId': widget._oId
                                            });
                                          });
                                    }),
                                Stack(children: [
                                  IconButton(
                                      icon: Icon(Icons.forum),
                                      onPressed: () {
                                        Navigator.push(
                                            context,
                                            MaterialPageRoute(
                                                builder: (ctx) =>
                                                    GraphQLProvider(
                                                        client:
                                                            GraphQLProvider.of(
                                                                context),
                                                        child: ArgumentsView(
                                                            widget._ballotId,
                                                            widget._oId,
                                                            IProContra.IPro))));
                                      }),
                                  Positioned(
                                      top: 3.0,
                                      right: 4.0,
                                      child: Text(
                                          (List.from(_option['oArguments'])
                                                ..retainWhere((arg) =>
                                                    arg['value']
                                                        ?['aProContra'] ==
                                                    'Pro'))
                                              .length
                                              .toString(),
                                          style: TextStyle(color: Colors.pink)))
                                ]),
                                Stack(children: [
                                  IconButton(
                                      icon: Icon(Icons.forum_outlined),
                                      onPressed: () {
                                        Navigator.push(
                                            context,
                                            MaterialPageRoute(
                                                builder: (ctx) =>
                                                    GraphQLProvider(
                                                        client:
                                                            GraphQLProvider.of(
                                                                context),
                                                        child: ArgumentsView(
                                                            widget._ballotId,
                                                            widget._oId,
                                                            IProContra
                                                                .IContra))));
                                      }),
                                  Positioned(
                                      top: 3.0,
                                      right: 4.0,
                                      child: Text(
                                          (List.from(_option['oArguments'])
                                                ..retainWhere((arg) =>
                                                    arg['value']
                                                        ?['aProContra'] ==
                                                    'Contra'))
                                              .length
                                              .toString(),
                                          style: TextStyle(color: Colors.pink)))
                                ]),
                                Mutation(
                                    options: MutationOptions(
                                      document: gql(GQLApi.createVote),
                                    ),
                                    builder: (RunMutation runMutation,
                                        QueryResult? result) {
                                      return IconButton(
                                          icon: Icon(Icons.how_to_vote),
                                          onPressed: () async {
                                            runMutation({
                                              'cvaBallotId': widget._ballotId,
                                              'cvaOptionId': widget._oId,
                                              'cvaStars': _doubleToNrOfStars(
                                                  _stars[widget._oId] ?? 0.0)
                                            });
                                          });
                                    }),
                              ]),
                          Row(
                              mainAxisAlignment: MainAxisAlignment.end,
                              children: [
                                RatingBar.builder(
                                  initialRating: _rating,
                                  itemSize: 30,
                                  minRating: 0,
                                  direction: Axis.horizontal,
                                  allowHalfRating: false,
                                  itemCount: 5,
                                  itemPadding:
                                      EdgeInsets.symmetric(horizontal: 4.0),
                                  itemBuilder: (context, _) => Icon(
                                    Icons.star,
                                    color: Colors.black,
                                  ),
                                  onRatingUpdate: (rating) {
                                    _stars[widget._oId] = rating;
                                    setState(() {});
                                  },
                                ),
                              ])
                        ]),
                  )
                ]));
              });
        });
  }
}
