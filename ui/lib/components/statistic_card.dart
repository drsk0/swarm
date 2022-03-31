import 'dart:async';
import 'package:graphql_flutter/graphql_flutter.dart';
import 'package:flutter/material.dart';
import 'package:percent_indicator/percent_indicator.dart';
import 'package:charts_flutter/flutter.dart' as charts;
import 'package:swarm/data/gql_api.dart';
import 'package:swarm/components/utils.dart';

class StatisticCard extends StatefulWidget {
  final String _ballotId;
  StatisticCard(this._ballotId);

  @override
  createState() {
    return StatisticCardState();
  }
}

class StatisticCardState extends State<StatisticCard> {
  final StreamController<dynamic> _streamController = new StreamController();
  StatisticCardState();

  @override
  dispose() {
    _streamController.close();
    super.dispose();
  }

  _queryResults(context) async {
    final _client = GraphQLProvider.of(context).value;
    final _data0 = await _client.query(QueryOptions(
        document: gql(GQLApi.queryBallotResult),
        variables: {'ballotId': widget._ballotId}));
    if (_data0.data?['qBallotResult'] != null && !_streamController.isClosed) {
      _streamController.add(_data0.data!['qBallotResult']);
    }
    _streamController.add(_data0.data?['qBallotResult']);
    final _subscription = _client.subscribe(SubscriptionOptions(
        document: gql(GQLApi.subscribeBallotResult),
        variables: {'ballotId': widget._ballotId}));
    _subscription.listen((result) {
      if (result.data?['sBallotResult'] != null &&
          !_streamController.isClosed) {
        _streamController.add(result.data!['sBallotResult']);
      }
    });
  }

  @override
  build(BuildContext context) {
    _queryResults(context);
    return StreamBuilder(
        stream: _streamController.stream,
        builder: (context, AsyncSnapshot snapshot) {
          return withSnapshot<dynamic>(
              context: context,
              snapshot: snapshot,
              builder: (_ballotResult) {
                Widget pieChart = Text('Something went wrong');
                Widget turnOut = Text('Something went wrong');
                final _nrOfOptions = List.from(_ballotResult['brVotes']).length;
                if (_nrOfOptions == 0 || _ballotResult['brTurnout'] == 0) {
                  pieChart = Text('no votes yet');
                  turnOut = Text(' - ');
                } else {
                  final Map<String, String> _idToTitle = Map.fromIterable(
                      snapshot.data['brOptions'],
                      key: (e) => e['wiId'],
                      value: (e) => e['wiValue']['oTitle']);
                  List<charts.Series<dynamic, String>> chartData = [
                    new charts.Series<dynamic, String>(
                        id: 'Ballot Result',
                        colorFn: (_, i) => charts.ColorUtil.fromDartColor(
                            Colors.grey[i == null || i == 0 || i > 9
                                ? 900
                                : 900 - i * 100]!),
                        domainFn: (dynamic opt, _) => opt['key'],
                        measureFn: (dynamic opt, _) => opt['value']!,
                        labelAccessorFn: (dynamic opt, _) =>
                            _idToTitle[opt['key']] ?? "no title",
                        outsideLabelStyleAccessorFn: (_, i) =>
                            charts.TextStyleSpec(
                                color: charts.ColorUtil.fromDartColor(
                                    Colors.pink)),
                        data: snapshot.data['brVotes'])
                  ];

                  pieChart = charts.PieChart(
                    chartData,
                    animate: true,
                    defaultRenderer: new charts.ArcRendererConfig<Object>(
                        arcWidth: 60,
                        arcRendererDecorators: [
                          new charts.ArcLabelDecorator(
                              labelPosition: charts.ArcLabelPosition.auto)
                        ]),
                  );
                  final double _turnOut = snapshot.data['brTurnout'].toDouble();
                  turnOut = LinearPercentIndicator(
                    width: MediaQuery.of(context).size.width - 40,
                    percent: _turnOut.toDouble() / 1000.0,
                    progressColor: Colors.black,
                    animation: true,
                    lineHeight: 20.0,
                    animationDuration: 2000,
                    center: Text((_turnOut / 10.0).toString() + "%",
                        style: TextStyle(color: Colors.pink)),
                    linearStrokeCap: LinearStrokeCap.butt,
                  );
                }
                return Card(
                  child: Column(children: [
                    SizedBox(child: pieChart, height: 300),
                    Padding(padding: EdgeInsets.all(15.0), child: turnOut)
                  ]),
                );
              });
        });
  }
}
