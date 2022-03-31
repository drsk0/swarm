import 'dart:async';
import 'package:flutter/material.dart';
import 'package:swarm/screens/viewBallot.dart';
import 'package:swarm/screens/newDelegation.dart';
import 'package:swarm/screens/dashboard.dart';
import 'package:flutter_markdown/flutter_markdown.dart';
import 'package:graphql_flutter/graphql_flutter.dart';
import 'package:swarm/data/gql_api.dart';
import 'package:swarm/components/utils.dart';

class BallotCard extends StatefulWidget {
  final String _ballotId;
  BallotCard(this._ballotId);
  @override
  createState() {
    return BallotCardState();
  }
}

class BallotCardState extends State<BallotCard> {
  final StreamController _streamController = new StreamController();
  BallotCardState();

  @override
  dispose() {
    print('#################### calling dispose BallotCard');
    _streamController.close();
    super.dispose();
  }

  // TODO (drsk) factor snapshot/update logic out
  _queryBallot(context) async {
    final _client = GraphQLProvider.of(context).value;
    final _data0 = await _client.query(QueryOptions(
        document: gql(GQLApi.queryBallotById),
        variables: {'ballotId': this.widget._ballotId}));
    if (_data0.data?['qBallotById'] != null && !_streamController.isClosed) {
      _streamController.add(_data0.data!['qBallotById']);
    }
    final _subscription = _client.subscribe(SubscriptionOptions(
        document: gql(GQLApi.subscribeBallotById),
        variables: {'ballotId': this.widget._ballotId}));
    _subscription.listen((result) {
      if (!result.hasException &&
          result.data != null &&
          result.data?['sBallotById'] != null &&
          !_streamController.isClosed) {
        print('ballot card stream event:');
        print(result.data.toString());
        _streamController.add(result.data?['sBallotById']);
      }
    });
  }

  @override
  build(BuildContext context) {
    _queryBallot(context);
    return StreamBuilder(
        stream: _streamController.stream,
        builder: (BuildContext context, AsyncSnapshot snapshot) {
          return withSnapshot<dynamic>(
              context: context,
              snapshot: snapshot,
              builder: (_ballot) {
                print('############## ballotId');
                print(this.widget._ballotId);
                final String? _delegateeM =
                    List.from(_ballot['bDelegations']).length > 0
                        ? _ballot['bDelegations'][0]['dDelegatee']
                        : null;
                return GestureDetector(
                    onTap: () {
                      Navigator.push(
                          context,
                          MaterialPageRoute(
                              builder: (ctx) => GraphQLProvider(
                                  client: GraphQLProvider.of(context),
                                  child: BallotView(this.widget._ballotId))));
                    },
                    child: Card(
                      child: Column(children: [
                        ListTile(
                            contentPadding:
                                ButtonBarTheme.of(context).buttonPadding ??
                                    EdgeInsets.only(left: 8.0),
                            title: Text(_ballot['bTitle'],
                                style: Theme.of(context).textTheme.headline6)),
                        Align(
                          alignment: Alignment.topLeft,
                          child: Container(
                              padding:
                                  ButtonBarTheme.of(context).buttonPadding ??
                                      EdgeInsets.only(left: 8.0),
                              child: MarkdownBody(
                                  data: _ballot['bDescription'],
                                  shrinkWrap: true)),
                        ),
                        BottomAppBar(
                            child: Row(
                                mainAxisAlignment:
                                    MainAxisAlignment.spaceBetween,
                                children: [
                              Mutation(
                                  options: MutationOptions(
                                      document: gql(GQLApi.deleteBallot)),
                                  builder: (RunMutation runMutation,
                                      QueryResult? result) {
                                    return IconButton(
                                        icon: Icon(Icons.delete),
                                        onPressed: () {
                                          runMutation({
                                            'dbaBallotId':
                                                this.widget._ballotId,
                                          });
                                        });
                                  }),
                              (_delegateeM == null
                                  ? Text('')
                                  : Query(
                                      options: QueryOptions(
                                          document:
                                              gql(GQLApi.queryFishByUserId),
                                          variables: {'qfUserId': _delegateeM}),
                                      builder: (QueryResult result,
                                          {VoidCallback? refetch,
                                          FetchMore? fetchMore}) {
                                        return withQueryResult(
                                            context: context,
                                            result: result,
                                            field: 'queryFishByUserId',
                                            builder: (_fish) =>
                                                _fish?['fName'] ?? 'anonymous');
                                      })),
                              IconButton(
                                  icon: Icon(Icons.forward_to_inbox,
                                      color: _delegateeM != null
                                          ? Colors.pink
                                          : Colors.black),
                                  onPressed: () {
                                    Navigator.push(
                                        context,
                                        MaterialPageRoute(
                                            builder: (ctx) => GraphQLProvider(
                                                client:
                                                    GraphQLProvider.of(context),
                                                child: NewDelegation(
                                                    this.widget._ballotId))));
                                  }),
                            ]))
                      ]),
                    ));
              });
        });
  }
}
