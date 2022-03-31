import 'package:flutter/material.dart';
import 'package:swarm/data/gql_api.dart';
import 'package:graphql_flutter/graphql_flutter.dart';
import 'package:swarm/components/utils.dart';

class FishSelector extends StatelessWidget {
  FishSelector(this._ballotId, this._fish);
  final dynamic _fish;
  final String _ballotId;
  @override
  Widget build(BuildContext context) {
    Widget child = Text('Something went wrong');
    child = Mutation(
        options: MutationOptions(document: gql(GQLApi.createDelegation)),
        builder: (RunMutation runMutation, QueryResult? result) {
          return ListTile(
            title: Text(_fish['fName']),
            onTap: () {
              runMutation({
                'dmaBallotId': _ballotId,
                'dmaDelegateeId': _fish['fUserId']
              });
              Navigator.pop(context);
            },
          );
        });
    return child;
  }
}

class NewDelegation extends StatelessWidget {
  final String _ballotId;
  NewDelegation(this._ballotId);
  @override
  Widget build(BuildContext context) {
    Widget child = Text('Something went wrong');
    child = Query(
        options: QueryOptions(document: gql(GQLApi.queryFish)),
        builder: (QueryResult result,
            {VoidCallback? refetch, FetchMore? fetchMore}) {
          return withQueryResult(
              context: context,
              result: result,
              field: 'qFish',
              builder: (_fish) => ListView(
                  padding: EdgeInsets.zero,
                  reverse: true,
                  children: List<Widget>.from(
                      _fish.map((f) => FishSelector(_ballotId, f)))));
        });

    return Scaffold(
        appBar: AppBar(title: Text('Delegate this ballot')), body: child);
  }
}
