import 'package:flutter/material.dart';
import 'package:graphql_flutter/graphql_flutter.dart';
import 'package:swarm/data/gql_api.dart';
import 'package:swarm/screens/newSwarm.dart';
import 'package:swarm/components/utils.dart';
import 'package:swarm/screens/joinSwarm.dart';

class SwarmSelector extends StatelessWidget {
  SwarmSelector(this._swarm, this._setSwarm);
  final dynamic _swarm;
  final Function(String swarmId, String swarmName) _setSwarm;
  @override
  Widget build(BuildContext context) {
    return ListTile(
        title: Text(_swarm['wrValue']),
        onTap: () {
          _setSwarm(_swarm['wrRef'], _swarm['wrValue']);
          Navigator.pop(context);
        });
  }
}

class Menu extends StatelessWidget {
  Menu(this._setSwarm);
  final Function(String swarmId, String swarmName) _setSwarm;
  @override
  Widget build(BuildContext context) {
    final _queryUserSwarm = GQLApi.queryUserSwarms;
    return Query(
        options: QueryOptions(document: gql(_queryUserSwarm)),
        builder: (QueryResult result,
            {VoidCallback? refetch, FetchMore? fetchMore}) {
          print(result.exception.toString());
          return withQueryResult(
              context: context,
              result: result,
              field: 'qUserSwarms',
              refetch: refetch,
              builder: (swarms) => Scaffold(
                  appBar: AppBar(title: Text('Choose a swarm')),
                  bottomNavigationBar: BottomAppBar(
                      child: Row(
                    mainAxisAlignment: MainAxisAlignment.spaceBetween,
                    mainAxisSize: MainAxisSize.max,
                    children: [
                      IconButton(
                          icon: Icon(Icons.edit),
                          onPressed: () => Navigator.push(
                              context,
                              MaterialPageRoute(
                                  builder: (ctx) => GraphQLProvider(
                                      client: GraphQLProvider.of(context),
                                      child: NewSwarmForm())))),
                      IconButton(
                          icon: Icon(Icons.qr_code),
                          onPressed: () => Navigator.push(
                              context,
                              MaterialPageRoute(
                                  builder: (ctx) => GraphQLProvider(
                                      client: GraphQLProvider.of(context),
                                      child: JoinSwarmForm()))))
                    ],
                  )),
                  body: ListView(
                      padding: EdgeInsets.zero,
                      reverse: true,
                      children: List<Widget>.from(
                          swarms.map((s) => SwarmSelector(s, _setSwarm))))));
        });
  }
}
