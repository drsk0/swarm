import 'package:flutter/material.dart';
import 'package:graphql_flutter/graphql_flutter.dart';
import 'package:swarm/data/gql_api.dart';
import 'package:share_plus/share_plus.dart';
import 'package:uuid/uuid.dart';
import 'package:uuid/uuid_util.dart';
import 'package:swarm/screens/viewProfile.dart';
import 'package:swarm/screens/editProfile.dart';
import 'package:swarm/components/utils.dart';

var uuid = new Uuid(options: {'grng': UuidUtil.cryptoRNG});

class FishSelector extends StatelessWidget {
  FishSelector(this._fish);
  final dynamic _fish;
  @override
  Widget build(BuildContext context) {
    return ListTile(
      title: Text(_fish['fName']),
      onTap: () {
        Navigator.push(
            context,
            MaterialPageRoute(
                builder: (ctx) => ProfileView(_fish['fProfile'])));
      },
    );
  }
}

class FishView extends StatelessWidget {
  final String _token;
  FishView(this._token);
  @override
  Widget build(BuildContext context) {
    Widget child = Query(
        options: QueryOptions(document: gql(GQLApi.queryFish)),
        builder: (QueryResult result,
            {VoidCallback? refetch, FetchMore? fetchMore}) {
          return withQueryResult(
              context: context,
              result: result,
              field: 'qFish',
              builder: (_fish) => Scaffold(
                  appBar: AppBar(title: Text('Members')),
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
                                        child: EditProfileForm(_token))))),
                        Mutation(
                            options: MutationOptions(
                                document: gql(GQLApi.createInvite)),
                            builder:
                                (RunMutation runMutation, QueryResult? result) {
                              return IconButton(
                                  icon: Icon(Icons.qr_code_sharp),
                                  onPressed: () async {
                                    final _inviteToken = uuid.v4(
                                        options: {'rng': UuidUtil.cryptoRNG});
                                    runMutation({'ciaToken': _inviteToken});
                                    print('Created invite:' + _inviteToken);
                                    Share.share(_inviteToken,
                                        subject: '>~^>a >~^>a >~^>a');
                                  });
                            })
                      ])),
                  body: ListView(
                      padding: EdgeInsets.zero,
                      reverse: true,
                      children: List<Widget>.from(
                          _fish.map((f) => FishSelector(f))))));
        });

    return child;
  } //
}
