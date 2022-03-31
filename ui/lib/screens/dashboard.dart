import 'dart:async';

import 'package:swarm/components/fish_menu.dart';
import 'package:swarm/screens/newBallot.dart';
import 'package:swarm/screens/tabs/dashboard/ballots.dart';
import 'package:swarm/config/client.dart';
import 'package:flutter/material.dart';
import 'package:firebase_auth/firebase_auth.dart';
import 'package:firebase_messaging/firebase_messaging.dart';
import 'package:swarm/components/menu.dart';
import 'package:graphql_flutter/graphql_flutter.dart';
import 'package:swarm/data/gql_api.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:swarm/components/utils.dart';

class SwarmInfo {
  String swarm;
  String swarmName;
  SwarmInfo(this.swarm, this.swarmName);
}

class Dashboard extends StatefulWidget {
  final User _user;
  Dashboard(this._user);

  createState() {
    return DashboardState(_user);
  }
}

class DashboardState extends State<Dashboard> {
  final User _user;
  final StreamController<SwarmInfo> _swarmInfoStream = new StreamController();

  DashboardState(this._user) {
    initMethod();
  }

  @override
  dispose() {
    print('############# calling dispose Dashboard');
    _swarmInfoStream.close();
    super.dispose();
  }

  Future<void> saveFcmTokenToBackend(
      {required User user, required String fcmToken}) async {
    ValueNotifier<GraphQLClient> client =
        Config.initializeClient(user: user, swarm: 'public');
    await client.value.mutate(MutationOptions(
        document: gql(GQLApi.updateFcmToken),
        variables: {'uftToken': fcmToken}));
  }

  initMethod() async {
    SharedPreferences _prefs = await SharedPreferences.getInstance();
    FirebaseMessaging messaging = FirebaseMessaging.instance;
    String? fcmToken = await messaging.getToken();
    if (fcmToken != null) {
      print('fcmToken ' + fcmToken);
    }
    FirebaseMessaging.onMessage.listen((RemoteMessage message) {
      print('Received message ' + (message.notification?.title ?? 'no title'));
    });

    if (fcmToken != null) {
      await saveFcmTokenToBackend(user: _user, fcmToken: fcmToken);
      messaging.onTokenRefresh
          .listen((fcm) => saveFcmTokenToBackend(user: _user, fcmToken: fcm));
    }

    String? _s = _prefs.getString('swarm');
    String? _n = _prefs.getString('swarmName');
    _swarmInfoStream.add(SwarmInfo(_s ?? 'public', _n ?? 'public'));
  }

  void setSwarm(String _newSwarm, String _newSwarmName) async {
    SharedPreferences _prefs = await SharedPreferences.getInstance();
    _prefs.setString('swarm', _newSwarm);
    _prefs.setString('swarmName', _newSwarmName);
    _swarmInfoStream.add(SwarmInfo(_newSwarm, _newSwarmName));
  }

  @override
  Widget build(BuildContext context) {
    return StreamBuilder(
        stream: _swarmInfoStream.stream,
        builder: (BuildContext context, AsyncSnapshot<SwarmInfo> snapshot1) {
          return withSnapshot<SwarmInfo>(
              context: context,
              snapshot: snapshot1,
              builder: (SwarmInfo _swarmInfo) {
                final String _swarm = _swarmInfo.swarm;
                final String _swarmName = _swarmInfo.swarmName;
                final ValueNotifier<GraphQLClient> _client =
                    Config.initializeClient(user: _user, swarm: _swarm);
                return GraphQLProvider(
                    client: _client,
                    child: Scaffold(
                      appBar: AppBar(title: Text(_swarmName), actions: [
                        IconButton(
                            icon: Icon(Icons.menu),
                            onPressed: () {
                              Navigator.push(
                                  context,
                                  MaterialPageRoute(
                                      builder: (ctx) => GraphQLProvider(
                                          client: _client,
                                          child: Menu(setSwarm))));
                            })
                      ]),
                      bottomNavigationBar: BottomAppBar(
                          child: Row(
                              mainAxisAlignment: MainAxisAlignment.spaceBetween,
                              mainAxisSize: MainAxisSize.max,
                              children: [
                            IconButton(
                              icon: Icon(Icons.logout),
                              onPressed: () async {
                                FirebaseAuth.instance.signOut();
                              },
                            ),
                            IconButton(
                              icon: Icon(Icons.create),
                              onPressed: () {
                                Navigator.push(
                                    context,
                                    MaterialPageRoute(
                                        builder: (ctx) => GraphQLProvider(
                                            client: _client,
                                            child: NewBallotForm())));
                              },
                            ),
                            IconButton(
                                icon: Icon(Icons.contacts),
                                onPressed: () async {
                                  String _token = await _user.getIdToken();
                                  Navigator.push(
                                      context,
                                      MaterialPageRoute(
                                          builder: (ctx) => GraphQLProvider(
                                              client: _client,
                                              child: FishView(_token))));
                                })
                          ])),
                      body: Ballots(),
                    ));
              });
        });
  }
}
