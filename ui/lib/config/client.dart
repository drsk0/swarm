import 'package:flutter/material.dart';
import 'package:graphql_flutter/graphql_flutter.dart';
import 'package:firebase_auth/firebase_auth.dart';

HttpLink httpLink({required String swarm}) {
  return HttpLink('https://swarmapp.org/api/query',
      defaultHeaders: {'swarm': swarm});
}

// HttpLink httpLink({required String swarm}) {
// return HttpLink('http://10.0.2.2:8000/api/query',
// defaultHeaders: {'swarm': swarm});
// }

AuthLink authLink(User user) {
  return AuthLink(getToken: () => user.getIdToken());
}

Link link({required User user, required String swarm}) {
  return Link.from([
    // common links run before every request
    DedupeLink(), // dedupe requests
    // TODO (drsk) factor out error handlers.
    ErrorLink(onException: (request, forward, response) {
      print(
          'link exception: ' + request.toString() + ' ' + response.toString());
      return (null);
    }, onGraphQLError: (request, forward, response) {
      print('graphql exception: ' +
          request.toString() +
          ' ' +
          response.toString());
      return (null);
    }),
  ]).split(
      (request) => request.isSubscription,
      authLink(user).concat(wsLink(swarm: swarm)),
      authLink(user).concat(httpLink(swarm: swarm)));
}

WebSocketLink wsLink({required String swarm}) {
  return WebSocketLink(
    // 'ws://10.0.2.2:8000/api/subscribe',
    'wss://swarmapp.org/api/subscribe',
    config: SocketClientConfig(
      autoReconnect: true,
      inactivityTimeout: null,
    ),
  );
}

class Config {
  static ValueNotifier<GraphQLClient> initializeClient(
      {required User user, required String swarm}) {
    ValueNotifier<GraphQLClient> client = ValueNotifier(GraphQLClient(
            // defaultPolicies: DefaultPolicies(
            // query: Policies(
            // fetch: FetchPolicy.noCache,
            // error: ErrorPolicy.none,
            // cacheReread: CacheRereadPolicy.ignoreAll,
            // )),
            link: link(user: user, swarm: swarm),
            cache: new GraphQLCache())
        // cache: new GraphQLCache(store: HiveStore(), typePolicies: {
        // 'WithId': TypePolicy(keyFields: {'wiId': true}),
        // 'WithRef': TypePolicy(keyFields: {'wiRef': true}),
        // })),
        );
    return client;
  }
}
