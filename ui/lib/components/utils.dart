import 'package:flutter/material.dart';
import 'package:fluttertoast/fluttertoast.dart';
import 'package:graphql_flutter/graphql_flutter.dart';

class UtilFs {
  static showToast(String message, BuildContext context) {
    Fluttertoast.showToast(
        msg: message,
        toastLength: Toast.LENGTH_LONG,
        gravity: ToastGravity.BOTTOM);
  }
}

Widget withQueryResult(
    {required BuildContext context,
    required QueryResult result,
    required String field,
    VoidCallback? refetch,
    required Widget Function(dynamic data) builder}) {
  Widget _child = Center(
      child: Text('Something wen\'t wrong.',
          style: Theme.of(context).textTheme.bodyText2));
  if (result.data?[field] != null) {
    _child = builder(result.data![field]);
  }

  if (result.isLoading) {
    _child = Center(
        child:
            Text('loading ...', style: Theme.of(context).textTheme.bodyText2));
  }

  if (result.hasException) {
    _child = Scaffold(
        body: Center(
            child: Text('Something wen\'t wrong.',
                style: Theme.of(context).textTheme.bodyText2)));
    if (result.exception is OperationException) {
      _child = Scaffold(
          body: Center(
        child: GestureDetector(
            child: Text('Network is down or server unreachable. Tap to retry.',
                style: Theme.of(context).textTheme.bodyText2),
            onTap: refetch),
      ));
    }
  }
  return _child;
}

Widget withSnapshot<T>(
    {required BuildContext context,
    required AsyncSnapshot<T> snapshot,
    required Widget Function(T data) builder}) {
  Widget child = Center(child: Text('Something went wrong.'));
  switch (snapshot.connectionState) {
    case ConnectionState.none:
      child = Center(child: Text('Connection: none.'));
      break;
    case ConnectionState.waiting:
      child = Center(child: CircularProgressIndicator());
      break;
    case ConnectionState.active:
      if (snapshot.data != null) {
        child = builder(snapshot.data!);
      } else {
        child = Center(child: Text('No data.'));
      }
      break;
    case ConnectionState.done:
      child = Center(child: Text('Connection done.'));
  }
  return child;
}

class Maybe<T> {}

class Just<T> extends Maybe<T> {
  final T a;
  Just(this.a);

  T get content {
    return a;
  }
}

class Nothing<T> extends Maybe<T> {
  Nothing();
}

T fromMaybe<T>(T def, Maybe<T> m) {
  if (m is Just)
    return (m as Just).content;
  else
    return def;
}

Maybe<T> toMaybe<T>(T? m) {
  return m != null ? Just(m) : Nothing();
}
