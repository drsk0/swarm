import 'package:flutter/material.dart';
import 'package:flutter_markdown/flutter_markdown.dart';

class ProfileView extends StatelessWidget {
  final String _profile;
  ProfileView(this._profile);
  @override
  build(BuildContext context) {
    return Scaffold(
        appBar: AppBar(title: Text('Profile')),
        body: SingleChildScrollView(
            child: MarkdownBody(
          data: _profile,
        )));
  }
}
