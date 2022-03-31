import 'package:flutter/material.dart';
import 'package:flutter_markdown/flutter_markdown.dart';

class MarkdownPreview extends StatelessWidget {
  final String _content;
  MarkdownPreview(this._content);

  @override
  build(BuildContext context) {
    return Scaffold(
        appBar: AppBar(title: Text('Preview')),
        body: MarkdownBody(data: _content));
  }
}
