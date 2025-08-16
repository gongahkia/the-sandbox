// httpCats 1.0.0 done

// ---------- REQUIRED IMPORTS ----------

import 'dart:io';
import 'dart:convert';
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' as html;

// ---------- PRESET ----------

void main() async {  

  print("http cat will find GIFs for you 🐈");

  // used for testing
  List<String> testUrlList = [
    "http://dart.dev",
    "https://www.smulit.org/",
    "https://jojowiki.com/Category:Images_of_Killer_Queen"
  ];

  List<String> genericUrlList = [
    "https://tenor.com/search/{SEARCH_QUERY}-gifs",
    "https://giphy.com/explore/{SEARCH_QUERY}",
    "https://gifdb.com/{SEARCH_QUERY}"
  ];

  // handle user input
  print("Enter your search query:");
  String? query = stdin.readLineSync();
  String cleanedQuery = query?.trim() ?? "funny-cat";
  List<String> finUrlList = [];
  genericUrlList.forEach((genericURL){
    String finUrl = genericURL.replaceFirst("{SEARCH_QUERY}", cleanedQuery);
    finUrlList.add(finUrl);
  });


// ---------- FUNCTION DECLARATION ----------

String? rStripS(String a) {
  if (a != null && a.endsWith("s")) {
    // Use safe navigation operators and null-aware checks
    List<String> charList = a.split('');
    if (charList.length > 1) {
      return charList.sublist(0, charList.length - 1).join('');
    } else {
      return a;
    }
  }
}

void debugLogFile(response){
  var document = html.parse(response.body);
  var bodyElement = document.body;
  File('log.txt').writeAsString(bodyElement?.innerHtml ?? '').then((file) {
    print('Output written to log.txt');
  }).catchError((exception) {
    print('Error writing file: $exception');
  });
}

List<String?> cleanTenor(dynamic inp){
  List<String?> out = [];
  var document = html.parse(inp.body);
  var images = document.getElementsByTagName('img');
  images.forEach((image){ 
    String? imageSrc = image.attributes["src"];
    if (imageSrc?.split(":")[0] == "https"){
      out.add(imageSrc);
    } else {}
  });
  return out;
}

List<String?> cleanGiphy(dynamic inp){
  List<String?> out = [];
  var document = html.parse(inp.body);
  var images = document.getElementsByTagName('img');
  images.forEach((image){ 
    String? imageSrc = image.attributes["src"]; 
    if (imageSrc?.split(":")[0] == "https"){
      out.add(imageSrc);
    } else {}
  });
  return out;
}

List<String?> cleanGifdb(dynamic inp){
  List<String?> out = [];
  var document = html.parse(inp.body);
  var images = document.getElementsByTagName('img');
  images.forEach((image){ 
    String? imageDataSrc = image.attributes["data-src"];
    if (imageDataSrc != null){
      out.add("https://gifdb.com$imageDataSrc");
    } else {}
  });
  return out;
}

// ---------- EXECUTION CODE ----------

// ----- SITE PARSING PHASE -----

Map<String, List<String?>> requestedURLMap = {};

  try {
    for (var url in finUrlList){
      var response = await http.get(Uri.parse(url));
      if (response.statusCode == 200){
        RegExp tenorRegex = RegExp(r'https://tenor\.com/search/.*');
        RegExp giphyRegex = RegExp(r'https://giphy\.com/explore/.*');
        RegExp gifdbRegex = RegExp(r'https://gifdb\.com/.*');
        // debugLogFile(response); // provides debug info
        if (tenorRegex.hasMatch(url)) {
          requestedURLMap["tenor"] = cleanTenor(response);
        } else if (giphyRegex.hasMatch(url)) {
          requestedURLMap["giphy"] = cleanGiphy(response);
        } else if (gifdbRegex.hasMatch(url)) {
          requestedURLMap["gifb"] = cleanGifdb(response);
        } else {
        }
      } else {
        print("Failed to load page: ${response.statusCode}");
      }
    }
  } catch(exception) {
    print("Error: $exception");
  }

  String finJsonString = jsonEncode(requestedURLMap);
  print(finJsonString);

  // ----- DOWNLOAD PHASE -----

  // intiated within the makefile and loader.sh

}