import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import '../services/auth_service.dart';
import '../services/book_service.dart';
import 'login_screen.dart';

class HomeScreen extends StatefulWidget {
  @override
  _HomeScreenState createState() => _HomeScreenState();
}

class _HomeScreenState extends State<HomeScreen> {
  final BookService _bookService = BookService();
  final TextEditingController _searchController = TextEditingController();
  List<Book> _searchResults = [];
  List<Book> _favorites = [];

  @override
  void initState() {
    super.initState();
    _loadFavorites();
  }

  void _loadFavorites() async {
    final user = Provider.of<AuthService>(context, listen: false).user;
    if (user != null) {
      final favorites = await _bookService.getFavorites(user);
      setState(() {
        _favorites = favorites;
      });
    }
  }

  @override
  Widget build(BuildContext context) {
    return DefaultTabController(
      length: 2,
      child: Scaffold(
        appBar: AppBar(
          title: Text('OpenLibrary Favorites'),
          actions: [
            IconButton(
              icon: Icon(Icons.logout),
              onPressed: () => _logout(context),
            ),
          ],
          bottom: TabBar(
            tabs: [
              Tab(text: 'Search'),
              Tab(text: 'Favorites'),
            ],
          ),
        ),
        body: TabBarView(
          children: [
            _buildSearchTab(),
            _buildFavoritesTab(),
          ],
        ),
      ),
    );
  }

  Widget _buildSearchTab() {
    return Column(
      children: [
        Padding(
          padding: EdgeInsets.all(8.0),
          child: TextField(
            controller: _searchController,
            decoration: InputDecoration(
              labelText: 'Search books',
              suffixIcon: IconButton(
                icon: Icon(Icons.search),
                onPressed: _searchBooks,
              ),
            ),
          ),
        ),
        Expanded(
          child: ListView.builder(
            itemCount: _searchResults.length,
            itemBuilder: (context, index) {
              final book = _searchResults[index];
              return ListTile(
                title: Text(book.title),
                subtitle: Text(book.author),
                trailing: IconButton(
                  icon: Icon(Icons.favorite_border),
                  onPressed: () => _addToFavorites(book),
                ),
              );
            },
          ),
        ),
      ],
    );
  }

  Widget _buildFavoritesTab() {
    return ListView.builder(
      itemCount: _favorites.length,
      itemBuilder: (context, index) {
        final book = _favorites[index];
        return ListTile(
          title: Text(book.title),
          subtitle: Text(book.author),
        );
      },
    );
  }

  void _searchBooks() async {
    final query = _searchController.text;
    if (query.isNotEmpty) {
      final results = await _bookService.searchBooks(query);
      setState(() {
        _searchResults = results;
      });
    }
  }

  void _addToFavorites(Book book) async {
    final user = Provider.of<AuthService>(context, listen: false).user;
    if (user != null) {
      await _bookService.addFavorite(user, book);
      _loadFavorites();
    }
  }

  void _logout(BuildContext context) async {
    final authService = Provider.of<AuthService>(context, listen: false);
    await authService.signOut();
    Navigator.pushReplacement(
      context,
      MaterialPageRoute(builder: (_) => LoginScreen()),
    );
  }
}