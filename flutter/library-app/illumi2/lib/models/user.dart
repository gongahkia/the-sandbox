class User {
  final int id;
  final String email;
  final String authUid;
  final DateTime createdAt;

  User({
    required this.id,
    required this.email,
    required this.authUid,
    required this.createdAt,
  });

  factory User.fromJson(Map<String, dynamic> json) {
    return User(
      id: json['id'],
      email: json['email'],
      authUid: json['auth_uid'],
      createdAt: DateTime.parse(json['created_at']),
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'email': email,
      'auth_uid': authUid,
      'created_at': createdAt.toIso8601String(),
    };
  }
}