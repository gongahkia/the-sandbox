class BoulderingSession {
  final String id;
  final String gym;
  final String difficulty;
  final int tries;
  final String description;
  final String terrainType;
  final DateTime date;

  BoulderingSession({
    required this.id,
    required this.gym,
    required this.difficulty,
    required this.tries,
    required this.description,
    required this.terrainType,
    required this.date,
  });

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'gym': gym,
      'difficulty': difficulty,
      'tries': tries,
      'description': description,
      'terrainType': terrainType,
      'date': date.toIso8601String(),
    };
  }

  factory BoulderingSession.fromJson(Map<String, dynamic> json) {
    return BoulderingSession(
      id: json['id'],
      gym: json['gym'],
      difficulty: json['difficulty'],
      tries: json['tries'],
      description: json['description'],
      terrainType: json['terrainType'],
      date: DateTime.parse(json['date']),
    );
  }
}