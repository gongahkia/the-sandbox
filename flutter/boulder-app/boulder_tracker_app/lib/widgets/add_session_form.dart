import 'package:flutter/material.dart';
import 'package:provider/provider.dart';
import 'package:uuid/uuid.dart';
import 'package:boulder_tracker_app/constants.dart';
import 'package:boulder_tracker_app/models/bouldering_session.dart';
import 'package:boulder_tracker_app/providers/session_provider.dart';

class AddSessionForm extends StatefulWidget {
  @override
  _AddSessionFormState createState() => _AddSessionFormState();
}

class _AddSessionFormState extends State<AddSessionForm> {
  final _formKey = GlobalKey<FormState>();
  String? _selectedGym;
  String? _difficulty;
  int? _tries;
  String? _description;
  String? _terrainType;

  @override
  Widget build(BuildContext context) {
    return Form(
      key: _formKey,
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.stretch,
        children: [
          DropdownButtonFormField<String>(
            value: _selectedGym,
            items: GYMS.map((gym) => DropdownMenuItem(value: gym, child: Text(gym))).toList(),
            onChanged: (value) => setState(() => _selectedGym = value),
            decoration: InputDecoration(labelText: 'Gym'),
            validator: (value) => value == null ? 'Please select a gym' : null,
          ),
          SizedBox(height: 16),
          TextFormField(
            decoration: InputDecoration(labelText: 'Difficulty'),
            onSaved: (value) => _difficulty = value,
            validator: (value) => value!.isEmpty ? 'Please enter difficulty' : null,
          ),
          SizedBox(height: 16),
          TextFormField(
            decoration: InputDecoration(labelText: 'Number of Tries'),
            keyboardType: TextInputType.number,
            onSaved: (value) => _tries = int.tryParse(value ?? ''),
            validator: (value) => value!.isEmpty ? 'Please enter number of tries' : null,
          ),
          SizedBox(height: 16),
          TextFormField(
            decoration: InputDecoration(labelText: 'Description'),
            onSaved: (value) => _description = value,
          ),
          SizedBox(height: 16),
          TextFormField(
            decoration: InputDecoration(labelText: 'Terrain Type'),
            onSaved: (value) => _terrainType = value,
            validator: (value) => value!.isEmpty ? 'Please enter terrain type' : null,
          ),
          SizedBox(height: 24),
          ElevatedButton(
            onPressed: _submitForm,
            child: Text('Add Session'),
          ),
        ],
      ),
    );
  }

  void _submitForm() {
    if (_formKey.currentState!.validate()) {
      _formKey.currentState!.save();
      final newSession = BoulderingSession(
        id: Uuid().v4(),
        gym: _selectedGym!,
        difficulty: _difficulty!,
        tries: _tries!,
        description: _description ?? '',
        terrainType: _terrainType!,
        date: DateTime.now(),
      );
      Provider.of<SessionProvider>(context, listen: false).addSession(newSession);
      Navigator.of(context).pop();
    }
  }
}