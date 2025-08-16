import React, { useState } from 'react';
import { View, Text, TextInput, TouchableOpacity, FlatList, StyleSheet, ScrollView, Modal } from 'react-native';
import { Picker } from '@react-native-picker/picker';

const GYMS = [
  "Adventure HQ (Rockpass)", "Ark Bloc", "BFF Climb (Bendemeer)", "BFF Climb (Tampines OTH)",
  "BFF Climb (Tampines yo:HA)", "Boruda", "Boulder Movement (Bugis+)", "Boulder Movement (OUE Downtown)",
  "Boulder Movement (Tai Seng)", "Boulder Movement (Tekka Place)", "Boulder+ (Aperia Mall)",
  "Boulder+ (The Chevrons)", "Boulder Planet (Sembawang)", "Boulder Planet (Tai Seng)",
  "Boulder World (Paragon)", "Boys Town Adventure Centre", "Climb@T3", "Climba Gym",
  "Climb Central (Katong)", "Climb Central (Novena)"
];

const App = () => {
  const [grade, setGrade] = useState('');
  const [attempts, setAttempts] = useState('');
  const [completed, setCompleted] = useState(false);
  const [description, setDescription] = useState('');
  const [sessions, setSessions] = useState([]);
  const [selectedGym, setSelectedGym] = useState(GYMS[0]);
  const [editingIndex, setEditingIndex] = useState(null);
  const [modalVisible, setModalVisible] = useState(false);

  const addOrUpdateSession = () => {
    if (grade && attempts) {
      const newSession = { grade, attempts: parseInt(attempts), completed, description, gym: selectedGym };
      if (editingIndex !== null) {
        const updatedSessions = [...sessions];
        updatedSessions[editingIndex] = newSession;
        setSessions(updatedSessions);
        setEditingIndex(null);
      } else {
        setSessions([...sessions, newSession]);
      }
      setGrade('');
      setAttempts('');
      setCompleted(false);
      setDescription('');
      setModalVisible(false);
    }
  };

  const editSession = (index) => {
    const session = sessions[index];
    setGrade(session.grade);
    setAttempts(session.attempts.toString());
    setCompleted(session.completed);
    setDescription(session.description || '');
    setSelectedGym(session.gym);
    setEditingIndex(index);
    setModalVisible(true);
  };

  const renderSession = ({ item, index }) => (
    <TouchableOpacity onPress={() => editSession(index)} style={styles.sessionItem}>
      <Text style={styles.sessionText}>Grade: {item.grade}</Text>
      <Text style={styles.sessionText}>Attempts: {item.attempts}</Text>
      <Text style={styles.sessionText}>Completed: {item.completed ? 'Yes' : 'No'}</Text>
      <Text style={styles.sessionText}>Description: {item.description || 'N/A'}</Text>
      <Text style={styles.sessionText}>Gym: {item.gym}</Text>
    </TouchableOpacity>
  );

  return (
    <View style={styles.container}>
      <Text style={styles.title}>Bouldering Tracker</Text>
      <TouchableOpacity style={styles.addButton} onPress={() => setModalVisible(true)}>
        <Text style={styles.buttonText}>Add New Session</Text>
      </TouchableOpacity>
      <FlatList
        data={sessions}
        renderItem={renderSession}
        keyExtractor={(item, index) => index.toString()}
        style={styles.list}
      />
      <Modal
        animationType="slide"
        transparent={true}
        visible={modalVisible}
        onRequestClose={() => setModalVisible(false)}
      >
        <View style={styles.modalView}>
          <ScrollView>
            <TextInput
              style={styles.input}
              placeholder="Grade (e.g., V5)"
              value={grade}
              onChangeText={setGrade}
            />
            <TextInput
              style={styles.input}
              placeholder="Attempts"
              value={attempts}
              onChangeText={setAttempts}
              keyboardType="numeric"
            />
            <TouchableOpacity
              style={[styles.button, completed && styles.buttonCompleted]}
              onPress={() => setCompleted(!completed)}
            >
              <Text style={styles.buttonText}>{completed ? 'Completed' : 'Not Completed'}</Text>
            </TouchableOpacity>
            <TextInput
              style={styles.input}
              placeholder="Description"
              value={description}
              onChangeText={setDescription}
              multiline
            />
            <Picker
              selectedValue={selectedGym}
              onValueChange={(itemValue) => setSelectedGym(itemValue)}
              style={styles.picker}
            >
              {GYMS.map((gym, index) => (
                <Picker.Item key={index} label={gym} value={gym} />
              ))}
            </Picker>
            <TouchableOpacity style={styles.button} onPress={addOrUpdateSession}>
              <Text style={styles.buttonText}>{editingIndex !== null ? 'Update' : 'Add'} Session</Text>
            </TouchableOpacity>
            <TouchableOpacity style={styles.button} onPress={() => setModalVisible(false)}>
              <Text style={styles.buttonText}>Cancel</Text>
            </TouchableOpacity>
          </ScrollView>
        </View>
      </Modal>
    </View>
  );
};

const styles = StyleSheet.create({
  container: {
    flex: 1,
    padding: 20,
    backgroundColor: '#282828', // Gruvbox dark background
  },
  title: {
    fontSize: 32,
    fontWeight: 'bold',
    marginBottom: 20,
    textAlign: 'center',
    color: '#ebdbb2', // Gruvbox light text
    textTransform: 'uppercase',
  },
  input: {
    backgroundColor: '#3c3836', // Gruvbox dark gray
    borderWidth: 2,
    borderColor: '#ebdbb2', // Gruvbox light border
    borderRadius: 0,
    padding: 10,
    marginBottom: 10,
    fontSize: 16,
    color: '#ebdbb2', // Gruvbox light text
  },
  button: {
    backgroundColor: '#98971a', // Gruvbox green
    borderWidth: 2,
    borderColor: '#ebdbb2', // Gruvbox light border
    borderRadius: 0,
    padding: 10,
    alignItems: 'center',
    marginBottom: 10,
  },
  buttonCompleted: {
    backgroundColor: '#b8bb26', // Gruvbox light green
  },
  buttonText: {
    color: '#282828', // Gruvbox dark text
    fontSize: 16,
    fontWeight: 'bold',
  },
  list: {
    flex: 1,
  },
  sessionItem: {
    backgroundColor: '#3c3836', // Gruvbox dark gray
    borderWidth: 2,
    borderColor: '#ebdbb2', // Gruvbox light border
    borderRadius: 0,
    padding: 10,
    marginBottom: 10,
  },
  sessionText: {
    fontSize: 16,
    marginBottom: 5,
    color: '#ebdbb2', // Gruvbox light text
  },
  addButton: {
    backgroundColor: '#d79921', // Gruvbox yellow
    borderWidth: 2,
    borderColor: '#ebdbb2', // Gruvbox light border
    borderRadius: 0,
    padding: 10,
    alignItems: 'center',
    marginBottom: 10,
  },
  modalView: {
    margin: 20,
    backgroundColor: '#282828', // Gruvbox dark background
    borderRadius: 0,
    padding: 35,
    alignItems: 'center',
    shadowColor: '#000',
    shadowOffset: {
      width: 0,
      height: 2
    },
    shadowOpacity: 0.25,
    shadowRadius: 4,
    elevation: 5
  },
  picker: {
    width: '100%',
    backgroundColor: '#3c3836', // Gruvbox dark gray
    color: '#ebdbb2', // Gruvbox light text
    marginBottom: 10,
  },
});

export default App;