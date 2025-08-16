import SwiftUI

struct AddClimbView: View {
    @Binding var session: Session
    @State private var climbName = ""
    @State private var climbGrade = ""
    @State private var attempts = ""
    @State private var terrainType: TerrainType = .slab
    @State private var description = ""
    
    var body: some View {
        NavigationView {
            Form {
                Section {
                    TextField("Climb Name", text: $climbName)
                    TextField("Climb Grade", text: $climbGrade)
                    TextField("Attempts", text: $attempts)
                    Picker("Terrain Type", selection: $terrainType) {
                        ForEach(TerrainType.allCases, id: \.self) {
                            Text($0.rawValue.capitalized)
                        }
                    }
                    TextEditor(text: $description)
                        .frame(height: 100)
                }
                
                Section {
                    Button(action: {
                        if let attemptsInt = Int(attempts) {
                            session.climbs.append(Climb(name: climbName, grade: climbGrade, attempts: attemptsInt, terrainType: terrainType, description: description))
                            climbName = ""
                            climbGrade = ""
                            attempts = ""
                            description = ""
                            print("Climb added successfully.")
                        } else {
                            print("Error: Attempts must be a valid integer.")
                        }
                    }) {
                        Text("Add Climb")
                    }
                }
            }
            .navigationTitle("Add Climb")
        }
    }
}
