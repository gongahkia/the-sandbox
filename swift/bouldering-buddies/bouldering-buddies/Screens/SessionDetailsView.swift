// SessionDetailsView.swift
import SwiftUI

struct SessionDetailsView: View {
    let session: Session
    
    var body: some View {
        VStack(alignment: .leading) {
            Text("Session Details")
                .font(.largeTitle)
            
            Text("Date: \(session.date.formatted())")
            Text("Gym: \(session.gym)")
            
            List(session.climbs) { climb in
                VStack(alignment: .leading) {
                    Text(climb.name)
                        .font(.headline)
                    Text("Grade: \(climb.grade), Attempts: \(climb.attempts)")
                        .foregroundColor(.secondary)
                    Text("Terrain: \(climb.terrainType.rawValue.capitalized)")
                        .foregroundColor(.secondary)
                    Text(climb.description)
                        .foregroundColor(.secondary)
                }
            }
            
            Button(action: {
                // Add logic to add a new climb here
            }) {
                Text("Add Climb")
            }
        }
        .padding()
    }
}
