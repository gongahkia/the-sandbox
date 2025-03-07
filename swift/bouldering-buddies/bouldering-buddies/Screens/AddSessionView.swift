// AddSessionView.swift
import SwiftUI

struct AddSessionView: View {
    @Binding var sessions: [Session]
    @State private var gym = ""
    @State private var date = Date()
    
    var body: some View {
        Form {
            Section {
                Picker("Gym", selection: $gym) {
                    ForEach(GYMS, id: \.self) {
                        Text($0)
                    }
                }
                DatePicker("Date", selection: $date)
            }
            
            Section {
                Button(action: {
                    sessions.append(Session(date: date, gym: gym, climbs: []))
                }) {
                    Text("Add Session")
                }
            }
        }
        .navigationTitle("Add Session")
    }
}
