// SessionListView.swift
import SwiftUI

struct SessionListView: View {
    @State private var sessions: [Session] = [
        Session(date: Date(), gym: GYMS.first!, climbs: [])
    ]
    
    var body: some View {
        NavigationView {
            List(sessions) { session in
                NavigationLink(destination: SessionDetailsView(session: session)) {
                    VStack(alignment: .leading) {
                        Text("Session at \(session.gym) on \(session.date.formatted())")
                            .font(.headline)
                        Text("\(session.climbs.count) climbs")
                            .foregroundColor(.secondary)
                    }
                }
            }
            .navigationTitle("Bouldering Sessions")
            .toolbar {
                ToolbarItem(placement: .automatic) {
                    NavigationLink(destination: AddSessionView(sessions: $sessions)) {
                        Text("Add Session")
                    }
                }
            }
        }
    }
}
