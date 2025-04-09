// Models.swift
import Foundation
import SwiftUI

enum TerrainType: String, CaseIterable {
    case overhang, coordination, slab, straightWall, other
}

struct Climb: Identifiable {
    let id = UUID()
    var name: String
    var grade: String
    var attempts: Int
    var terrainType: TerrainType
    var description: String
}

struct Session: Identifiable {
    let id = UUID()
    var date: Date
    var gym: String
    var climbs: [Climb]
}

let GYMS = [
    "Adventure HQ (Rockpass)", "Ark Bloc", "BFF Climb (Bendemeer)", "BFF Climb (Tampines OTH)",
    "BFF Climb (Tampines yo:HA)", "Boruda", "Boulder Movement (Bugis+)", "Boulder Movement (OUE Downtown)",
    "Boulder Movement (Tai Seng)", "Boulder Movement (Tekka Place)", "Boulder+ (Aperia Mall)",
    "Boulder+ (The Chevrons)", "Boulder Planet (Sembawang)", "Boulder Planet (Tai Seng)",
    "Boulder World (Paragon)", "Boys Town Adventure Centre", "Climb@T3", "Climba Gym",
    "Climb Central (Katong)", "Climb Central (Novena)"
]
