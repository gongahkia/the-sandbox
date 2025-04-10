use bevy::prelude::*;
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug, Deserialize, Serialize, Clone)]
pub enum HitObjectType {
    Circle,
    Slider,
    Spinner,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct HitObject {
    pub position: Vec2,
    pub time: f32,  // Time in milliseconds
    pub object_type: HitObjectType,
    pub duration: Option<f32>,  // For sliders and spinners
    pub curve_points: Option<Vec<Vec2>>,  // For sliders
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct Beatmap {
    pub title: String,
    pub artist: String,
    pub creator: String,
    pub difficulty: String,
    pub audio_file: String,
    pub background_file: Option<String>,
    pub hit_objects: Vec<HitObject>,
    pub approach_rate: f32,
    pub circle_size: f32,
    pub overall_difficulty: f32,
    pub hp_drain_rate: f32,
}

impl Beatmap {
    pub fn load(path: &Path) -> Result<Self, String> {
        let mut file = File::open(path).map_err(|e| format!("Failed to open beatmap file: {}", e))?;
        let mut contents = String::new();
        file.read_to_string(&mut contents).map_err(|e| format!("Failed to read beatmap file: {}", e))?;
        
        // For a real implementation, you'd need to parse the .osu file format
        // Here we're assuming a simplified JSON format for demonstration
        serde_json::from_str(&contents).map_err(|e| format!("Failed to parse beatmap: {}", e))
    }
    
    pub fn approach_time(&self) -> f32 {
        // Convert AR to approach time in milliseconds
        // AR 5 is 1200ms, AR 10 is 450ms
        1200.0 - (self.approach_rate - 5.0) * 150.0
    }
    
    pub fn hit_circle_radius(&self) -> f32 {
        // Convert CS to radius in pixels
        // CS 0 is 54.4px, CS 10 is 23.2px
        54.4 - 3.12 * self.circle_size
    }
    
    pub fn hit_window_300(&self) -> f32 {
        // Convert OD to timing window in milliseconds
        // OD 0 is 80ms, OD 10 is 20ms
        80.0 - 6.0 * self.overall_difficulty
    }
}