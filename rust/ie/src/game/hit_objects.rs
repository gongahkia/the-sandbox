use bevy::prelude::*;
use crate::game::beatmap::{HitObject, HitObjectType};

#[derive(Component)]
pub struct HitCircle {
    pub hit_time: f32,
    pub position: Vec2,
    pub radius: f32,
    pub approach_time: f32,
    pub hit_window_300: f32,
    pub hit_window_100: f32,
    pub hit_window_50: f32,
    pub state: HitCircleState,
}

#[derive(PartialEq)]
pub enum HitCircleState {
    Approaching,
    Hittable,
    Hit(HitScore),
    Missed,
}

#[derive(PartialEq, Clone, Copy)]
pub enum HitScore {
    Miss,
    Hit50,
    Hit100,
    Hit300,
}

impl HitCircle {
    pub fn new(hit_object: &HitObject, approach_time: f32, circle_radius: f32, hit_window_300: f32) -> Self {
        let hit_window_100 = hit_window_300 * 2.0;
        let hit_window_50 = hit_window_300 * 3.0;
        
        Self {
            hit_time: hit_object.time,
            position: hit_object.position,
            radius: circle_radius,
            approach_time,
            hit_window_300,
            hit_window_100,
            hit_window_50,
            state: HitCircleState::Approaching,
        }
    }
    
    pub fn update(&mut self, current_time: f32) {
        if self.state == HitCircleState::Approaching && current_time >= self.hit_time - self.approach_time {
            self.state = HitCircleState::Hittable;
        }
        
        if self.state == HitCircleState::Hittable && current_time > self.hit_time + self.hit_window_50 {
            self.state = HitCircleState::Missed;
        }
    }
    
    pub fn try_hit(&mut self, current_time: f32) -> Option<HitScore> {
        if self.state != HitCircleState::Hittable {
            return None;
        }
        
        let time_diff = (current_time - self.hit_time).abs();
        
        let score = if time_diff <= self.hit_window_300 {
            HitScore::Hit300
        } else if time_diff <= self.hit_window_100 {
            HitScore::Hit100
        } else if time_diff <= self.hit_window_50 {
            HitScore::Hit50
        } else {
            HitScore::Miss
        };
        
        self.state = HitCircleState::Hit(score);
        Some(score)
    }
}
