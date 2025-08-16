use bevy::prelude::*;
use crate::game::beatmap::Beatmap;
use crate::game::hit_objects::{HitCircle, HitCircleState, HitScore};
use crate::states::AppState;

pub struct GamePlugin;

impl Plugin for GamePlugin {
    fn build(&self, app: &mut App) {
        app
            .add_systems(OnEnter(AppState::Playing), setup_game)
            .add_systems(OnExit(AppState::Playing), cleanup_game)
            .add_systems(Update, 
                (
                    spawn_hit_objects,
                    update_hit_objects,
                    handle_input,
                    update_score,
                ).run_if(in_state(AppState::Playing))
            );
    }
}

#[derive(Resource)]
pub struct GameData {
    pub beatmap: Beatmap,
    pub game_time: f32,
    pub score: u32,
    pub combo: u32,
    pub max_combo: u32,
    pub accuracy: f32,
    pub hit_300_count: u32,
    pub hit_100_count: u32,
    pub hit_50_count: u32,
    pub miss_count: u32,
    pub next_hit_object_index: usize,
}

fn setup_game(mut commands: Commands, asset_server: Res<AssetServer>) {
    // Load a sample beatmap
    let beatmap = Beatmap::load(std::path::Path::new("assets/beatmaps/sample.json"))
        .expect("Failed to load beatmap");
    
    commands.insert_resource(GameData {
        beatmap,
        game_time: 0.0,
        score: 0,
        combo: 0,
        max_combo: 0,
        accuracy: 100.0,
        hit_300_count: 0,
        hit_100_count: 0,
        hit_50_count: 0,
        miss_count: 0,
        next_hit_object_index: 0,
    });
    
    // Load audio
    // In a real implementation, you'd use rodio or bevy_audio to play the song
}

fn cleanup_game(mut commands: Commands, query: Query<Entity, With<HitCircle>>) {
    for entity in query.iter() {
        commands.entity(entity).despawn_recursive();
    }
    commands.remove_resource::<GameData>();
}

fn spawn_hit_objects(
    mut commands: Commands,
    mut game_data: ResMut<GameData>,
    time: Res<Time>,
    asset_server: Res<AssetServer>,
) {
    // Update game time
    game_data.game_time += time.delta_seconds() * 1000.0; // Convert to ms
    
    // Spawn hit objects that should appear now
    while game_data.next_hit_object_index < game_data.beatmap.hit_objects.len() {
        let hit_object = &game_data.beatmap.hit_objects[game_data.next_hit_object_index];
        
        // If it's time to spawn this hit object
        if game_data.game_time >= hit_object.time - game_data.beatmap.approach_time() {
            match hit_object.object_type {
                HitObjectType::Circle => {
                    let texture = asset_server.load("textures/hit_circle.png");
                    let approach_circle = asset_server.load("textures/approach_circle.png");
                    
                    commands.spawn((
                        SpriteBundle {
                            texture,
                            transform: Transform::from_translation(Vec3::new(
                                hit_object.position.x, 
                                hit_object.position.y, 
                                1.0
                            )),
                            sprite: Sprite {
                                custom_size: Some(Vec2::new(
                                    game_data.beatmap.hit_circle_radius() * 2.0,
                                    game_data.beatmap.hit_circle_radius() * 2.0
                                )),
                                ..default()
                            },
                            ..default()
                        },
                        HitCircle::new(
                            hit_object,
                            game_data.beatmap.approach_time(),
                            game_data.beatmap.hit_circle_radius(),
                            game_data.beatmap.hit_window_300()
                        ),
                    ));
                    
                    // Also spawn approach circle
                    commands.spawn(SpriteBundle {
                        texture: approach_circle,
                        transform: Transform::from_translation(Vec3::new(
                            hit_object.position.x, 
                            hit_object.position.y, 
                            0.9
                        )),
                        sprite: Sprite {
                            custom_size: Some(Vec2::new(
                                game_data.beatmap.hit_circle_radius() * 6.0,
                                game_data.beatmap.hit_circle_radius() * 6.0
                            )),
                            ..default()
                        },
                        ..default()
                    });
                },
                // Handle other hit object types similarly
                _ => {}
            }
            
            game_data.next_hit_object_index += 1;
        } else {
            break;
        }
    }
}

fn update_hit_objects(
    mut commands: Commands,
    mut game_data: ResMut<GameData>,
    mut query: Query<(Entity, &mut HitCircle, &mut Transform)>,
    time: Res<Time>,
) {
    for (entity, mut hit_circle, mut transform) in query.iter_mut() {
        // Update hit circle state
        hit_circle.update(game_data.game_time);
        
        // Update approach circle size
        if hit_circle.state == HitCircleState::Approaching {
            let time_left = hit_circle.hit_time - game_data.game_time;
            let scale_factor = time_left / hit_circle.approach_time;
            transform.scale = Vec3::new(1.0 + scale_factor * 2.0, 1.0 + scale_factor * 2.0, 1.0);
        }
        
        // Remove missed hit circles
        if let HitCircleState::Missed = hit_circle.state {
            commands.entity(entity).despawn_recursive();
            game_data.miss_count += 1;
            game_data.combo = 0;
            // Update accuracy
            let total_hits = game_data.hit_300_count + game_data.hit_100_count + 
                            game_data.hit_50_count + game_data.miss_count;
            if total_hits > 0 {
                game_data.accuracy = (game_data.hit_300_count * 300 + game_data.hit_100_count * 100 + 
                                    game_data.hit_50_count * 50) as f32 / (total_hits * 300) as f32 * 100.0;
            }
        }
        
        // Remove hit circles
        if let HitCircleState::Hit(_) = hit_circle.state {
            commands.entity(entity).despawn_recursive();
        }
    }
}

fn handle_input(
    mut commands: Commands,
    buttons: Res<Input<MouseButton>>,
    windows: Query<&Window>,
    camera_q: Query<(&Camera, &GlobalTransform)>,
    mut game_data: ResMut<GameData>,
    mut hit_circles: Query<(Entity, &mut HitCircle, &Transform)>,
) {
    if buttons.just_pressed(MouseButton::Left) {
        let window = windows.single();
        let (camera, camera_transform) = camera_q.single();
        
        if let Some(cursor_position) = window.cursor_position() {
            // Convert screen position to world position
            let world_position = camera
                .viewport_to_world(camera_transform, cursor_position)
                .map(|ray| ray.origin.truncate())
                .unwrap_or_default();
            
            // Find the closest hittable circle
            let mut closest_hit_circle = None;
            let mut closest_distance = f32::MAX;
            
            for (entity, hit_circle, transform) in hit_circles.iter() {
                if hit_circle.state == HitCircleState::Hittable {
                    let circle_position = transform.translation.truncate();
                    let distance = world_position.distance(circle_position);
                    
                    if distance <= hit_circle.radius && distance < closest_distance {
                        closest_hit_circle = Some((entity, hit_circle.clone()));
                        closest_distance = distance;
                    }
                }
            }
            
            // If we found a hit circle, try to hit it
            if let Some((entity, mut hit_circle)) = closest_hit_circle {
                if let Some(score) = hit_circle.try_hit(game_data.game_time) {
                    match score {
                        HitScore::Hit300 => {
                            game_data.hit_300_count += 1;
                            game_data.combo += 1;
                        },
                        HitScore::Hit100 => {
                            game_data.hit_100_count += 1;
                            game_data.combo += 1;
                        },

// ... (previous code)

fn update_score(
    mut game_data: ResMut<GameData>,
    query: Query<&HitCircle, Changed<HitCircle>>,
) {
    for hit_circle in query.iter() {
        if let HitCircleState::Hit(score) = hit_circle.state {
            match score {
                HitScore::Hit300 => {
                    game_data.hit_300_count += 1;
                    game_data.combo += 1;
                    game_data.score += 300 * game_data.combo as u32;
                },
                HitScore::Hit100 => {
                    game_data.hit_100_count += 1;
                    game_data.combo += 1;
                    game_data.score += 100 * game_data.combo as u32;
                },
                HitScore::Hit50 => {
                    game_data.hit_50_count += 1;
                    game_data.combo += 1;
                    game_data.score += 50 * game_data.combo as u32;
                },
                HitScore::Miss => {
                    game_data.miss_count += 1;
                    game_data.combo = 0;
                },
            }

            game_data.max_combo = game_data.max_combo.max(game_data.combo);

            // Update accuracy
            let total_hits = game_data.hit_300_count + game_data.hit_100_count + 
                            game_data.hit_50_count + game_data.miss_count;
            if total_hits > 0 {
                game_data.accuracy = (game_data.hit_300_count * 300 + game_data.hit_100_count * 100 + 
                                    game_data.hit_50_count * 50) as f32 / (total_hits * 300) as f32 * 100.0;
            }
        }
    }
}

fn check_game_end(
    mut game_data: ResMut<GameData>,
    mut app_state: ResMut<NextState<AppState>>,
) {
    if game_data.next_hit_object_index >= game_data.beatmap.hit_objects.len() &&
       game_data.game_time > game_data.beatmap.hit_objects.last().unwrap().time + 2000.0 {
        // Transition to results screen
        app_state.set(AppState::Results);
    }
}

// Add this system to the GamePlugin
.add_systems(Update, check_game_end.run_if(in_state(AppState::Playing)))