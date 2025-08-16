use bevy::prelude::*;
use crate::game::GameData;
use crate::states::AppState;

pub struct ResultPlugin;

impl Plugin for ResultPlugin {
    fn build(&self, app: &mut App) {
        app
            .add_systems(OnEnter(AppState::Results), setup_results)
            .add_systems(Update, handle_results_input.run_if(in_state(AppState::Results)))
            .add_systems(OnExit(AppState::Results), cleanup_results);
    }
}

#[derive(Component)]
struct ResultsUi;

fn setup_results(
    mut commands: Commands,
    game_data: Res<GameData>,
    asset_server: Res<AssetServer>,
) {
    let font = asset_server.load("fonts/NotoSans-Regular.ttf");

    commands
        .spawn((
            NodeBundle {
                style: Style {
                    width: Val::Percent(100.0),
                    height: Val::Percent(100.0),
                    flex_direction: FlexDirection::Column,
                    align_items: AlignItems::Center,
                    justify_content: JustifyContent::Center,
                    ..default()
                },
                background_color: Color::rgb(0.1, 0.1, 0.1).into(),
                ..default()
            },
            ResultsUi,
        ))
        .with_children(|parent| {
            parent.spawn(TextBundle::from_section(
                "Results",
                TextStyle {
                    font: font.clone(),
                    font_size: 60.0,
                    color: Color::WHITE,
                },
            ));

            parent.spawn(TextBundle::from_section(
                format!("Score: {}", game_data.score),
                TextStyle {
                    font: font.clone(),
                    font_size: 40.0,
                    color: Color::WHITE,
                },
            ));

            parent.spawn(TextBundle::from_section(
                format!("Accuracy: {:.2}%", game_data.accuracy),
                TextStyle {
                    font: font.clone(),
                    font_size: 40.0,
                    color: Color::WHITE,
                },
            ));

            parent.spawn(TextBundle::from_section(
                format!("Max Combo: {}", game_data.max_combo),
                TextStyle {
                    font: font.clone(),
                    font_size: 40.0,
                    color: Color::WHITE,
                },
            ));

            parent.spawn(TextBundle::from_section(
                "Press SPACE to return to the main menu",
                TextStyle {
                    font: font.clone(),
                    font_size: 30.0,
                    color: Color::GRAY,
                },
            ));
        });
}

fn handle_results_input(
    keyboard_input: Res<Input<KeyCode>>,
    mut app_state: ResMut<NextState<AppState>>,
) {
    if keyboard_input.just_pressed(KeyCode::Space) {
        app_state.set(AppState::MainMenu);
    }
}

fn cleanup_results(mut commands: Commands, query: Query<Entity, With<ResultsUi>>) {
    for entity in query.iter() {
        commands.entity(entity).despawn_recursive();
    }
}