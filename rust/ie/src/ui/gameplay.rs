use bevy::prelude::*;
use crate::game::GameData;

pub struct GameplayUiPlugin;

impl Plugin for GameplayUiPlugin {
    fn build(&self, app: &mut App) {
        app
            .add_systems(OnEnter(AppState::Playing), setup_gameplay_ui)
            .add_systems(Update, update_gameplay_ui.run_if(in_state(AppState::Playing)))
            .add_systems(OnExit(AppState::Playing), cleanup_gameplay_ui);
    }
}

#[derive(Component)]
struct GameplayUi;

#[derive(Component)]
struct ScoreText;

#[derive(Component)]
struct AccuracyText;

#[derive(Component)]
struct ComboText;

fn setup_gameplay_ui(mut commands: Commands, asset_server: Res<AssetServer>) {
    let font = asset_server.load("fonts/NotoSans-Regular.ttf");

    commands
        .spawn((
            NodeBundle {
                style: Style {
                    width: Val::Percent(100.0),
                    height: Val::Percent(100.0),
                    justify_content: JustifyContent::SpaceBetween,
                    ..default()
                },
                ..default()
            },
            GameplayUi,
        ))
        .with_children(|parent| {
            // Score
            parent.spawn((
                TextBundle::from_section(
                    "Score: 0",
                    TextStyle {
                        font: font.clone(),
                        font_size: 30.0,
                        color: Color::WHITE,
                    },
                )
                .with_style(Style {
                    position_type: PositionType::Absolute,
                    top: Val::Px(10.0),
                    left: Val::Px(10.0),
                    ..default()
                }),
                ScoreText,
            ));

            // Accuracy
            parent.spawn((
                TextBundle::from_section(
                    "Accuracy: 100.00%",
                    TextStyle {
                        font: font.clone(),
                        font_size: 30.0,
                        color: Color::WHITE,
                    },
                )
                .with_style(Style {
                    position_type: PositionType::Absolute,
                    top: Val::Px(50.0),
                    left: Val::Px(10.0),
                    ..default()
                }),
                AccuracyText,
            ));

            // Combo
            parent.spawn((
                TextBundle::from_section(
                    "Combo: 0",
                    TextStyle {
                        font: font.clone(),
                        font_size: 30.0,
                        color: Color::WHITE,
                    },
                )
                .with_style(Style {
                    position_type: PositionType::Absolute,
                    top: Val::Px(90.0),
                    left: Val::Px(10.0),
                    ..default()
                }),
                ComboText,
            ));
        });
}

fn update_gameplay_ui(
    game_data: Res<GameData>,
    mut score_query: Query<&mut Text, (With<ScoreText>, Without<AccuracyText>, Without<ComboText>)>,
    mut accuracy_query: Query<&mut Text, (With<AccuracyText>, Without<ScoreText>, Without<ComboText>)>,
    mut combo_query: Query<&mut Text, (With<ComboText>, Without<ScoreText>, Without<AccuracyText>)>,
) {
    if let Ok(mut score_text) = score_query.get_single_mut() {
        score_text.sections[0].value = format!("Score: {}", game_data.score);
    }

    if let Ok(mut accuracy_text) = accuracy_query.get_single_mut() {
        accuracy_text.sections[0].value = format!("Accuracy: {:.2}%", game_data.accuracy);
    }

    if let Ok(mut combo_text) = combo_query.get_single_mut() {
        combo_text.sections[0].value = format!("Combo: {}", game_data.combo);
    }
}

fn cleanup_gameplay_ui(mut commands: Commands, query: Query<Entity, With<GameplayUi>>) {
    for entity in query.iter() {
        commands.entity(entity).despawn_recursive();
    }
}