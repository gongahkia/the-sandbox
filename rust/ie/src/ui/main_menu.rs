use bevy::prelude::*;
use crate::states::AppState;

pub struct MainMenuPlugin;

impl Plugin for MainMenuPlugin {
    fn build(&self, app: &mut App) {
        app
            .add_systems(OnEnter(AppState::MainMenu), setup_main_menu)
            .add_systems(Update, handle_main_menu_input.run_if(in_state(AppState::MainMenu)))
            .add_systems(OnExit(AppState::MainMenu), cleanup_main_menu);
    }
}

#[derive(Component)]
struct MainMenuUi;

#[derive(Component)]
struct PlayButton;

fn setup_main_menu(mut commands: Commands, asset_server: Res<AssetServer>) {
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
            MainMenuUi,
        ))
        .with_children(|parent| {
            parent.spawn(TextBundle::from_section(
                "Rust osu! Clone",
                TextStyle {
                    font: font.clone(),
                    font_size: 80.0,
                    color: Color::WHITE,
                },
            ));

            parent.spawn((
                ButtonBundle {
                    style: Style {
                        width: Val::Px(200.0),
                        height: Val::Px(65.0),
                        margin: UiRect::all(Val::Px(20.0)),
                        justify_content: JustifyContent::Center,
                        align_items: AlignItems::Center,
                        ..default()
                    },
                    background_color: Color::rgb(0.15, 0.15, 0.15).into(),
                    ..default()
                },
                PlayButton,
            ))
            .with_children(|parent| {
                parent.spawn(TextBundle::from_section(
                    "Play",
                    TextStyle {
                        font,
                        font_size: 40.0,
                        color: Color::WHITE,
                    },
                ));
            });
        });
}

fn handle_main_menu_input(
    mut interaction_query: Query<
        (&Interaction, &mut BackgroundColor),
        (Changed<Interaction>, With<PlayButton>),
    >,
    mut app_state: ResMut<NextState<AppState>>,
) {
    for (interaction, mut color) in &mut interaction_query {
        match *interaction {
            Interaction::Pressed => {
                app_state.set(AppState::Playing);
            }
            Interaction::Hovered => {
                *color = Color::rgb(0.25, 0.25, 0.25).into();
            }
            Interaction::None => {
                *color = Color::rgb(0.15, 0.15, 0.15).into();
            }
        }
    }
}

fn cleanup_main_menu(mut commands: Commands, query: Query<Entity, With<MainMenuUi>>) {
    for entity in query.iter() {
        commands.entity(entity).despawn_recursive();
    }
}