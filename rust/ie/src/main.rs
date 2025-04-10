mod game;
mod ui;
mod states;

use bevy::prelude::*;
use states::{AppState, MenuState, GameState, ResultState};

fn main() {
    App::new()
        .add_plugins(DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                title: "Rust osu! Clone".into(),
                resolution: (800.0, 600.0).into(),
                ..default()
            }),
            ..default()
        }))
        .add_state::<AppState>()
        .add_systems(Startup, setup)
        .add_systems(Update, (
            bevy::window::close_on_esc,
        ))
        .add_plugins((
            states::StatePlugin,
            game::GamePlugin,
            ui::UiPlugin,
        ))
        .run();
}

fn setup(mut commands: Commands) {
    commands.spawn(Camera2dBundle::default());
}