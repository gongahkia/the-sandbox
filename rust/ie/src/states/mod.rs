mod menu_state;
mod game_state;
mod result_state;

pub use menu_state::MenuState;
pub use game_state::GameState;
pub use result_state::ResultState;

use bevy::prelude::*;

#[derive(States, Debug, Clone, Copy, Eq, PartialEq, Hash, Default)]
pub enum AppState {
    #[default]
    MainMenu,
    Playing,
    Results,
}

pub struct StatePlugin;

impl Plugin for StatePlugin {
    fn build(&self, app: &mut App) {
        app
            .add_plugins(menu_state::MenuPlugin)
            .add_plugins(game_state::GamePlugin)
            .add_plugins(result_state::ResultPlugin);
    }
}