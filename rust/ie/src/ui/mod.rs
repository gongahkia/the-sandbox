mod main_menu;
mod gameplay;

pub use main_menu::MainMenuPlugin;
pub use gameplay::GameplayUiPlugin;

use bevy::prelude::*;

pub struct UiPlugin;

impl Plugin for UiPlugin {
    fn build(&self, app: &mut App) {
        app
            .add_plugins(MainMenuPlugin)
            .add_plugins(GameplayUiPlugin);
    }
}