//TO DO
    // -- enemy AI
    // -- add notification screen for enemy AI to have character model drawn to screen
    // -- add cutscenes (https://crates.io/crates/tplay)
    // -- if above not feasible, preload ASCII art
    // -- enemy difficulty scale ([E]z, [M]edium, [A]dvanced)

use std::io;
use std::process;
use std::process::Command;
use rand::Rng;
use colored::*;

// make use of this enum eventually
#[derive(Debug)]
enum Difficulty {
    Easy,
    Medium,
    Advanced
}

#[derive(Debug)]
// player input is cross [X]
// AI input is circle [O]
enum BoxState {
    Empty,
    Cross,
    Circle
}

#[derive(Debug)]
struct Grid {
    box1:BoxState,
    box2:BoxState,
    box3:BoxState,
    box4:BoxState,
    box5:BoxState,
    box6:BoxState,
    box7:BoxState,
    box8:BoxState,
    box9:BoxState
}

#[derive(Debug)]
struct GridWinDrawCheck {
    box1:i8,
    box2:i8,
    box3:i8,
    box4:i8,
    box5:i8,
    box6:i8,
    box7:i8,
    box8:i8,
    box9:i8
}

fn menu() -> Difficulty {
    println!("{} {}", "Welcome to".yellow(), "Tic-Tac-Toe".cyan());
    println!("\n{}", "Choose your difficulty:".yellow());
    println!("\n[E]asy\n[M]edium\n[A]dvanced\n");
    let mut difficulty:String = String::new();
    io::stdin().read_line(&mut difficulty).expect("Failed to read line.");
    let difficulty_neat:&str = difficulty.trim_end();
    let difficulty_rating:Difficulty = Difficulty::Easy;
    match difficulty_neat {
        "e" => {
            Command::new("clear").status().expect("Failed to read command");
            let difficulty_rating = Difficulty::Easy;
            println!("{} {}\n", "You are playing on".yellow(), "Easy difficulty.".green().underline());
            return difficulty_rating
        },
        "m" => {
            Command::new("clear").status().expect("Failed to read command");
            let difficulty_rating = Difficulty::Medium;
            println!("{} {}\n", "You are playing on".yellow(), "Medium difficulty.".blue().underline());
            return difficulty_rating
        },
        "a" => {
            Command::new("clear").status().expect("Failed to read command");
            let difficulty_rating = Difficulty::Advanced;
            println!("{} {}\n", "You are playing on".yellow(), "Advanced difficulty.".bright_red().underline());
            return difficulty_rating

        },
        _ => {
            Command::new("clear").status().expect("Failed to read command");
            println!("{}\n{} {}\n", "Invalid input detected.".red(), "Defaulting to".yellow(), "Easy difficulty.".green().underline());
            return difficulty_rating
        }
    }
}

// 0 is empty, 1 is cross, 2 is circle
fn check_win_draw(playgrid:&mut Grid) -> bool {
    let mut check_win_draw_struct:GridWinDrawCheck = GridWinDrawCheck { box1: 0, box2: 0, box3: 0, box4: 0, box5: 0, box6: 0, box7: 0, box8: 0, box9: 0 };
    match playgrid.box1 {
        BoxState::Empty => {
            check_win_draw_struct.box1 = 0;
        }
        BoxState::Cross => {
            check_win_draw_struct.box1 = 1;
        }
        BoxState::Circle => {
            check_win_draw_struct.box1 = 2;
        }
    }
    match playgrid.box2 {
        BoxState::Empty => {
            check_win_draw_struct.box2 = 0;
        }
        BoxState::Cross => {
            check_win_draw_struct.box2 = 1;
        }
        BoxState::Circle => {
            check_win_draw_struct.box2 = 2;
        }
    }
    match playgrid.box3 {
        BoxState::Empty => {
            check_win_draw_struct.box3 = 0;
        }
        BoxState::Cross => {
            check_win_draw_struct.box3 = 1;
        }
        BoxState::Circle => {
            check_win_draw_struct.box3 = 2;
        }
    }
    match playgrid.box4 {
        BoxState::Empty => {
            check_win_draw_struct.box4 = 0;
        }
        BoxState::Cross => {
            check_win_draw_struct.box4 = 1;
        }
        BoxState::Circle => {
            check_win_draw_struct.box4 = 2;
        }
    }
    match playgrid.box5 {
        BoxState::Empty => {
            check_win_draw_struct.box5 = 0;
        }
        BoxState::Cross => {
            check_win_draw_struct.box5 = 1;
        }
        BoxState::Circle => {
            check_win_draw_struct.box5 = 2;
        }
    }
    match playgrid.box6 {
        BoxState::Empty => {
            check_win_draw_struct.box6 = 0;
        }
        BoxState::Cross => {
            check_win_draw_struct.box6 = 1;
        }
        BoxState::Circle => {
            check_win_draw_struct.box6 = 2;
        }
    }
    match playgrid.box7 {
        BoxState::Empty => {
            check_win_draw_struct.box7 = 0;
        }
        BoxState::Cross => {
            check_win_draw_struct.box7 = 1;
        }
        BoxState::Circle => {
            check_win_draw_struct.box7 = 2;
        }
    }
    match playgrid.box8 {
        BoxState::Empty => {
            check_win_draw_struct.box8 = 0;
        }
        BoxState::Cross => {
            check_win_draw_struct.box8 = 1;
        }
        BoxState::Circle => {
            check_win_draw_struct.box8 = 2;
        }
    }
    match playgrid.box9 {
        BoxState::Empty => {
            check_win_draw_struct.box9 = 0;
        }
        BoxState::Cross => {
            check_win_draw_struct.box9 = 1;
        }
        BoxState::Circle => {
            check_win_draw_struct.box9 = 2;
        }
    }

    // actual win draw check
    if check_win_draw_struct.box1 == 1 && check_win_draw_struct.box2 == 1 && check_win_draw_struct.box3 == 1 || check_win_draw_struct.box1 == 1 && check_win_draw_struct.box4 == 1 && check_win_draw_struct.box7 == 1 || check_win_draw_struct.box7 == 1 && check_win_draw_struct.box8 == 1 && check_win_draw_struct.box9 == 1 || check_win_draw_struct.box3 == 1 && check_win_draw_struct.box6 == 1 && check_win_draw_struct.box9 == 1 || check_win_draw_struct.box1 == 1 && check_win_draw_struct.box5 == 1 && check_win_draw_struct.box9 == 1 || check_win_draw_struct.box2 == 1 && check_win_draw_struct.box5 == 1 && check_win_draw_struct.box8 == 1 || check_win_draw_struct.box4 == 1 && check_win_draw_struct.box5 == 1 && check_win_draw_struct.box6 == 1 {
        println!("\n{}\n", "Player won!\nEnemy lost!".green());
        true
    } else if check_win_draw_struct.box1 == 2 && check_win_draw_struct.box2 == 2 && check_win_draw_struct.box3 == 2 || check_win_draw_struct.box1 == 2 && check_win_draw_struct.box4 == 2 && check_win_draw_struct.box7 == 2 || check_win_draw_struct.box7 == 2 && check_win_draw_struct.box8 == 2 && check_win_draw_struct.box9 == 2 || check_win_draw_struct.box3 == 2 && check_win_draw_struct.box6 == 2 && check_win_draw_struct.box9 == 2 || check_win_draw_struct.box1 == 2 && check_win_draw_struct.box5 == 2 && check_win_draw_struct.box9 == 2 || check_win_draw_struct.box2 == 2 && check_win_draw_struct.box5 == 2 && check_win_draw_struct.box8 == 2 || check_win_draw_struct.box4 == 2 && check_win_draw_struct.box5 == 2 && check_win_draw_struct.box6 == 2 {
        println!("\n{}\n", "Enemy won!\nPlayer lost!".red());
        true
    } else if check_win_draw_struct.box1 == 0 || check_win_draw_struct.box2 == 0 || check_win_draw_struct.box3 == 0 || check_win_draw_struct.box4 == 0 || check_win_draw_struct.box5 == 0 || check_win_draw_struct.box6 == 0 || check_win_draw_struct.box7 == 0 || check_win_draw_struct.box8 == 0 || check_win_draw_struct.box9 == 0 {
        // println!("\n{}\n", "Game not over yet!".yellow());
        false
    } else {
        println!("\n{}\n", "Draw!\nNoone won!".blue());
        true
    }
} 

fn player_turn(playgrid:&mut Grid) -> &mut Grid {
    loop {
        let mut player_input:String = String::new();
        io::stdin().read_line(&mut player_input).expect("Failed to read line");
        let player_input_num:i8 = player_input.trim_end().parse().expect("Failed to parse number");

        match player_input_num {
            1 => {
                match playgrid.box1 {
                    BoxState::Empty => {
                        playgrid.box1 = BoxState::Cross;
                        break;
                    },
                    BoxState::Cross => {
                        println!("{}", "Square already taken! Choose an empty square!".red());
                        continue;
                    },
                    BoxState::Circle => {
                        println!("{}", "Square already taken! Choose an empty square!".red());
                        continue;
                    },
                }
            },
            2 => {
                match playgrid.box2 {
                    BoxState::Empty => {
                        playgrid.box2 = BoxState::Cross;
                        break;
                    },
                    BoxState::Cross => {
                        println!("{}", "Square already taken! Choose an empty square!".red());
                        continue;
                    },
                    BoxState::Circle => {
                        println!("{}", "Square already taken! Choose an empty square!".red());
                        continue;
                    },
                }
            },
            3 => {
                match playgrid.box3 {
                    BoxState::Empty => {
                        playgrid.box3 = BoxState::Cross;
                        break;
                    },
                    BoxState::Cross => {
                        println!("{}", "Square already taken! Choose an empty square!".red());
                        continue;
                    },
                    BoxState::Circle => {
                        println!("{}", "Square already taken! Choose an empty square!".red());
                        continue;
                    },
                }
            },
            4 => {
                match playgrid.box4 {
                    BoxState::Empty => {
                        playgrid.box4 = BoxState::Cross;
                        break;
                    },
                    BoxState::Cross => {
                        println!("{}", "Square already taken! Choose an empty square!".red());
                        continue;
                    },
                    BoxState::Circle => {
                        println!("{}", "Square already taken! Choose an empty square!".red());
                        continue;
                    },
                }
            },
            5 => {
                match playgrid.box5 {
                    BoxState::Empty => {
                        playgrid.box5 = BoxState::Cross;
                        break;
                    },
                    BoxState::Cross => {
                        println!("{}", "Square already taken! Choose an empty square!".red());
                        continue;
                    },
                    BoxState::Circle => {
                        println!("{}", "Square already taken! Choose an empty square!".red());
                        continue;
                    },
                }
            },
            6 => {
                match playgrid.box6 {
                    BoxState::Empty => {
                        playgrid.box6 = BoxState::Cross;
                        break;
                    },
                    BoxState::Cross => {
                        println!("{}", "Square already taken! Choose an empty square!".red());
                        continue;
                    },
                    BoxState::Circle => {
                        println!("{}", "Square already taken! Choose an empty square!".red());
                        continue;
                    },
                }
            },
            7 => {
                match playgrid.box7 {
                    BoxState::Empty => {
                        playgrid.box7 = BoxState::Cross;
                        break;
                    },
                    BoxState::Cross => {
                        println!("{}", "Square already taken! Choose an empty square!".red());
                        continue;
                    },
                    BoxState::Circle => {
                        println!("{}", "Square already taken! Choose an empty square!".red());
                        continue;
                    },
                }
            },
            8 => {
                match playgrid.box8 {
                    BoxState::Empty => {
                        playgrid.box8 = BoxState::Cross;
                        break;
                    },
                    BoxState::Cross => {
                        println!("{}", "Square already taken! Choose an empty square!".red());
                        continue;
                    },
                    BoxState::Circle => {
                        println!("{}", "Square already taken! Choose an empty square!".red());
                        continue;
                    },
                }
            },
            9 => {
                match playgrid.box9 {
                    BoxState::Empty => {
                        playgrid.box9 = BoxState::Cross;
                        break;
                    },
                    BoxState::Cross => {
                        println!("{}", "Square already taken! Choose an empty square!".red());
                        continue;
                    },
                    BoxState::Circle => {
                        println!("{}", "Square already taken! Choose an empty square!".red());
                        continue;
                    },
                }
            }
            _ => {
                //error handling
                println!("{}", "Invalid input!".red());
                println!("{}", "Number 1-9:".yellow());
                continue
            }
        }
    }
    playgrid
}

fn beginner_enemy_turn(playgrid:&mut Grid) -> &mut Grid {
    loop {
        let beginner_enemy_move:i8 = rand::thread_rng().gen_range(1..10);
        match beginner_enemy_move {
            1 => {
                match playgrid.box1 {
                    BoxState::Empty => {
                        playgrid.box1 = BoxState::Circle;
                        break;
                    },
                    BoxState::Cross => {
                        continue;
                    },
                    BoxState::Circle => {
                        continue;
                    },
                }
            },
            2 => {
                match playgrid.box2 {
                    BoxState::Empty => {
                        playgrid.box2 = BoxState::Circle;
                        break;
                    },
                    BoxState::Cross => {
                        continue;
                    },
                    BoxState::Circle => {
                        continue;
                    },
                }
            },
            3 => {
                match playgrid.box3 {
                    BoxState::Empty => {
                        playgrid.box3 = BoxState::Circle;
                        break;
                    },
                    BoxState::Cross => {
                        continue;
                    },
                    BoxState::Circle => {
                        continue;
                    },
                }
            },
            4 => {
                match playgrid.box4 {
                    BoxState::Empty => {
                        playgrid.box4 = BoxState::Circle;
                        break;
                    },
                    BoxState::Cross => {
                        continue;
                    },
                    BoxState::Circle => {
                        continue;
                    },
                }
            },
            5 => {
                match playgrid.box5 {
                    BoxState::Empty => {
                        playgrid.box5 = BoxState::Circle;
                        break;
                    },
                    BoxState::Cross => {
                        continue;
                    },
                    BoxState::Circle => {
                        continue;
                    },
                }
            },
            6 => {
                match playgrid.box6 {
                    BoxState::Empty => {
                        playgrid.box6 = BoxState::Circle;
                        break;
                    },
                    BoxState::Cross => {
                        continue;
                    },
                    BoxState::Circle => {
                        continue;
                    },
                }
            },
            7 => {
                match playgrid.box7 {
                    BoxState::Empty => {
                        playgrid.box7 = BoxState::Circle;
                        break;
                    },
                    BoxState::Cross => {
                        continue;
                    },
                    BoxState::Circle => {
                        continue;
                    },
                }
            },
            8 => {
                match playgrid.box8 {
                    BoxState::Empty => {
                        playgrid.box8 = BoxState::Circle;
                        break;
                    },
                    BoxState::Cross => {
                        continue;
                    },
                    BoxState::Circle => {
                        continue;
                    },
                }
            },
            9 => {
                match playgrid.box9 {
                    BoxState::Empty => {
                        playgrid.box9 = BoxState::Circle;
                        break;
                    },
                    BoxState::Cross => {
                        continue;
                    },
                    BoxState::Circle => {
                        continue;
                    },
                }
            },
            _ => (),
        }
    }
    playgrid
}

fn display_grid(playgrid:&Grid) -> String {
    let mut final_print:String = String::from(" [");
    match playgrid.box1 {
        BoxState::Empty => {
            final_print.push_str(" ");
        },
        BoxState::Cross => {
            final_print.push_str("X");
        }
        BoxState::Circle => {
            final_print.push_str("O");
        }
    }
    final_print.push_str( "] | [");
    match playgrid.box2 {
        BoxState::Empty => {
            final_print.push_str(" ");
        },
        BoxState::Cross => {
            final_print.push_str("X");
        }
        BoxState::Circle => {
            final_print.push_str("O");
        }
    }
    final_print.push_str( "] | [");
    match playgrid.box3 {
        BoxState::Empty => {
            final_print.push_str(" ");
        },
        BoxState::Cross => {
            final_print.push_str("X");
        }
        BoxState::Circle => {
            final_print.push_str("O");
        }
    }
    final_print.push_str( "] \n [");
    match playgrid.box4 {
        BoxState::Empty => {
            final_print.push_str(" ");
        },
        BoxState::Cross => {
            final_print.push_str("X");
        }
        BoxState::Circle => {
            final_print.push_str("O");
        }
    }
    final_print.push_str( "] | [");
    match playgrid.box5 {
        BoxState::Empty => {
            final_print.push_str(" ");
        },
        BoxState::Cross => {
            final_print.push_str("X");
        }
        BoxState::Circle => {
            final_print.push_str("O");
        }
    }
    final_print.push_str( "] | [");
    match playgrid.box6 {
        BoxState::Empty => {
            final_print.push_str(" ");
        },
        BoxState::Cross => {
            final_print.push_str("X");
        }
        BoxState::Circle => {
            final_print.push_str("O");
        }
    }
    final_print.push_str( "] \n [");
    match playgrid.box7 {
        BoxState::Empty => {
            final_print.push_str(" ");
        },
        BoxState::Cross => {
            final_print.push_str("X");
        }
        BoxState::Circle => {
            final_print.push_str("O");
        }
    }
    final_print.push_str("] | [");
    match playgrid.box8 {
        BoxState::Empty => {
            final_print.push_str(" ");
        },
        BoxState::Cross => {
            final_print.push_str("X");
        }
        BoxState::Circle => {
            final_print.push_str("O");
        }
    }
    final_print.push_str("] | [");
    match playgrid.box9 {
        BoxState::Empty => {
            final_print.push_str(" ");
        },
        BoxState::Cross => {
            final_print.push_str("X");
        }
        BoxState::Circle => {
            final_print.push_str("O");
        }
    }
    final_print.push_str("]");
    final_print
}

fn main() {
    Command::new("clear").status().expect("Failed to run argument");
    let difficulty_level:Difficulty = menu();
    match difficulty_level {
        Difficulty::Easy => {
            let mut playgrid:Grid = Grid{box1:BoxState::Empty, box2:BoxState::Empty, box3:BoxState::Empty, box4:BoxState::Empty, box5:BoxState::Empty, box6:BoxState::Empty, box7:BoxState::Empty, box8:BoxState::Empty, box9:BoxState::Empty};
            println!("{}", "Let's play some Tic Tac Toe".yellow());
            println!("\n [{}] | [{}] | [{}] \n [{}] | [{}] | [{}] \n [{}] | [{}] | [{}] \n", "1".yellow(), "2".yellow(), "3".yellow(), "4".yellow(), "5".yellow(), "6".yellow(), "7".yellow(), "8".yellow(), "9".yellow());
            println!("{}","Number 1-9:".yellow());
            loop {
                player_turn(&mut playgrid);
                Command::new("clear").status().expect("Failed to run argument");
                println!("{}", "Player turn".green());
                println!("\n{}\n", display_grid(&playgrid));
                let result_check_win_draw:bool = check_win_draw(&mut playgrid);
                if result_check_win_draw {
                    break
                } else {
                }
                beginner_enemy_turn(&mut playgrid);
                println!("{}", "Enemy turn".red());
                println!("\n{}\n", display_grid(&playgrid));
                let result_check_win_draw:bool = check_win_draw(&mut playgrid);
                if result_check_win_draw {
                    break
                } else {
                }
                println!("\n [{}] | [{}] | [{}] \n [{}] | [{}] | [{}] \n [{}] | [{}] | [{}] \n", "1".yellow(), "2".yellow(), "3".yellow(), "4".yellow(), "5".yellow(), "6".yellow(), "7".yellow(), "8".yellow(), "9".yellow());
                println!("{}", "Choose a square!".yellow());
                println!("{}","Number 1-9:".yellow());
            }
        }
        Difficulty::Medium => {
            println!("Implement later");
        }
        Difficulty::Advanced => {
            println!("Implement later");
        }
    }
}
