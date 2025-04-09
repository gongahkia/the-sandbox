package main

import "core:fmt"
import "core:math"
import "sdl2"

const WindowWidth = 800
const WindowHeight = 600
const PaddleWidth = 100
const PaddleHeight = 20
const BallRadius = 10
const BrickRows = 5
const BrickCols = 10

type Paddle struct {
    x     f32
    y     f32
}

type Ball struct {
    x        f32
    y        f32
    velocity_x f32
    velocity_y f32
}

type Brick struct {
    x      f32
    y      f32
    width  f32
    height f32
    is_destroyed bool
}

var (
    paddle Paddle
    ball   Ball
    bricks  []Brick
    score   int
)

proc init_game() {
    paddle = Paddle{x: WindowWidth / 2 - PaddleWidth / 2, y: WindowHeight - PaddleHeight - 10}
    ball = Ball{x: WindowWidth / 2, y: WindowHeight - PaddleHeight - BallRadius - 10, velocity_x: 5, velocity_y: -5}
    
    for i in 0 .. BrickRows {
        for j in 0 .. BrickCols {
            bricks = append(bricks, Brick{x: f32(j * (WindowWidth / BrickCols)), y: f32(i * 30 + 50), width: WindowWidth / BrickCols, height: 20, is_destroyed: false})
        }
    }
}

proc update_game() {
    ball.x += ball.velocity_x
    ball.y += ball.velocity_y
    
    if (ball.x < BallRadius or ball.x > WindowWidth - BallRadius) {
        ball.velocity_x = -ball.velocity_x
    }
    
    if (ball.y < BallRadius) {
        ball.velocity_y = -ball.velocity_y
    }
    
    if (ball.y + BallRadius >= paddle.y and ball.x >= paddle.x and ball.x <= paddle.x + PaddleWidth) {
        ball.velocity_y = -ball.velocity_y
        ball.y = paddle.y - BallRadius
    }

    for brick in bricks {
        if not brick.is_destroyed and 
           (ball.x + BallRadius >= brick.x and 
            ball.x - BallRadius <= brick.x + brick.width and 
            ball.y + BallRadius >= brick.y and 
            ball.y - BallRadius <= brick.y + brick.height) {
            brick.is_destroyed = true
            score += 10
            ball.velocity_y = -ball.velocity_y
        }
    }
}

proc render_game() {
    sdl2.ClearScreen()
    
    sdl2.DrawRect(paddle.x, paddle.y, PaddleWidth, PaddleHeight, sdl2.Color{0, 0, 255, 255})
    
    sdl2.DrawCircle(i32(ball.x), i32(ball.y), BallRadius, sdl2.Color{255, 0, 0, 255})
    
    for brick in bricks {
        if not brick.is_destroyed {
            sdl2.DrawRect(brick.x, brick.y, brick.width, brick.height, sdl2.Color{0, 255, 0, 255})
        }
    }
}

proc handle_input() {
    var event sdl2.Event

    while sdl2.PollEvent(&event) != 0 {
        if event.type == sdl2.QUIT {
            return false 
        }

        if event.type == sdl2.KEYDOWN {
            switch event.key.keysym.sym {
                case sdl2.K_LEFT:
                    paddle.x -= 20 
                case sdl2.K_RIGHT:
                    paddle.x += 20 
            }
        }
        
        if paddle.x < 0 { paddle.x = 0 }
        if paddle.x > WindowWidth - PaddleWidth { paddle.x = WindowWidth - PaddleWidth }
    }
    
    return true 
}

proc main() {
    sdl2.Init(sdl2.INIT_EVERYTHING)
    
    defer sdl2.Quit()
    
    var window = sdl2.CreateWindow("Brick Breaker", sdl2.WINDOWPOS_UNDEFINED, sdl2.WINDOWPOS_UNDEFINED, WindowWidth, WindowHeight, sdl2.WINDOW_SHOWN)
    
    defer sdl2.DestroyWindow(window)

    init_game()
    
    var running = true
    
    while running {
        running = handle_input()
        
        update_game()
        
        render_game()
        
        sdl2.Delay(16)
        
        fmt.printf("Score: %d\n", score)
        
        if ball.y > WindowHeight { 
            fmt.printf("Game Over! Final Score: %d\n", score)
            break; 
        }
        
        sdl2.Present(window) 
        
        if all_bricks_destroyed() { 
            fmt.printf("You Win! Final Score: %d\n", score)
            break; 
        }
     }
}

proc all_bricks_destroyed() -> bool {
   for brick in bricks {
       if not brick.is_destroyed { return false }
   }
   return true 
}