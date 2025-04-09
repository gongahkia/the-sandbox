package main

import "core:os"
import "core:math"
import "core:time"
import "core:fmt"
import "core:input"

const (
    width  = 20
    height = 10
)

type Direction int

const (
    Up Direction = iota
    Down
    Left
    Right
)

type Point struct {
    x int
    y int
}

type Snake struct {
    body     []Point
    direction Direction
}

type Game struct {
    snake Snake
    food  Point
}

func (g *Game) Init() {
    g.snake.body = []Point{{x: width / 2, y: height / 2}}
    g.snake.direction = Right
    g.GenerateFood()
}

func (g *Game) GenerateFood() {
    g.food = Point{math.random_int(0, width-1), math.random_int(0, height-1)}
}

func (g *Game) Move() {
    head := g.snake.body[0]
    switch g.snake.direction {
        case Up: head.y--
        case Down: head.y++
        case Left: head.x--
        case Right: head.x++
    }
    
    if head == g.food {
        g.snake.body = append([]Point{head}, g.snake.body...)
        g.GenerateFood()
    } else {
        g.snake.body = append([]Point{head}, g.snake.body[:len(g.snake.body)-1]...)
    }
}

func (g *Game) ChangeDirection(newDir Direction) {
    if (g.snake.direction == Up && newDir != Down) ||
       (g.snake.direction == Down && newDir != Up) ||
       (g.snake.direction == Left && newDir != Right) ||
       (g.snake.direction == Right && newDir != Left) {
        g.snake.direction = newDir
    }
}

func (g *Game) IsGameOver() bool {
    head := g.snake.body[0]
    return head.x < 0 || head.x >= width || head.y < 0 || head.y >= height || g.CollidesWithSelf()
}

func (g *Game) CollidesWithSelf() bool {
    head := g.snake.body[0]
    for i := 1; i < len(g.snake.body); i++ {
        if head == g.snake.body[i] {
            return true
        }
    }
    return false
}

func (g *Game) Draw() {
    os.clear()
    for y := 0; y < height; y++ {
        for x := 0; x < width; x++ {
            point := Point{x, y}
            if point == g.food {
                fmt.print("F")
            } else if point == g.snake.body[0] {
                fmt.print("O")
            } else if g.Contains(point, g.snake.body[1:]) {
                fmt.print("X")
            } else {
                fmt.print(".")
            }
        }
        fmt.println()
    }
}

func (g *Game) Contains(p Point, points []Point) bool {
    for _, pt := range points {
        if pt == p {
            return true
        }
    }
    return false
}

func main() {
    game := Game{}
    game.Init()
    
    go func() {
        for !game.IsGameOver() {
            time.sleep(100 * time.Millisecond)
            game.Move()
            game.Draw()
        }
        fmt.println("Game Over!")
        os.exit(0)
    }()
    
    for !game.IsGameOver() {
        input.poll_events()
        if input.is_key_pressed(input.Key_Up) { game.ChangeDirection(Up) }
        if input.is_key_pressed(input.Key_Down) { game.ChangeDirection(Down) }
        if input.is_key_pressed(input.Key_Left) { game.ChangeDirection(Left) }
        if input.is_key_pressed(input.Key_Right) { game.ChangeDirection(Right) }
        
        time.sleep(50 * time.Millisecond)
    }
}