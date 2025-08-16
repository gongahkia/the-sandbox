package main

import (
	"fmt"
	"log"
	"ekans/lib/general"
	"github.com/hajimehoshi/ebiten/v2"
)

type Game struct { // stores game specific data
	coord general.Coord // a coord has the datatype coord
}

func (g *Game) Update() error { // called each frame to update game logic
	return nil
}

// Draw is called each frame to draw on the screen
func (g *Game) Draw(screen *ebiten.Image) { // called each frame to draw the screen

}

// FUA what does this do 
func (g *Game) Layout(outsideWidth, outsideHeight int) (int, int) { // layout defines logical screen size based on window size, so can vary size as needed
	return 840, 840
}

func main() {
	fmt.Println("testing ebitengine...")
	ebiten.SetWindowSize(840, 840) // create a 60 x 60 grid where each cell is 14 pixels wide
	ebiten.SetWindowTitle("ekans")
	game := &Game{}
	err := ebiten.RunGame(game)
	if err != nil { // if error hit
		fmt.Println("error(s) hit...")
		log.Fatal(err) // log error
	} else {
		fmt.Println("no error(s) hit...")
	} // do nothing
}