// reference
	// https://github.com/gopxl/pixel

// note
	// package "go get github.com/gopxl/pixel/v2/pixelgl" not found 
	// wait till library is updated and current rendering code is updated

package main

import (
	"github.com/gopxl/pixel/v2"
	"github.com/gopxl/pixel/v2/pixelgl"
	"golang.org/x/image/colornames"
)

func run() {
	cfg := pixelgl.WindowConfig{
		Title:  "Pixel Rocks!",
		Bounds: pixel.R(0, 0, 1024, 768),
		VSync:  true,
	}
	win, err := pixelgl.NewWindow(cfg)
	if err != nil {
		panic(err)
	}

	win.Clear(colornames.Skyblue)

	for !win.Closed() {
		win.Update()
	}
}

func main() {
	pixelgl.Run(run)
}