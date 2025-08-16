// referencing 
    // https://medium.com/@osho_jay/building-lightweight-cross-platform-applications-entirely-in-go-no-js-no-bs-4b737e3f5067

// note
  // requires Go 1.19 or more recent versions

package main

import (
    // go defaults
    "image/color"
    "log"
    "os"

    // gioui defaults
    "gioui.org/app"
    "gioui.org/font/gofont"
    "gioui.org/io/system"
    "gioui.org/layout"
    "gioui.org/op"
    "gioui.org/text"
    "gioui.org/widget/material"
)

var theme  = material.NewTheme(gofont.Collection()) 

// --- main execution code --- 

func main() {
  go func() { // anonymous function
    w := app.NewWindow() 
    err := run(w)
    if err != nil { // error emitted 
      log.Fatal(err)
    }
    os.Exit(0)
  }()
  app.Main()
}

func run(w *app.Window) error {
    var ops op.Ops
    for {
        e := <-w.Events()
        switch e := e.(type) { // constant event listener
            case system.DestroyEvent: // error hit causing destruction of goGUI event
                return e.Err
            case system.FrameEvent: // no error hit, instantiate frame
                gtx := layout.NewContext(&ops, e)
                title := material.H1(theme, "Hi, I'm Giggles")
                maroon := color.NRGBA{R: 127, G: 0, B: 0, A: 255}
                title.Color = maroon
                title.Alignment = text.Middle
                title.Layout(gtx)
                e.Frame(gtx.Ops)
        }
    }
}