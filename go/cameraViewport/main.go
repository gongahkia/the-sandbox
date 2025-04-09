package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
)

type gridCell struct {
	x     int
	y     int
	state string // empty, wall, player
	model string
}

func readTxt(fileName string) ([]gridCell, error) {
	var fin []gridCell
	file, err := os.Open(fileName)
	if err != nil { // error hit
		return nil, err
	}
	defer file.Close()

	reader := bufio.NewReader(file)
	xCount := 0
	yCount := 0
	for {
		char, _, err := reader.ReadRune()
		if err == io.EOF { // end of file
			break
		} else if err != nil { // error hit
			return nil, err
		}
		var tem gridCell
		isNewLine := char == '\n'
		if isNewLine {
			yCount++
			xCount = 0 // Reset xCount for new line
			continue
		}
		tem.x = xCount
		tem.y = yCount
		switch char {
		case 'X': // wall
			tem.state = "Wall"
			tem.model = "X"
		case '.':
			tem.state = "Empty"
			tem.model = "."
		case '@':
			tem.state = "Player"
			tem.model = "@"
		default:
			continue // Skip unknown characters
		}
		fin = append(fin, tem)
		xCount++
	}
	return fin, nil
}

func renderGraphics(fin []gridCell) string {
	var finString string
	maxX, maxY := findMaxCoordinates(fin)

	for y := 0; y <= maxY; y++ {
		for x := 0; x <= maxX; x++ {
			found := false
			for _, cell := range fin {
				if cell.x == x && cell.y == y {
					finString += cell.model
					found = true
					break
				}
			}
			if !found {
				finString += " "
			}
		}
		finString += "\n"
	}

	return finString
}

func findMaxCoordinates(fin []gridCell) (int, int) {
	maxX, maxY := 0, 0
	for _, cell := range fin {
		if cell.x > maxX {
			maxX = cell.x
		}
		if cell.y > maxY {
			maxY = cell.y
		}
	}
	return maxX, maxY
}

func playerInput(fin []gridCell) {
	for i := range fin {
		if fin[i].state == "Player" {
			fmt.Print("Enter movement (wasd): ")
			inputReader := bufio.NewReader(os.Stdin)
			char, _, err := inputReader.ReadRune()
			if err != nil {
				fmt.Println("Error reading input:", err)
				return
			}
			switch char {
			case 'w':
				fin[i].y--
			case 'a':
				fin[i].x--
			case 's':
				fin[i].y++
			case 'd':
				fin[i].x++
			default:
				fmt.Println("Invalid input. Please use wasd.")
			}
			return // Exit after processing player's input
		}
	}
}

func main() {
	fmt.Println("cameraViewport main function test")
	finMap, err := readTxt("begin.txt")
	if err != nil { // error hit
		fmt.Println("Error reading file:", err)
		return
	}

	for {
		fmt.Println(renderGraphics(finMap))
		playerInput(finMap)
	}
}
