package general

type Coord struct {
	X int 
	Y int
	minX int
	maxX int
	minY int
	maxY int
}

func (c Coord) CheckBounds() bool { // only value receiver struct since just checking field value
	return c.X >= c.minX && c.X <= c.maxX && c.Y >= c.minY && c.Y <= c.maxY
}