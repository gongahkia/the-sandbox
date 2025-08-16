// references
	// gin HTTP web framework package => https://pkg.go.dev/github.com/gin-gonic/gin#Context

// required packages

package main

// required imports

import (
	"net/http"
	"github.com/gin-gonic/gin"
)

type album struct { // type definition for the album struct
	ID string
	Title string
	Artist string
	Price float64
}

var albums = []album{ // slice of album structs
	{
		ID: "1", 
		Title: "Blue Train", 
		Artist: "John Coltrane", 
		Price: 56.99,
	},
	{
		ID: "2", 
		Title: "Jeru", 
		Artist: "Gerry Mulligan", 
		Price: 17.99,
	},
	{
		ID: "3", 
		Title: "Sarah Vaughan and Clifford Brown", 
		Artist: "Sarah Vaughan", 
		Price: 39.99,
	},
}

// --- function definition ---
	// c => context argument

func getAlbums(c *gin.Context){
	c.IndentedJSON(http.StatusOK, albums)
}

func postAlbums(c *gin.Context){
	var newAlbum album
	if err := c.BindJSON(&newAlbum); err != nil { // context binds the request body to newAlbum argument and error is hit
		return
	}
	albums = append(albums, newAlbum) // append the initialised album struct from the JSON to the album slice
	c.IndentedJSON(http.StatusCreated, newAlbum)
}

func getAlbumByID(c *gin.Context) {
    id := c.Param("id")
    for _, a := range albums {
        if a.ID == id {
            c.IndentedJSON(http.StatusOK, a)
            return
        }
    }
    c.IndentedJSON(http.StatusNotFound, gin.H{
		"message": "album not found",
	})
} 

// --- main function execution ---

func main(){
	router := gin.Default() // initialise a Gin router
	router.GET("/albums", getAlbums) // associates the GET HTTP method with the handler function
	router.GET("/albums/:id", getAlbumByID) // associates the GET HTTP method with the handler function
	router.POST("/albums", postAlbums) // associates the POST HTTP method with the handler function
	router.Run("localhost:8080") // attaches the router to a HTTP server && starts the server
}
