package main

import (
	"fmt"
	"os"
	"os/user"
	"him/monkey/repl"
)

func main(){
	user, err := user.Current() // retrieves the current user from the os/user package 
	if err != nil { // there is an error
		panic(err) // panic() => indicates an unrecoverabl error, with optional arguments allowing further specification of error type
	} else { // there is no error
		fmt.Printf("Hello %s! This is the monkey programming language!\n", user.Username)
		fmt.Printf("Feel free to type in commands\n")
		repl.Start(os.Stdin, os.Stdout) // proving the stdin and stdout from the os package as arguements fro io.Reader and io.Writer
	}
}