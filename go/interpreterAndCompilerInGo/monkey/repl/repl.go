package repl

import (
	"bufio"
	"fmt"
	"io"
	"him/monkey/lexer"
	"him/monkey/token"
)

const PROMPT = ">>"

func Start(in io.Reader, out io.Writer){ // specifies reading input from the stdin in in io.Reader and writing output to the stdout for out io.Writer
	scanner := bufio.NewScanner(in)
	for {
		fmt.Fprintf(out, PROMPT) // prints out the constant PROMPT earlier defined
		scanned := scanner.Scan() // scans the current line
		if !scanned { // no input received
			return // return an empty function call
		} else { // there is input received, now we must read it
			line := scanner.Text() // read the line
			l := lexer.New(line) // initialize a new lexer that takes in the line as an input string 
			for tok := l.NextToken(); tok.Type != token.EOF; tok = l.NextToken(){ // unqiue for loop that is a derivative of the generic "for i:=0;i<10;i++" style for loop
				fmt.Fprintf(out, "%+v\n", tok)
			}
		}
	}
}