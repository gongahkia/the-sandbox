package parser

import (
	"testing"
	"him/monkey/ast"
	"him/monkey/lexer"
)

// --- testing helper functions ---
	// be well aware that we are gonna be writing test functions for basically every possible ast node that we specify in ast

// let statements

func TestLetStatements(t *testing.T){ // test function to check all the let statements provided as test input

	input := `
		let x = 5;
		let y = 10;
		let foorbar = 838383;
	` // test input consisting of 3 statements

	l := lexer.New(input) // initialize a new lexer instance
	p := New(l) // initialize a new parser instance
	program := p.ParseProgram() // creates a program root node
	checkParserErrors(t, p) // checks whether the parser has any errors after the program has been parsed into a slice of Statement interfaces
	if program == nil { // program does not exist
		t.Fatalf("ParseProgram() returned nil") // return a fatal error and halt function execution
	} else { // program exists and no error hit
		if len(program.Statements) != 3 { //  we are just hardcoding the number of statements as expected right now, which is currently 3
			t.Fatalf("program.Statements does not contain 3 statements. got=%d", len(program.Statements))
		} else { // number of statements correct
			tests := []struct { // initialise the test struct with expectedIdentifiers
				expectedIdentifier string
			}{
					{"x"},
					{"y"},
					{"foobar"},
			} 
			for i, tt := range tests { // actual validation code
				stmt := program.Statements[i]
				if !testLetStatement(t, stmt, tt.expectedIdentifier) {
					return // return nothing since an error has been hit
				}
			}
		}
	}

}

func testLetStatement(t *testing.T, s ast.Statement, name string) bool { // helper function to check one individual let statement, called within TestLetStatements

	if s.TokenLiteralValue() != "let" {// checks the validity of the input if its a let statement
		t.Errorf("s.TokenLiteralValue not 'let'. got=%q", s.TokenLiteralValue())
		return false
	} else {}

	letStmt, ok := s.(*ast.LetStatement) // performs type assertion by trying to convert value of s to type *ast.LetStatement, result of conversion is stored in letStmt and booleanValue indicating success of the operation is stored in ok to check if the fields of s match those of LetStatement as defined in the ast package
	if !ok { // if failure to convert type succesfully, then fields don't match completely
		t.Errorf("s not *ast.LetStatement. got=%q", s)
		return false
	} else {} 

	if letStmt.Name.Value != name { // checks the validity of the input if its an identifier
		t.Errorf("letStmt.Name.Value not '%s', got=%s", name, letStmt.Name.Value)
		return false
	} else {}

	return true

}

// return statements 

func TestReturnStatements(t *testing.T) {
	input := `
		return 5;
		return 10;
		return 993322;
	`
	l := lexer.New(input) // new lexer initialised
	p := New(l) // new parser initialised
	program := p.ParseProgram()
	checkParserErrors(t, p)
	if len(program.Statements) != 3 { // here we are hardcoding the number of arguments provided to the program
			t.Fatalf("program.Statements does not contain 3 statements. got=%d",len(program.Statements))
	} else { // correct number of program statements
		for _, statement := range program.Statements{
			returnStatement, ok := statement.(*ast.ReturnStatement) // performing type assertion 
			if !ok { // wrong type
				t.Errorf("statement not *ast.ReturnStatement, got=%T", statement)
				continue
			} else if returnStatement.TokenLiteralValue() != "return" { // wrong literal value
				t.Errorf("returnStatement.TokenLiteralValue not 'return', got %q", returnStatement.TokenLiteralValue())
			} else {
				// do nothing because no error is thrown
			} 
		}
	}
}

// --- generic helper functions ---

func checkParserErrors(t *testing.T, p *Parser){ // simple helper function that checks for errors and if present, then exits the program after displaying them
	errors := p.Errors() // getter method returns errors to the local variable here
	if len(errors) == 0 { // no errors
		return 
	} else { // at least 1 error
		t.Errorf("parser has %d errors", len(errors)) // emits an error using go's test framework
		for _, msg := range errors { // enumerate over and print all the errors
			t.Errorf("parser error hit: %q", msg)
		}
		t.FailNow() // exits the program and causes tests to fail under go's test framework
	} 
}