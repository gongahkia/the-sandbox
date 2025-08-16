package parser

import (
	"fmt"
	"him/monkey/ast"
	"him/monkey/lexer"
	"him/monkey/token"
)

type Parser struct { // type definition for a Parser struct
	l *lexer.Lexer // pointer to a lexer
	curToken token.Token // 'index' that points to the current token
	peekToken token.Token // 'index' that points to the next token
	errors []string // slice of strings storing errors
}

// --- generic helper function ---

func New(l *lexer.Lexer) *Parser {
	p := &Parser{
		l: l,
		errors: []string{

		}, // empty slice of strings currently
	} // initialize a new parser 
	p.nextToken() // reads this token so curToken value is set
	p.nextToken() // reads this token so peekToken value is set
	return p // returns a pointer to the new parser instance
}

// --- pointer receiver functions for Parser --- 

func (p *Parser) nextToken(){ // parser pointer receiver function that advances the values to the next token using the lexer's own pointer receiver function
	p.curToken = p.peekToken // advance current token
	p.peekToken = p.l.NextToken() // advance next token
}

func (p *Parser) parseStatement() ast.Statement { // parser pointer receiver function that parses each individual statement within the ParseProgram pointer receiver function
	switch p.curToken.Type {
		case token.LET: // runs the parseLetStatement test to check whether the following tokens adhere to the grammatical rules we set out
			return p.parseLetStatement()
		case token.RETURN:
			return p.parseReturnStatement() // runs the parseReturnStatement test to check whether the following tokens adhere to the grammatical rules we set out
		default:
			return nil
	}
}

func (p *Parser) parseLetStatement() *ast.LetStatement { // parser pointer receiver function that checks the validity of a given Let statement, performing syntactic analysis on it

	stmt := &ast.LetStatement{ // initialize a statement instance
		Token: p.curToken,
	}

	// LET statements comprise...
		// 1. identifier IDENT
		// 2. assignment ASSIGN
		// 3. expression 

	// 1. identifier check
	if !p.expectPeek(token.IDENT){ // first i expect an identity token, so in its absence we return nil because an error has effetcively been hit
		return nil
	}

	stmt.Name = &ast.Identifier{
		Token: p.curToken,
		Value: p.curToken.LiteralValue,
	}

	// 2. assignment check
	if !p.expectPeek(token.ASSIGN){ // next i expect to see an assignment token
		return nil
	}

	// 3. expression check (fua to be implemented later)


	// temporary end of let statement loop
	for !p.expectPeek(token.SEMICOLON){ // keep advancing the loop if not the end of the expression and continue reading until we hit a semicolon
		p.nextToken() // advance to read the next curToken and peekToken
	}

	return stmt

}

func (p *Parser) parseReturnStatement() *ast.ReturnStatement {

	stmt := &ast.ReturnStatement{
		Token: p.curToken,
	}

	p.nextToken()

	for !p.curTokenIs(token.SEMICOLON){
		p.nextToken()
	}

	return stmt

}

func (p *Parser) ParseProgram() *ast.Program { // parser pointer receiver function that returns a program 

	program := &ast.Program{ // initialised with no values currently to construct the root node of the AST

	}
	program.Statements = []ast.Statement{ // initialize an empty slice meant to contain the Statement datatype

	} 

	for p.curToken.Type != token.EOF { // while its not currently the EOF for our tokens
		stmt := p.parseStatement() // statement instance is initialised with the required values
		if stmt != nil { // if statement is not empty, then append it to the overall Statements slice
			program.Statements = append(program.Statements, stmt)
		} else {} // if not, do nothing
		p.nextToken() // advance to the next token which advances curToken and peekToken
	}

	return program

}

func (p *Parser) curTokenIs(t token.TokenType) bool {
	return p.curToken.Type == t
}

func (p *Parser) peekTokenIs(t token.TokenType) bool { // small helper function for the below parser pointer receiver function
	return p.peekToken.Type == t
}

func (p *Parser) expectPeek(t token.TokenType) bool { // ASSERTION FUNCTION that asserts the correctness of token order
	if p.peekTokenIs(t){
		p.nextToken() // advance curToken and peekToken once
		return true
	} else {
		p.peekError(t) // an error has been hit, so append an error to the parser errors slice
		return false
	}
}

func (p *Parser) Errors() []string { // getter parser pointer receiver function that returns all the errors
	return p.errors
}

func (p *Parser) peekError(t token.TokenType) { // returns an error when the next peeked token is of the wrong type
	msg := fmt.Sprintf("expected next token to be %s, got %s instead", t, p.peekToken.Type)
	p.errors = append(p.errors, msg) // appends the error to out errors slice
}