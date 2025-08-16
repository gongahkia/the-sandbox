// simiar to token, ast merely defines the outline of its composite elements, and it is up to parser to determine how the relationships between the various AST nodes are implemented

package ast

import (
	"him/monkey/token" // recall each token merely stores a Token type and a literal value
)

// --- generic type definitions ---
	// all structs that inherit from the below interfaces need to implement methods with the specified type signatures, although actual meat of the implementation is not strictly defined

type Node interface { // interface here specifies only the method signatures of the Node type but does not specify the implementation
	TokenLiteralValue() string // defines a method that returns the literal string value of a token, used for debugging and testing
}

type Statement interface {
	Node // this syntax indicates that interface Statement inherits from the Node interface
	statementNode() // dummy method for now to guide debugging
}

type Expression interface {
	Node // this syntax indicates that interface EXpression inherits from the Node interface
	expressionNode() // dummy method for now to guide debugging
}

// --- program ---

type Program struct { // root node of every AST produced by the parser
	Statements []Statement // has a field that is comprised of a slice of statement interfaces
}

func (p *Program) TokenLiteralValue() string{ // pointer receiver method called by the Program type
	if len(p.Statements) > 0{ // as long as there is one statement in the program
		return p.Statements[0].TokenLiteralValue()
	} else { // no statements in the program
		return ""
	}
}

// --- identifier ---

type Identifier struct {
	Token token.Token // will be assigned the token.IDENT token value
	Value string // actual identifier
}

func (i *Identifier) expressionNode(){
	// dummy method for now
}

func (i *Identifier) TokenLiteralValue() string { // effectively a getter method to return the token literal value of any AST node
	return i.Token.LiteralValue
}

// --- let statement ---

type LetStatement struct { // basic structure of a let statement broken down comprises a LET identifier = value
	Token token.Token // will be assigned the token.LET token value
	Name *Identifier // pointer to an identifier struct which is basically an IDENT token as defined in the token package
	Value Expression
}

func (ls *LetStatement) statementNode(){
	// dummy method for now to fulfill the statement node interface
}

func (ls *LetStatement) TokenLiteralValue() string{ // effectively a getter method to return the token literal value of the LetStatement AST node
	return ls.Token.LiteralValue
}

// --- return statement ---

type ReturnStatement struct { // basic structure of a return statement broken down to RETURN expression
	Token token.Token // will be assigned the token.RETURN token value
	ReturnValue Expression
}

func (rs *ReturnStatement) statementNode(){
	// dummy method for now to fulfill the statement node interface
}

func (rs *ReturnStatement) TokenLiteralValue() string{ // a getter method that returns the token literal value of the ReturnStatement AST node
	return rs.Token.LiteralValue
}