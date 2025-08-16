package lexer

// --- imports ---

import (
    "him/monkey/token"
)

// --- type definition ---

type Lexer struct {
    input string // input string that takes in monkey source code
    position int // index that points to current position in input (pointer that points to current char)
    readPosition int // index of current reading position in input (pointer position + 1)
    ch byte // current char being read at position of index Lexer.position
}

// --- constructor function --- 

// effectively a constructor function for the lexer struct
func New(input string) *Lexer { // returns a pointer to a newly created Lexer struct
    l := &Lexer{
        input: input,
    }
    l.readChar() // reads the first character to assign position, readPosition and current ch
    return l
}

// effectively a constructor function for the token struct
func newToken(tokenType token.TokenType, ch byte) token.Token { 
    return token.Token{
        Type: tokenType, // type string
        LiteralValue: string(ch), // changes the byte to a string value as specified in Token struct under him/monkey/token
    }
}

// --- helper function --- 

func (l *Lexer) readChar(){ // pointer receiver function that READS the current char into the lexer struct
    if l.readPosition >= len(l.input){ // end of input, index error would otherwise be hit
        l.ch = 0
    } else {
        l.ch = l.input[l.readPosition]
    }
    l.position = l.readPosition // increment position by advancing it to readPosition
    l.readPosition++ // increment l.Position
}

func (l *Lexer) peekChar() byte { // does not increment the current position of our lexer since we only really want to peek at the next char but not increment the position of the current index
    if l.readPosition >= len(l.input){
        return 0 // end of input, index error would otherwise be hit here
    } else {
        return l.input[l.readPosition] // peeks at the char in the position to be read after the char at the current position
    }
}

func (l *Lexer) NextToken() token.Token { // pointer receiver function that validates the current char within the lexer struct
    var tok token.Token // calling the imported token module here to declare a variable that is to be returned
    l.skipWhiteSpace() // skips any whitespace between tokens
    switch l.ch { // validating the next character based upon existing tokens that are one character wide, and creates a new token struct if the character deemed valid
        case '=': // need to account for the EQ token also
            if l.peekChar() == '=' { // checks the next character but doesn't read it 
                ch := l.ch // reads the current char which is a "="
                l.readChar() // advances to read the next char which is also a "="
                literal := string(ch) + string(l.ch) // token's literal value
                tok = token.Token{ // creates the new EQ token
                    Type: token.EQ,
                    LiteralValue: literal,
                }
            } else { // the next character is not a "=" so there's no chance we're reading an EQ token, treat this as an ASSIGN token
                tok = newToken(token.ASSIGN, l.ch)
            }
        case '+':
            tok = newToken(token.PLUS, l.ch)
        case '-':
            tok = newToken(token.MINUS, l.ch)
        case '!': // need to account for the NOT_EQ token also
            if l.peekChar() == '=' { // checks the next character but does not read it
                ch := l.ch // reads the current char which is a "!"
                l.readChar() // advances to read the next char which we already established is a "="
                literal := string(ch) + string(l.ch) // token's literal value
                tok = token.Token{ // creates the new NOT_EQ token
                    Type: token.NOT_EQ,
                    LiteralValue: literal,
                }
            } else { // the next character is not a "=" so there's no chance we're reading an NOT_EQ token, treat this as a BANG token
                tok = newToken(token.BANG, l.ch)
            }
        case '/':
            tok = newToken(token.SLASH, l.ch)
        case '*':
            tok = newToken(token.ASTERISK, l.ch)
        case '<':
            tok = newToken(token.LT, l.ch)
        case '>': 
            tok = newToken(token.GT, l.ch)
        case ';':
            tok = newToken(token.SEMICOLON, l.ch)
        case ',':
            tok = newToken(token.COMMA, l.ch)
        case '(':
            tok = newToken(token.LPAREN, l.ch)
        case ')':
            tok = newToken(token.RPAREN, l.ch)
        case '{':
            tok = newToken(token.LBRACE, l.ch)
        case '}':
            tok = newToken(token.RBRACE, l.ch)
        case 0: // 0 because end of line is assigned within the readChar method of the lexer struct
            tok.LiteralValue = ""
            tok.Type = token.EOF
        default: // default case if not any of the above scenarios to handle keywords, identifiers and numbers
            if isLetter(l.ch){ // if character is a letter, then still valid character token
                tok.LiteralValue = l.readIdentifier()
                tok.Type = token.LookUpIdent(tok.LiteralValue) // assigns the corresponding type to the token, either IDENT, LET or FUNCTION
                return tok
            } else if isDigit(l.ch) { // if character is an integer, then still valid integer token
                tok.Type = token.INT
                tok.LiteralValue = l.readNumber()
                return tok
            } else { // illegal token
                tok = newToken(token.ILLEGAL, l.ch)
            }
    }
    l.readChar() // reads the next char into the lexer struct before returning token
    return tok
}

func (l *Lexer) readIdentifier() string { // reads an identifier and advances the lexer's position until it encounters a non-letter character
    position := l.position // incremented position of current character being read
    for isLetter(l.ch){ // for creates a while loop which runs until the isLetter(l.ch) returns a false as a break condition
        l.readChar() // advances the pointer to check the next character of the identifier
    }
    return l.input[position:l.position] // returns the sliced identifier string
}

func isLetter(ch byte) bool { // sneaks in a check that includes _ to allow for camelcased names like this_function and that_function
    return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

func (l *Lexer) skipWhiteSpace() { // advances the current character index pointer if the next character is an empty string until its no longer the case
    for l.ch == ' ' || l.ch == '\t' || l.ch == '\n' || l.ch == '\r' { // for creates a while loop where these are the break conditions
        l.readChar()
    }
}

func (l *Lexer) readNumber() string { // reads an integer as a string
    position := l.position
    for isDigit(l.ch){ // if the given character is a digit, for creates a while loop where the l.ch not being a digit is the break condition
        l.readChar() // advance the pointer to check the next digit
    }
    return l.input[position:l.position] // returns the sliced integer as a string
}

func isDigit(ch byte) bool { // checks whether the current character is a digit, we can include further support for floats and octal and hex digits in future implementations, would that just entail accepting the "." as a valid INT token type
    return '0' <= ch && ch <= '9'
}
