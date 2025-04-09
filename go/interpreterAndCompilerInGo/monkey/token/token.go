package token

// --- type definition ---

type TokenType string // type definition for the type of a given token 

type Token struct { // type definition for a token, which comprises the aforementioned TokenType and the LiteralValue stored within said token
    Type TokenType
    LiteralValue string
}

// --- TokenTypes defined ---

const (
    // special tokens
    ILLEGAL = "ILLEGAL" // tokens we don't know about
    EOF = "EOF" // token for the end of file 

    // identifiers + literals
    IDENT = "IDENT"
    INT = "INT"

    // operators
    ASSIGN = "="
    PLUS = "+"
    MINUS = "-"
    BANG = "!"
    ASTERISK = "*"
    SLASH = "/"
    LT = "<" // less than
    GT = ">" // greater than
    EQ = "==" // equal
    NOT_EQ = "!=" // not equal

    // delimiters
    COMMA = ","
    SEMICOLON = ";"
    LPAREN = "("
    RPAREN = ")"
    LBRACE = "{"
    RBRACE = "}"

    // keywords
    FUNCTION = "FUNCTION"
    LET = "LET"
    TRUE = "TRUE"
    FALSE = "FALSE"
    IF = "IF"
    ELSE = "ELSE"
    RETURN = "RETURN"
)

var keywords = map[string] TokenType {
    "fn": FUNCTION,
    "let": LET,
    "true": TRUE,
    "false": FALSE, 
    "if": IF,
    "else": ELSE,
    "return": RETURN,
}

func LookUpIdent(ident string) TokenType {
    if tok, ok := keywords[ident]; ok { // destructures the map and checks whether the key ident exists in the map, if it does, then returns the assigned tok value
        return tok
    }
    return IDENT // if the tokentype not found in keywords map, then return the TokenType IDENT to indicate the word will likely be treated as a special identifier assigned by the user
}
