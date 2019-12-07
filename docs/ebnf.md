# EBNF

This is more for me to make sure I'm not being an idiot with any syntax then it is for anyone else.

For that reason some of the basic sentinel values (i.e. literals) won't be formally defined...

A few technical notes;

- CVM files are utf8
- `//` is comments in the EBNF (and comments in the CVM source files)
- I also prefer to inline some definitions because they make the EBNF cleaner and easier to implement

```ebnf
// can use \u{} to place utf8 sequences
id ::= (A-z | '_') (A-z | '_' | 0-9)*
string ::= double quoted sequence of utf8 codepoints
char ::= single quoted utf8 codepoint
int ::= a series of digits with a sign followed by an optional prefix (numPrefix)
flt ::= a decimal number (also supports exponential form) followed by optional prefix (numPrefix)

// f8 and f16 aren't required to be supported
// 32 is default if no size is defined
numPrefix ::= ('u' | 'i' | 'f') ['8' | '16' | '32' | '64' | 'size']

program ::= topLevel*

topLevel ::= struct | function

struct ::= 'struct' id ['<' idList '>'] ['is' '(' idList ')'] '{' declList '}'

function ::= 'fn' ['<' idList '>'] id '(' declList ')' [ '->' type ] (block | '=>' conditional)

// Note: atleast one of type or conditional has to exist
decl ::= id ':' type? '=' conditional
       | id ':' type

// recursively is easier to handle type induction I think??
type ::= type '[' conditional ']'
       | '*' type
       | '(' type ')'
       | id [ '<' type? { ',' type } ','? '>' ]
       | 'fn' id? '(' id [':' type] { ',' id [':' type] } ','? ')'

// recursive types are ew
// so we avoid them by putting pointers
// before and putting arrays after
// the only case of recursion occurs in generics
// which is honestly fine (we can't really avoid it)
type ::= {'*'} id ('[' conditional ']') ['<' type '>']

new ::= 'new' type initialisation?

init_single ::= ['.' id '='] conditional
initialisation ::= '{' [ init_single (',' init_single)* ','? ] '}'

unary ::= { '!' | '*' | '&' | '+' | '-' | '(' type ')' } atom

atom ::= '(' conditional ')'
       | id [ '<' type? { ',' type } ','? '>' ]
       | new
       | func_call
       | index
       | sizeof // maybe alignof??
       | int | flt | string | char | 'null' | 'true' | 'false'
       | '---' // uninitialised signifier
       | 'let' assignment
       | function

sizeof ::= 'sizeof' [ '<' type? '>' ] '(' expr? ')'

func_call ::= atom '(' expr_list ')'

// in reality we'll fold multiple indexes
// into the same expression (same with calls)
// makes analysis and codegen easier
index ::= atom '[' expr ']'

if ::= 'if' conditional block ('else if' expr block)* ['else' block]

block ::= '{' {expr ';'} '}'

while ::= 'while' conditional block

// add foreach
for ::= 'for' expr? ';' conditional? ';' expr? block

defer ::= 'defer' block

conditional ::= logical_or
              | logical_or '?' conditional : conditonal

logical_or ::= logical_and
             | logical_or '||' logical_and 

logical_and ::= equality
              | logical_and '&&' equality

equality ::= relational
           | equality ('==' | '!=') relational

relational ::= bitwise_or
             | relational ('<' | '<=' | '>' | '>=') bitwise_or

bitwise_or ::= bitwise_xor
             | bitwise_or '&' bitwise_xor

bitwise_xor ::= bitwise_and
             | bitwise_xor '^' bitwise_and

bitwise_and ::= shift
             | bitwise_and '&' shift

shift ::= additive
        | shift ('>>' | '<<') additive

additive ::= multiplicative
           | additive ('+' | '-') multiplicative

multiplicative ::= unary
                 | multiplicative ('*' | '/' | '%') unary

assignment ::= (unary [':' type?] ('*' | '/' | '%' | '+' | '-' | '<<' | '>>' | '&' | '^' | '|') '=')+ conditional

expr ::= [ 'return' ] conditional
       | assignment

// allows optional trailing comma
idList ::= id {',' id} ','?
declList ::= decl {',' decl} ','?
```


