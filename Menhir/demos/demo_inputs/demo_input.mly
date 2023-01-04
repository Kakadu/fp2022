%token PLUS
%token MINUS
%token MULTY
%token DIV
%token LBRACE
%token RBRACE
%token INT
%token EOL
%start main
%%
main:
| expr; EOL
| EOL
expr:
| expr; PLUS; expr
| expr; MINUS; expr
| expr; MULTY; expr
| expr; DIV; expr
| LBRACE; expr; RBRACE
| INT
