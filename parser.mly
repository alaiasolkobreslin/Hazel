%token <int> INT
%token TIMES PLUS MINUS DIVIDE MOD

%token <string> ID
%token <string> STRING


%token TRUE FALSE
%token AND OR EQ NEQ LT GT NOT

%token LEFT_BRACE RIGHT_BRACE LEFT_BRACK RIGHT_BRACK LEFT_PAREN RIGHT_PAREN BEGIN END

%token COLON
%token COMMA
%token EOF
%token SEMICOLON
%token CONS
%token OPEN

%token LET REC MUTUAL_REC IN
%token FUN ARROW CONSTRAINT

%token REFERENCE
%token DEREFERENCE
%token ASSIGNREF

%token IF THEN ELSE
%token MATCH WITH WHEN

%token <string> CONSTRUCTOR
%token TYPE OF
%token VERTBAR
