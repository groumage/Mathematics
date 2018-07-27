%{
	open Function
%}

%token <float> FLOAT
%token <string> VAR
%token COS SIN SQRT EXP LN
%token PLUS MINUS TIMES DIV
%token LPAR RPAR
%token EOL
%left PLUS MINUS TIMES DIV
%left COS SIN SQRT EXP LN
%nonassoc UMINUS
%type <Function.formel> main
%start main
%%

main:
	expr EOL					{ $1 }
;

expr:
	|	FLOAT					{ flt $1 }
	|	VAR						{ var $1 }
	| 	LPAR expr RPAR			{ $2 }
	| 	expr PLUS expr			{ add $1 $3 }
	| 	PLUS expr				{ pos $2 }
	|	MINUS expr				{ neg $2 }
	| 	expr MINUS expr			{ sub $1 $3 }
	| 	expr TIMES expr			{ mul $1 $3 }
	| 	expr DIV expr			{ div $1 $3 }
	| 	COS expr				{ cos $2 }
	|	SIN expr				{ sin $2 }
	|	SQRT expr				{ sqrt $2 }
	|	LN expr					{ lnp $2 }
	| 	EXP expr				{ exp $2 }
;