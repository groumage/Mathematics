%{
	open Function
%}

%token <float> FLOAT
%token <string> VAR
%token COS SIN SQRT EXP LN PUIS
%token PLUS MINUS TIMES DIV
%token LPAR RPAR
%token EOL
%left LPAR RPAR
%left COS SIN SQRT EXP LN
%left PLUS MINUS
%left TIMES DIV
%left PUIS
%type <Function.formel> main
%start main
%%

main:
	expr EOL					{ $1 }
;

expr:
	|	FLOAT						{ flt $1 }
	|	VAR							{ var $1 }
	| 	FLOAT VAR 					{ mul (flt $1) (var $2)}
	| 	LPAR expr RPAR				{ $2 }
	| 	expr TIMES expr				{ mul $1 $3 }
	| 	expr DIV expr				{ div $1 $3 }
	| 	expr PLUS expr				{ add $1 $3 }
	| 	expr MINUS expr				{ sub $1 $3 }
	| 	expr PUIS expr				{ puis $1 $3 }
	| 	COS LPAR expr RPAR			{ cos $3 }
	| 	PLUS expr					{ pos $2 }
	|	MINUS expr					{ neg $2 }
	| 	FLOAT COS LPAR expr RPAR	{ mul (flt $1) (cos $4) }
	|	SIN LPAR expr RPAR			{ sin $3 }
	| 	FLOAT SIN LPAR expr RPAR	{ mul (flt $1) (sin $4) }
	|	SQRT LPAR expr RPAR			{ sqrt $3 }
	|	LN LPAR expr RPAR			{ lnp $3 }
	| 	EXP LPAR expr RPAR			{ expo $3 }
;