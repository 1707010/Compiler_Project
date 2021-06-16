%{
	#include<stdio.h>
	#include<string.h>
	#include<stdlib.h>
	#include<math.h>
	#include<stdarg.h>
	#include "main.tab.h"
%}

alpha [a-zA-Z][a-zA-Z0-9]*
digit [0-9]
header					"insert"[ ]*+[^\n]+[.h][ ]*
keyWord					IF|ELSE|FOR|RETURN|INTEGER|FLOAT|CHAR|SWITCH|CASE|BREAK|DEFAULT|PF
singleLineComment		[ ]*[#][#][a-zA-Z0-9@$%^&*(){}_+-,.:\=|?>< ]*
multipleLineComment		[ ]*[!][#][a-zA-Z0-9@$*(){}_+-,.:\|?>=<\n\t ]*[#][!]	

%%

SWITCH 		   { return SWITCH;}
out 		   { return PF;}
CASE   		   { return CASE;} 
IF 			   { return IF; }
FI		       { return ELSE;}
BREAK  		   { return BREAK; }
DEFAULT 	   { return DEFAULT;}
"add"		   {return PLUS;}
"sub"		   {return MINUS;}
"mul"		   {return MULTI;}
"div"		   {return DIV;}
"sup"		   {return POW;}
"((" 		   { return START;}
"))"  		   { return END;}
LOOP 		   { return FOR;}
iLOOP			{return iFOR;}
SIN			   {return SIN;}
FACT			{return FACTIORIAL;}
isODD			{return ODDEVEN;}
isPRIME 		{return PRIME;}
FIBB			{return FIBBO;}
COS            {return COS;}
TAN            {return TAN;}
LOG			   {return LOG;}
LOG10 		   {return LOG10;}
LN 				{return NEWL;}


{singleLineComment}		{printf("single Line Comment \n");}
{multipleLineComment}	{printf("multiple Line Comment \n");}
{header}  				{printf("Valid header\n");}
[-+/*<>=,():;%^]     {	return *yytext;	}
INTEGER  			 {return INT;}
CHAR 		         {return CHAR;}
FRACTION 			 	{return FLOAT;}
"body"	   { return MAIN; }
[ ]*	    			{}
[\n]*					{}
{digit}+	{ yylval.number = atoi(yytext);	 return NUM; }
{alpha}	    { yylval.string = strdup(yytext);	 return VAR; }
[\t]*					{}
.						{printf("%s --> invalid\n",yytext);}


%%
int yywrap()
{
	return 1;
}

int main()
{
	freopen("input.txt","r",stdin);
	freopen("output.txt","w",stdout);
	yyparse();
    
	return 0;
}