%{
   #include<stdio.h>
   #include "y.tab.h"
   int ocCcFlag=0;
%}


DIGIT "0"|[1-9][0-9]*
IDENTIFIER [a-zA-Z_][a-zA-Z0-9_]*
COMMENT [;][;].*


%% 
"and"      { return KW_AND;} 
"or"       { return KW_OR; }
"not"      { return KW_NOT; }
"equal"    { return KW_EQUAL; }
"less"     { return KW_LESS; }
"nil"      { return KW_NIL; }
"list"     { return KW_LIST; }
"append"   { return KW_APPEND; }
"concat"   { return KW_CONCAT; }
"set"      { return KW_SET; }
"deffun"   { return KW_DEFFUN; }
"for"      { return KW_FOR; }
"if"       { return KW_IF; }
"exit"     { return KW_EXIT; }
"load"     { return KW_LOAD; }
"disp"     { return KW_DISP; }
"true"     { return KW_TRUE; }
"false"    { return KW_FALSE; }
[ ][\t]*   { } 
[\n]       {printf("%s\n",yytext); return 0;}
[\n][\n]   { return 0;}
[+]        { return OP_PLUS; }
[-]	     { return OP_MINUS; }
[/]        { return OP_DIV; }
[*]        { return OP_MULT; }
[(]      { return OP_OP; }
[*][*]     { return OP_DBLMULT ; }
[)]        { return OP_CP ; }


{DIGIT}      {return VALUE ;}
{IDENTIFIER} {return IDENTIFIER ;}
{COMMENT}    {return COMMENT ;}

.  {fprintf(yyout,"%s \n","Syntax error cannot be tokenized" );}

%%



int yywrap() 
{ 
return 1; 
} 