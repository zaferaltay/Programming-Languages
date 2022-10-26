%{
   #include<stdio.h>

   int ocCcFlag=0;

%}


DIGIT "0"|[1-9][0-9]*
IDENTIFIER [a-zA-Z_][a-zA-Z0-9_]*
COMMENT [;][;].*


%% 
"and"      { fprintf(yyout,"%s \n","KW_AND");} 
"or"       { fprintf(yyout,"%s \n","KW_OR" ); }
"not"      { fprintf(yyout,"%s \n","KW_NOT" ); }
"equal"    { fprintf(yyout,"%s \n","KW_EQUAL" ); }
"less"     { fprintf(yyout,"%s \n","KW_LESS" ); }
"nil"      { fprintf(yyout,"%s \n","KW_NIL" ); }
"list"     { fprintf(yyout,"%s \n","KW_LIST" ); }
"append"   { fprintf(yyout,"%s \n","KW_APPEND" ); }
"concat"   { fprintf(yyout,"%s \n","KW_CONCAT" ); }
"set"      { fprintf(yyout,"%s \n","KW_SET" ); }
"deffun"   { fprintf(yyout,"%s \n","KW_DEFFUN" ); }
"for"      { fprintf(yyout,"%s \n","KW_FOR" ); }
"if"       { fprintf(yyout,"%s \n","KW_IF" ); }
"exit"     { fprintf(yyout,"%s \n","KW_EXIT" ); }
"load"     { fprintf(yyout,"%s \n","KW_LOAD" ); }
"disp"     { fprintf(yyout,"%s \n","KW_DISP" ); }
"true"     { fprintf(yyout,"%s \n","KW_TRUE" ); }
"false"    { fprintf(yyout,"%s \n","KW_FALSE" ); }
[ ][\t]*   { } 
[\n]       {}
[\n][\n]   { return 0;}
[+]        { fprintf(yyout,"%s \n","OP_PLUS" ); }
[-]	       { fprintf(yyout,"%s \n","OP_MINUS" ); }
[/]        { fprintf(yyout,"%s \n","OP_DIV" ); }
[*]        { fprintf(yyout,"%s \n","OP_MULT" ); }
[(]        { fprintf(yyout,"%s \n","OP_OP" ); }
[*][*]     { fprintf(yyout,"%s \n","OP_DBLMULT" ); }
[)]        { fprintf(yyout,"%s \n","OP_CP" ); }
"\""   {
 				if(ocCcFlag==0){
 						fprintf(yyout,"%s \n","OP_OC" );
 						ocCcFlag=1;
 				}
 				else{
 						fprintf(yyout,"%s \n","OP_CC" );
 						ocCcFlag=0;
 				}
 	       

 	       }
[,]        { fprintf(yyout,"%s \n","OP_COMMA" ); }

{DIGIT}      {fprintf(yyout,"%s \n","VALUE" );}
{IDENTIFIER} {fprintf(yyout,"%s \n","IDENTIFIER" );}
{COMMENT}    {fprintf(yyout,"%s \n","COMMENT" );}

.  {fprintf(yyout,"%s \n","Syntax error cannot be tokenized" );}

%%



int yywrap(){}

int main(int argc, char const *argv[]){

  	FILE *fpp=fopen("parsed_cpp.txt","a");
  	yyout=fpp;

 	if (argc == 1)
 	{
 		yylex();
 	}
    else{
    	FILE *fp=fopen(argv[1],"r");
    	yyin=fp;
    	yylex();
    }   
   

  }





 

 





