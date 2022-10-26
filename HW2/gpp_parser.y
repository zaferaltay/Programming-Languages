%{
#include <stdio.h>
#include <math.h>
extern FILE *yyin;
int faul=0,temp=0,outOfProg=0,numb=0,id1=0,myArray[1000],indx=0,forel=0,pter=0,forif=0,temp2=0,resss=0;

void printer(int myArray[],int x){
    int i=0;
    while(i< x){
        if(i!= x -1)
            printf("%d,",myArray[i]);
        else    
             printf("%d",myArray[i]);    
        i++;
    }

}
%}


%start START
%token OP_PLUS
%token OP_MINUS
%token OP_MULT
%token OP_DIV
%token OP_OP
%token OP_CP
%token OP_DBLQUOTE
%token OP_DBLMULT
%token KW_AND
%token KW_OR
%token KW_NOT
%token KW_EQUAL
%token KW_LESS
%token KW_NIL
%token KW_APPEND
%token KW_CONCAT
%token KW_SET
%token KW_DEFFUN
%token KW_OPLIST
%token KW_FOR
%token KW_WHILE
%token KW_DEFVAR
%token KW_IF
%token KW_EXIT
%token KW_LOAD
%token KW_DISP
%token KW_TRUE
%token KW_FALSE
%token KW_LIST
%token VALUE
%token IDENTIFIER
%token COMMENT





%% 

START: | INPUT;

INPUT: EXPI { 
    if(!faul){
        printf("SYNTAX OK.\n") ;
        if (pter==1)
        {
            if(numb==1){
                 temp=0;forel=0;pter=0;temp2=0;numb=0;forif=0;numb=0; 
                 printf("\nResult:%d\n",$$);
                 
            }
            if (temp2==0)
            {
                if(forif==1  && forel==0 ){ 
                    numb=0;indx=0;temp2=0;forif=0;pter=0;
                    printf ("Result: (" );printer(myArray,indx);printf (")");
                }
                else if(forel==1){
                    int couter=0; 
                    numb=0;indx=0;temp2=0;forif=0;pter=0;forel=0;
                    while(couter<indx-temp){
                        myArray[couter]=myArray[temp+couter];
                        couter++;
                    }
                    indx=indx-temp;temp=0;
                    printf("Result:(");printer(myArray,indx);printf(")");
                    
                }
            } 
            else if (forif==0 && numb==0 ){ 
                numb=0;indx=0;temp2=0;forif=0;pter=0;temp=0;forel=0;
                printf("Result: nil"); 
            }
            else if(temp2==1){ 
                indx=temp;
                numb=0;indx=0;temp2=0;forif=0;pter=0;temp=0;forel=0;
                printf ("Result: (" );printer(myArray,indx);printf (")");
            }     
        }
    }
}        
| EXPB { 
    if (!faul){
        temp=0;temp2=0;numb=0; forif=0;forel=0;
        printf("SYNTAX OK."); 
        if(pter==1){
            printf("\nResult: ");
            if($$ == 1)
                printf ("TRUE");
            else
                printf("NIL");  
            pter=0; 
        }
    }
} 
| EXPLISTI {
    if (!faul)
    {
        if(pter==1 &&forif==1){ 
            temp2=0;temp=0;forel=0;pter=0;forif=0;numb=0;indx=0;
            printf("SYNTAX OK.\n");
            printf ("Result: (" );printer(myArray,indx);printf (")");
        }    
        
    }
}
     

  



//-----------------------------------------------------------------------------------------------------------------
EXPI:OP_OP OP_PLUS EXPI EXPI OP_CP {numb=1; pter=1; $$=$3+$4; }
| OP_OP KW_IF EXPB EXPLISTI OP_CP {
    if($3==1) forif=1; 
    else forif=0;
    pter=1;
     
}

| OP_OP KW_FOR OP_OP IDENTIFIER EXPI EXPI OP_CP EXPLISTI OP_CP 
{ 
    forif=1;    
    pter=1;
}

| OP_OP KW_WHILE  EXPB  EXPLISTI OP_CP  
{ 
    if($3==1) forif=1; 
    else forif=0;
    pter=1;
}

| OP_OP OP_DIV EXPI EXPI OP_CP {numb=1;pter=1;$$=$3/$4;}
| COMMENT 
| OP_OP KW_SET IDENTIFIER EXPI OP_CP {pter=1; $$ = $4;}
| OP_OP OP_MINUS EXPI EXPI OP_CP {numb=1;pter=1;$$=$3-$4;}
| OP_OP KW_DEFFUN IDENTIFIER IDLIST EXPLISTI OP_CP 
| OP_OP OP_MULT EXPI EXPI OP_CP {numb=1; pter=1;$$=$3*$4;}
| IDENTIFIER 
| OP_OP KW_LOAD OP_DBLQUOTE IDENTIFIER OP_DBLQUOTE OP_CP
| OP_OP OP_DBLMULT EXPI EXPI OP_CP 
| OP_OP KW_DISP  OP_DBLQUOTE IDENTIFIER OP_DBLQUOTE  OP_CP
| OP_OP IDENTIFIER EXPLISTI OP_CP { forif=1; pter=1; $$= $3;}
| VALUE  {$$=$1;}
| OP_OP KW_EXIT OP_CP {exit(1);}
| OP_OP KW_DEFVAR IDENTIFIER EXPI OP_CP {pter=1; numb=1; forif=0; $$=$4; }
| OP_OP KW_LIST VALUES OP_CP
;

//------------------------------------------------------------------------------------------------------------------
EXPB: OP_OP KW_AND EXPB EXPB OP_CP  {pter=1;$$=($3 && $4);} 

|OP_OP KW_EQUAL EXPB EXPB OP_CP  {pter=1;$$=($3==$4);} 
|OP_OP KW_NOT EXPB  OP_CP  {pter=1;$$=!$3;} 
|OP_OP KW_EQUAL EXPI EXPI OP_CP  {pter=1; $$=($3==$4);} 
|OP_OP KW_OR EXPB EXPB OP_CP   {pter=1; $$=($3 || $4);} 
|OP_OP KW_LESS EXPI EXPI OP_CP  {pter=1; $$=($3 < $4);} 
| BINARYVAL
;
//------------------------------------------------------------------------------------------------------------------
IdentifierList : IdentifierList IDENTIFIER 
| IDENTIFIER
;
//--------------------------------------------------------------------------------------------------------------------
IDLIST: OP_OP IdentifierList OP_CP;
BINARYVAL: KW_TRUE { $$=1;}
| KW_FALSE   {$$=0; }
;
//---------------------------------------------------------------------------------------------------------
LISTVALUE: KW_OPLIST VALUES OP_CP {  if (!faul && temp==0) temp=indx;}
| KW_OPLIST OP_CP 
;
VALUES: VALUES VALUES { myArray[indx++]=$2; }
| VALUES  { myArray[indx++]=$1 ; }

;
//--------------------------------------------------------------------------------------------------------------------
EXPLISTI: OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP 
| LISTVALUE { forif=1; pter=1;} 
| OP_OP KW_APPEND EXPI EXPLISTI OP_CP { 
    int i=indx-1;
    while(i>=0){
        myArray[i+1]=myArray[i];
        i--;
    }
    myArray[0]=$$; 
    indx=indx+1; 
}
;


%% 
int yyerror(const char * ch) { 
    printf("\nSYNTAX ERROR\n\n");
    faul=1;    
} 
int main(){ 
   
    FILE *fp; 
 
    printf("$ g++\n"); 
    yyin = stdin;
    while (1){
        yyparse();         
    }         
    return 0; 
}
