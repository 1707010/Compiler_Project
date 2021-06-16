%{
    #include<stdio.h>
    #include<stdlib.h>
    #include<math.h>
    #include<string.h>
    #include<stdarg.h>
    int data[60];
    int yylex();
    extern FILE *yyin,*yyout;
    int i,f=0;

    int switchdone = 0;
    int switchvar;
    
    typedef struct variable {
            char *str;
                int n;
            }array;
    
    array store[1000];
    void yyerror(char *s);
    void vari (array *p, char *s, int n);
    void set_val (char *s, int n);
    int check(char *key);
    int count = 1;
%}




%union 
{
     int number;
     char *string;
}

/* bison declarations */

%token <string> VAR
%type <number> expression statement 
%token <number> NUM

%token NUM VAR IF ELSE MAIN INT FLOAT CHAR START END SWITCH CASE DEFAULT BREAK FOR iFOR PF SIN COS TAN LOG LOG10 PLUS MINUS MULTI DIV FACTIORIAL POW ODDEVEN PRIME FIBBO NEWL
%nonassoc IFX
%nonassoc ELSE
%nonassoc SWITCH
%nonassoc CASE
%nonassoc DEFAULT
%left '<' '>'
%left '+' '-'
%left '*' '/'

/* Grammar rules and actions follow.  */

%%



program: MAIN ':' START cstatement END
     ;

cstatement: /* NULL */

    | cstatement statement
    ;

statement: ';'       {}     
    | declaration ';'       //{ printf("Declaration\n"); }

    | expression ';'        {}  //{   printf("value of expression: %d\n", $1); $$=$1;}
    
    | VAR '=' expression ';' { 
                                if(check($1))
                                {
                                    set_val ($1,$3);
                                    printf("Value of the Variable (%s)= %d\t\n",$1,$3);
                                }
                                else
                                {
                                    printf("(%s) Variable Not DEclared\n",$1); 
                                }
                            }
   
    | FOR '(' NUM '<' NUM ',' NUM ')' START statement END {
                                    int i;
                                    for(i=$3 ; i<$5 ; i=i+$7) {printf("value of the loop: %d expression value: %d\n", i,$10);}                                  
                               }

    | iFOR '(' NUM '>' NUM ',' NUM ')' START statement END {
                                    int i;
                                    for(i=$3 ; i>$5 ; i=i-$7) {printf("value of the iloop: %d expression value: %d\n", i,$10);}                                  
                               }

    | SWITCH '(' B1  ')' START B2  END  {}

    | IF '(' expression ')' START expression ';' END %prec IFX {
                                if($3){
                                    printf("value of expression in IF: %d\n",$6);
                                }
                                else{
                                    printf("condition value zero in IF block\n");
                                }
                            }

    | IF '(' expression ')' START expression ';' END ELSE START expression ';' END {
                                if($3){
                                    printf("value of expression in IF: %d\n",$6);
                                }
                                else{
                                    printf("value of expression in ELSE: %d\n",$11);
                                }
                            }
    | PF '(' expression ')' ';' {printf("Print Expression %d\n",$3);}
    ;
    
B1 : expression   {
    
                    switchdone = 0;
                    switchvar = $1;

                }
    

B2  :/*empty*/
    |B2 CASE expression ':' expression ';' BREAK ';'
            {
            if ($3 == switchvar){
            printf("\n CASE %d executed. Value of expression %d \n",$3,$5);
            switchdone=1;
            }
    

            }
     |B2 DEFAULT  ':' expression ';' BREAK ';'
            {
            if (switchdone == 0){
            printf("\n Default CASE  executed. Value of expression %d \n",$4);
            }

            }


    
declaration : TYPE ID1 
             ;


TYPE : INT   
     | FLOAT  
     | CHAR   
     ;



ID1 : ID1 ',' VAR '=' NUM    {
                if(check($3))
                {
                    printf("\n(%s) Variable  DEclared Before \n",$3);
                }
                else
                { 
                    set_val ($3,$5);
                    printf("\nValue of the Variable (%s)= %d\n",$3,$5);
                }
        }
    | ID1 ',' VAR   {               if(check($3))
                        {
                            printf("\nERROR:Multiple Declaration Of (%s) \n", $3 );
                        }
                        else
                        {
                            printf("(%s) Variable Declared\n",$3);
                            vari(&store[count],$3, count);
                            count++;
                        }
            }
    | VAR '=' NUM    {
                if(check($1))
                {
                    printf("\n(%s) Variable  DEclared Before \n",$1);
                }
                else
                { 
                    set_val ($1,$3);
                    printf("\nValue of the Variable (%s)= %d\n",$1,$3);
                }
        }
    | VAR   {               if(check($1))
                        {
                            printf("\nERROR:Multiple Declaration Of (%s) \n", $1 );
                        }
                        else
                        {
                            printf("(%s) Variable Declared\n",$1);
                            vari(&store[count],$1, count);
                            count++;
                        }
            }  
    ;

expression: NUM                 { $$ = $1;  }

    | VAR   {   int i = 1;
                char *name = store[i].str;
                while (name) 
                {
                    if (strcmp(name, $1) == 0)
                    {
                        $$ = (int)store[i].n;
                        //printf("%s -> %d\n", $1, (int)store[i].n ) ;
                        break;
                    }
                        name = store[++i].str;
                }
            }
    
    | expression PLUS expression    { $$ = $1 + $3; }

    | expression MINUS expression   { $$ = $1 - $3; }

    | expression MULTI expression   { $$ = $1 * $3; }

    | expression DIV expression { if($3){
                                        $$ = $1 / $3;
                                    }
                                    else{
                                        $$ = 0;
                                        printf("\ndivision by zero\t");
                                    }   
                                }
    | expression '%' expression { if($3){
                                        $$ = $1 % $3;
                                    }
                                    else{
                                        $$ = 0;
                                        printf("\nMOD by zero\t");
                                    }   
                                }
    | expression POW expression { $$ = pow($1 , $3);}
    | expression '<' expression { $$ = $1 < $3; }
    
    | expression '>' expression { $$ = $1 > $3; }

    | '(' expression ')'        { $$ = $2;  }
    | SIN expression            {printf("Value of Sin(%d) is %lf\n",$2,sin($2*3.1416/180)); $$=sin($2*3.1416/180);}

    | COS expression            {printf("Value of Cos(%d) is %lf\n",$2,cos($2*3.1416/180)); $$=cos($2*3.1416/180);}

    | TAN expression            {printf("Value of Tan(%d) is %lf\n",$2,tan($2*3.1416/180)); $$=tan($2*3.1416/180);}

    | LOG10 expression          {printf("Value of Log10(%d) is %lf\n",$2,(log($2*1.0)/log(10.0))); $$=(log($2*1.0)/log(10.0));}
    | LOG expression            {printf("Value of Log(%d) is %lf\n",$2,(log($2))); $$=(log($2));}

    | NEWL '(' ')' {printf("\n");}

    | FACTIORIAL expression
              {
                 int i,fact = 1;
                 for(i=1;i<=$2;i++)
                 {
                    fact=fact*i;
                 }
                 printf("The factorial of %d  is : %d\n",$2,fact);
                 
                 
              }

    | ODDEVEN expression
              {
                 int i = $2;
                 if(i%2==0)
                 {
                   printf("%d Even Number \n",$2);
                 }
                 else
                 {
                   printf("%d Odd Number \n",$2);
                 }
                

              }

     | PRIME expression
     {
     int n, i, flag = 0;
     n=$2;


  for (i = 2; i <= n / 2; ++i) {
    // condition for non-prime
    if (n % i == 0) {
      flag = 1;
      break;
    }
  }

  if (n == 1) {
    printf("1 is neither prime nor composite.");
  } 
  else {
    if (flag == 0)
      printf("%d is a prime number.", n);
    else
      printf("%d is not a prime number.", n);
  }
  printf("\n");
  }

  | FIBBO expression
  {int n=$2, first = 0, second = 1, next, c;


  printf("First %d terms of Fibonacci series are:", n);

  for (c = 0; c < n; c++)
  {
    if (c <= 1)
      next = c;
    else
    {
      next = first + second;
      first = second;
      second = next;
    }
    printf("%d ", next);
  }

  }

    
%%


void vari(array *p, char *s, int n)
                {
                  p->str = s;
                  p->n = n;
                }
void set_val(char *s, int num)
            {
                    int i = 1;
                    char *name = store[i].str;
                    while (name) {
                        if (strcmp(name, s) == 0){
                    store[i].n=num;
                        break;
                            }
                    name = store[++i].str;
                }
            }

int check(char *key)
            {
                
                int i = 1;
                char *name = store[i].str;
                while (name) {
                        if (strcmp(name, key) == 0){
                        return i;
                    }
                        name = store[++i].str;
                }
                return 0;
            }


yyerror(char *s){
    printf( "%s\n", s);
}

