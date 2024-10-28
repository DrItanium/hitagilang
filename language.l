%{
%}
%option yylineno
%option noyywrap
%option nounput

%%
[ \t\n] ; 
"--".* ; // comments
%%
