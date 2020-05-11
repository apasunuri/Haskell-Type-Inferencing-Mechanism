:- dynamic gvar/2.
:- dynamic lvar/2.

typeExp(X, int) :-
    X = int;
    integer(X);
    lvar(X, int);
    gvar(X, int).

typeExp(X, float) :-
    X = float;
    float(X);
    lvar(X, float);
    gvar(X, float).

typeExp(X, bool) :-
    X = bool;
    typeBoolExp(X);
    lvar(X, float);
    gvar(X, bool).

typeExp(X, string) :-
    X = string;
    atom(X);
    lvar(X, string);
    gvar(X, string).

typeExp(Fct, T) :-
    \+ var(Fct), 
    \+ atom(Fct),
    functor(Fct, Fname, _Nargs),
    !,
    Fct =.. [Fname|Args],
    append(Args, [T], FType),
    functionType(Fname, TArgs),
    typeExpList(FType, TArgs).

typeExp(T, T).

typeExpList([], []).
typeExpList([Hin|Tin], [Hout|Tout]) :-
    typeExp(Hin, Hout),
    typeExpList(Tin, Tout).

hasComparison(int).
hasComparison(float).
hasComparison(string).

typeBoolExp(true).
typeBoolExp(false). 
typeBoolExp(X < Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).
typeBoolExp(X =< Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).
typeBoolExp(X > Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).
typeBoolExp(X >= Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).
typeBoolExp(X == Y) :- 
    typeExp(X, T),
    typeExp(Y, T).
typeBoolExp(X \== Y) :- 
    typeExp(X, T),
    typeExp(Y, T).

typeStatement(X, T) :-
    typeExp(X, T),
    deleteLVars().

typeStatement([S], T) :- typeStatement(S, T), deleteLVars().
typeStatement([S, S2|Statements], T) :-
    typeStatement(S, _T),
    typeStatement([S2|Statements], T),
    deleteLVars().

typeStatement(gvLet(Name, T, Code), unit) :-
    atom(Name),
    typeExp(Code, T),
    bType(T),
    asserta(gvar(Name, T)).

typeStatement(funcLet(Name, T, Expr), unit) :-
    atom(Name),
    is_list(T),
    bType(T),
    typeStatement(Expr, X),
    last(T, X),
    asserta(gvar(Name, T)).

typeStatement(if(Cond, TrueB, FalseB), T) :-
    typeBoolExp(Cond),
    typeStatement(TrueB, T),
    typeStatement(FalseB, T),
    deleteLVars().

typeStatement(lvLet(Name, T, Code, []), unit) :-
    atom(Name),
    typeExp(Code, T),
    bType(T).
typeStatement(lvLet(Name, T, Code, [H|_Tail]), S) :-
    atom(Name),
    typeExp(Code, T),
    bType(T),
    asserta(lvar(Name, T)),
    typeStatement(H, S).

typeStatement(for(StartIndex, EndIndex, Code), T) :-
    integer(StartIndex),
    integer(EndIndex),
    StartIndex =< EndIndex,
    typeStatement(Code, T),
    deleteLVars().

typeCode([S], T) :- typeStatement(S, T).
typeCode([S, S2|Code], T) :-
    typeStatement(S, _T),
    typeCode([S2|Code], T).

infer(Code, T) :-
    is_list(Code),
    deleteGVars(),
    typeCode(Code, T).

bType(int).
bType(float).
bType(string).
bType(bool).
bType(unit).

bType([H]) :- bType(H).
bType([H|T]) :- bType(H), bType(T).

hasAdd(int).
hasAdd(float).
hasAdd(string).

numeric(int).
numeric(float).

deleteLVars() :- retractall(lvar(_,_)), asserta(lvar(_X,_Y) :- false()).
deleteGVars() :- retractall(gvar), asserta(gvar(_X,_Y) :- false()).

fType((+), [T, T, T]) :- hasAdd(T).
fType((-), [T, T, T]) :- numeric(T).
fType((*), [T, T, T]) :- numeric(T).
fType((/), [T, T, T]) :- numeric(T).
fType(iplus, [int, int, int]).
fType(iminus, [int, int, int]).
fType(imultiply, [int, int, int]).
fType(idivision, [int, int, int]).
fType(fplus, [float, float, float]).
fType(fminus, [float, float, float]).
fType(fmultiply, [float, float, float]).
fType(fdivision, [float, float, float]).
fType(sqrt, [T, float]) :- numeric(T).
fType(exp, [T, float]) :- numeric(T).
fType(ln, [T, float]) :- numeric(T).
fType(sin, [T, float]) :- numeric(T).
fType(cos, [T, float]) :- numeric(T).
fType(fToInt, [float, int]).
fType(iToFloat, [int, float]).
fType(and, [bool, bool, bool]).
fType(or, [bool, bool, bool]).
fType(not, [bool, bool]).
fType(print, [_X, unit]).

functionType(Name, Args) :-
    gvar(Name, Args),
    is_list(Args), !.

functionType(Name, Args) :-
    fType(Name, Args).

lvar(_, _) :- false().
gvar(_, _) :- false().