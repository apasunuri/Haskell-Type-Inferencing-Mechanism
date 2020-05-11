:- begin_tests(typeInf).
:- include(typeInf).

test(typeExp_int, [nondet]) :-
    typeExp(3, int).

test(typeExp_float, [nondet]) :-
    typeExp(3.3, float).

test(typeExp_float_F, [fail]) :-
    typeExp(3, float).

test(typeExp_bool, [nondet]) :-
    typeExp(true == false, bool).

test(typeExp_string, [nondet]) :-
    typeExp(prolog, string).

test(typeExp_func, [nondet]) :-
    typeExp(iToFloat(3), T),
    assertion(T == float).

test(typeExp_type, [nondet]) :-
    typeExp(int, int).

test(typeExp_list, [nondet, T == float]) :-
    typeExpList([2, 3.14, 7 == 8, sqrt(5)], [int, float, bool, T]).

test(typeBoolExp, [nondet]) :-
    typeBoolExp(2 > 3),
    typeBoolExp(2 >= 3),
    typeBoolExp(2.7 < 3.88),
    typeBoolExp(2 =< 5),
    typeBoolExp(1.1 == 3.14),
    typeBoolExp(1 \== 3),
    typeBoolExp(true == (1 < 2)),
    typeBoolExp(false \== true).

test(single_statement, [nondet]) :-
    typeStatement(3 + 4, T),
    assertion(T == int).

test(single_statement_in_list, [nondet]) :-
    typeStatement([and(true, false)], T),
    assertion(T == bool).

test(statements_in_list, [nondet]) :-
    typeStatement([and(true, false), sin(4 * 89)], T),
    assertion(T == float).

test(typeStatement_gvar, [nondet, true(T == int)]) :-
    deleteGVars(),
    typeStatement(gvLet(v, T, iplus(X, Y)), unit),
    assertion(X == int), assertion( Y == int),
    gvar(v, int).

test(typeStatement_function, [nondet, true(T == int)]) :-
    deleteGVars(),
    typeStatement(funcLet(subtract, [int, int, T], int - int), unit),
    assertion(T == int).

test(typeStatement_if, [nondet]) :-
    typeStatement(if(true, [3], [4]), T),
    assertion(T == int).

test(typeStatement_for, [nondet]) :-
    typeStatement(for(1, 3, [print(hello)]), T),
    assertion(T == unit).

test(typeCode, [nondet]) :-
    typeCode([for(1, 3, [print(hello)]), gvLet(a, int, 4 * 3)], T),
    assertion(T == unit).

test(bType_list, [nondet]) :-
    bType([int, float, string]).

test(fTypes, [nondet]) :-
    fType((+), [S, S, S]),
    fType((-), [T, T, T]),
    fType((*), [T, T, T]),
    fType((/), [T, T, T]),
    fType(sin, [T, float]),
    fType(cos, [T, float]),
    fType(sqrt, [T, float]),
    fType(exp, [T, float]),
    fType(ln, [T, float]),
    assertion(T == int; T == float),
    assertion(S == int; S == float; S == string).

test(functionType_builtin, [nondet]) :-
    functionType(fplus, [T, T, T]),
    assertion(T == float).

test(functionType_userdefined, [nondet]) :-
    typeStatement(funcLet(runTest, [float, float, bool], float >= float), unit),
    functionType(runTest, [T, T, S]),
    assertion(T == float),
    assertion(S == bool).

test(infer_expression, [nondet]) :-
    infer([int, float, bool], T),
    assertion(T == bool).

test(infer_if, [nondet]) :-
    infer([if(5.5 > 8.9, iplus(int, int), iminus(int, int))], T),
    assertion(T == int).

test(infer_if_F, [fail]) :-
    infer([if(true == 5, iToFloat(int, int), fmultiply(float, float))], T),
    assertion(T == int).

test(infer_for, [nondet]) :-
    infer([for(1, 6, fdivision(float, float))], T),
    assertion(T == float).

test(infer_for_F, [fail]) :-
    infer([for(12, 9, fdivision(float, float))], T),
    assertion(T == float).

test(infer_letIn, [nondet]) :-
    infer([lvLet(x, int, iplus(int, int), [lvLet(y, float, fplus(float, float), [lvLet(z, bool, true, [print(y)])])])], T),
    assertion(T == unit).

test(infer_letInGlobal, [nondet]) :-
    deleteGVars(),
    infer([gvLet(v, V, fminus(float, float)), lvLet(x, X, 8.0 * fmultiply(float, float), [lvLet(y, Y, fdivision(float, float), [lvLet(z, Z, float < float, [v + x + y])])])], T),
    assertion(V == float),
    assertion(X == float),
    assertion(Y == float),
    assertion(Z == bool),
    assertion(T == float).

test(infer_functionDefCall, [nondet]) :-
    infer([funcLet(runTest, [string, int, bool], float =< float), runTest(X, Y)], T),
    assertion(X == string),
    assertion(Y == int),
    assertion(T == bool).

test(infer_variable, [nondet]) :-
    infer([gvLet(o, T, sin(6.7) + cos(5.6))], S),
    assertion(T == float),
    assertion(S == unit).

test(infer_variable_2, [nondet]) :-
    infer([gvLet(o, T, ln(exp(8.22)))], S),
    assertion(T == float),
    assertion(S == unit).

test(infer_1, [nondet]) :-
    infer([gvLet(temp_c, T, 4.0), gvLet(temp_f, T, 1.8 * temp_c + 32.0), if(temp_f < 32.0, print(freezing), print(freezing))], S),
    assertion(T == float),
    assertion(S == unit).

test(infer_2, [fail]) :-
    infer([lvLet(temp_c, float, 8.0, [print(temp_c)]), gvLet(temp_f, float, 1.8 * temp_c + 32.0)], float).

test(infer_3, [nondet]) :-
    infer([lvLet(x, X, 2, [lvLet(y, Y, 3, [lvLet(z, Z, true, [[lvLet(z, Z, x < y, []), print(z)]])])])], T),
    assertion(X == int),
    assertion(Y == int),
    assertion(Z == bool),
    assertion(T == unit).

test(infer_4, [nondet]) :-
    infer([funcLet(pythagorean, [float, float, X], sqrt(float * float + float * float)), pythagorean(Y, Z)], T),
    assertion(X == float),
    assertion(Y == float),
    assertion(Z == float),
    assertion(T == float).

test(infer_5, [nondet]) :-
    infer([for(1, 8, [sin(8), iplus(6, 12), exp(2.2), fToInt(ln(5))])], T),
    assertion(T == int).

test(infer_6, [nondet]) :-
    infer([funcLet(concat, [string, string, T], string + string), concat(X, Y)], S),
    assertion(T == string),
    assertion(X == string),
    assertion(Y == string),
    assertion(S == string).

test(infer_7, [nondet]) :-
    infer([[if(false, print(true), print(false)), for(1, 10000, print(loop_iteration))], 2 * 8], T),
    assertion(T == int).

test(infer_8, [fail]) :-
    infer([for(1, 40, [8 + 9, 4.5 * 2.0])], int).

test(infer_9, [fail]) :-
    infer([lvLet(a, float, 8 * sin(2.2), []), gvLet(b, float, 6.0 + a)], float).

test(infer_10, [nondet]) :-
    infer([gvLet(a, int, 67), lvLet(b, int, fToInt(sin(6.8)), [lvLet(c, int, 6, [])]), a + b + c], string).

:- end_tests(typeInf).