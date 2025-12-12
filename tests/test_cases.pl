:- module(test_cases, [run_tests/0]).

:- use_module('../src/gym_expert').

run_tests :-
    writeln('--- Running Gym Expert System Tests ---'),
    test_1,
    test_2,
    test_3,
    test_4,
    test_5,
    writeln('--- All tests executed ---').

has_action(Key, Advice) :-
    member(action(Key, _), Advice).

get_action(Key, Advice, Value) :-
    member(action(Key, Value), Advice).

assert_true(Cond, Name) :-
    (   call(Cond)
    ->  format('PASS: ~w~n', [Name])
    ;   format('FAIL: ~w~n', [Name])
    ).