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

test_1 :-
    recommend(strength, 5, 5, 6, 5, mild, 8.0, good, high, high, Advice),
    assert_true(has_action(rest_seconds, Advice), 'T1 has rest_seconds'),
    get_action(load_adjustment, Advice, LoadAdj),
    assert_true((LoadAdj = increase ; LoadAdj = hold), 'T1 load is increase/hold'),
    get_action(deload, Advice, Deload),
    assert_true(Deload = no, 'T1 deload = no').

test_2 :-
    recommend(strength, 3, 5, 9, 15, mild, 7.0, ok, normal, medium, Advice),
    get_action(load_adjustment, Advice, LoadAdj),
    assert_true(LoadAdj = decrease, 'T2 load decreases'),
    get_action(rest_seconds, Advice, Rest),
    assert_true(Rest > 210, 'T2 rest increases above base').

test_3 :-
    recommend(hypertrophy, 8, 10, 8, 25, moderate, 5.5, poor, low, low, Advice),
    get_action(rest_seconds, Advice, Rest),
    assert_true(Rest > 90, 'T3 rest increases above base'),
    get_action(volume_adjustment, Advice, Vol),
    assert_true((Vol = reduce_25 ; Vol = reduce_30), 'T3 volume reduces'),
    get_action(warmup, Advice, Warm),
    assert_true((Warm = longer_general_warmup ; Warm = add_ramp_up_set), 'T3 warmup adjusted').

test_4 :-
    recommend(hypertrophy, 6, 10, 9, 40, severe, 6.0, poor, low, low, Advice),
    get_action(deload, Advice, Deload),
    assert_true(Deload = yes, 'T4 deload = yes').

test_5 :-
    recommend(endurance, 15, 12, 5, 5, none, 8.0, good, high, high, Advice),
    get_action(rest_seconds, Advice, Rest),
    assert_true(Rest >= 45, 'T5 rest >= base'),
    get_action(deload, Advice, Deload),
    assert_true(Deload = no, 'T5 deload = no').
