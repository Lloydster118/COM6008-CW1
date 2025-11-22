:- module(gym_expert, [
    rule/1
]).

goal(strength).
goal(hypertrophy).
goal(endurance).

rpe(V) :-
    number(V),
    V >= 1,
    V =< 10.

sleep_quality(poor).
sleep_quality(ok).
sleep_quality(good).

soreness(none).
soreness(mild).
soreness(moderate).
soreness(severe).

hrv(low).
hrv(normal).
hrv(high).

recovery(low).
recovery(medium).
recovery(high).

lt(A,B) :- A < B.
gt(A,B) :- A > B.
betweeni(L,H,X) :- X >= L, X =< H.

rule('Strength base rest ~ 180-240 seconds.').
rule('Hypertrophy base rest ~ 60-120 seconds.').
rule('Endurance base rest ~ 30-60 seconds.').

base_rest(strength,    210).
base_rest(hypertrophy,  90).
base_rest(endurance,    45).

rule('If strength & RPE =< 7 & hit target reps -> increase ~3.5%.').
rule('If strength & RPE >= 9 or miss target by >=2 reps -> decrease ~5%.').

load_delta(strength, Reps, Target, RPE, increase, 0.035) :-
    Reps >= Target,
    RPE =< 7, !.

load_delta(strength, Reps, Target, RPE, decrease, 0.05) :-
    ( RPE >= 9
    ; Target - Reps >= 2
    ), !.

load_delta(strength, _, _, _, hold, 0.0).

rule('If hypertrophy & RPE =< 6 & >= target -> increase ~3%.').
rule('If hypertrophy & RPE >= 9 or well below target -> decrease ~3.5%.').

load_delta(hypertrophy, Reps, Target, RPE, increase, 0.03) :-
    Reps >= Target,
    RPE =< 6, !.

load_delta(hypertrophy, Reps, Target, RPE, decrease, 0.035) :-
    ( RPE >= 9
    ; Target - Reps >= 2
    ), !.

load_delta(hypertrophy, _, _, _, hold, 0.0).

rule('If endurance & RPE =< 5 & >= target -> increase 2-3%.').
rule('If endurance & RPE >= 8 or well below target -> decrease 3-5%.').

load_delta(endurance, Reps, Target, RPE, increase, 0.025) :-
    Reps >= Target,
    RPE =< 5, !.

load_delta(endurance, Reps, Target, RPE, decrease, 0.04) :-
    ( RPE >= 8
    ; Target - Reps >= 2
    ), !.

load_delta(endurance, _, _, _, hold, 0.0).

rule('If RPE >= 9 -> add 60-90s rest.').
rule('If performance drop >= 20% -> add 60-120s rest.').
rule('If soreness moderate/severe -> add 30-60s rest.').
rule('If sleep < 6.5h or quality poor -> add 30-60s rest.').
rule('If HRV low or recovery low -> add 30-90s rest.').
rule('Otherwise keep base rest.').

rest_adjust(RPE, PerfDrop, Soreness, SleepH, SleepQ,
            HRV, Recovery, Base, Rest) :-

    Extra1 is (RPE >= 9      -> 75 ; 0),
    Extra2 is (PerfDrop >=20 -> 90 ; 0),
    sore_extra(Soreness, E3),
    sleep_extra(SleepH, SleepQ, E4),
    recov_extra(HRV, Recovery, E5),

    Total is Base + Extra1 + Extra2 + E3 + E4 + E5,
    clamp(Total, 30, 420, Rest).

sore_extra(moderate, 45).
sore_extra(severe,   60).
sore_extra(_,        0).

sleep_extra(H, poor, 45) :- H =< 7.0, !.
sleep_extra(H, _,    30) :- H <  6.5, !.
sleep_extra(_, _,     0).

recov_extra(low,   _,    60).
recov_extra(_,     low,  60).
recov_extra(normal,medium, 0).
recov_extra(high,  high,   0).
recov_extra(_,     _,      0).

clamp(X, L, _, L) :- X < L, !.
clamp(X, _, H, H) :- X > H, !.
clamp(X, _, _, X).