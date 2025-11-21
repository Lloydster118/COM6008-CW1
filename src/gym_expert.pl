:- module(gym_expert, [
    rule/1
]).

% Training goals
goal(strength).
goal(hypertrophy).
goal(endurance).

% RPE 1–10
rpe(V) :-
    number(V),
    V >= 1,
    V =< 10.

% Sleep quality categories
sleep_quality(poor).
sleep_quality(ok).
sleep_quality(good).

% Muscle soreness levels
soreness(none).
soreness(mild).
soreness(moderate).
soreness(severe).

% HRV state
hrv(low).
hrv(normal).
hrv(high).

% Overall recovery
recovery(low).
recovery(medium).
recovery(high).

lt(A,B) :- A < B.
gt(A,B) :- A > B.
betweeni(L,H,X) :- X >= L, X =< H.

% Human-readable rule descriptions (for docs / debugging)
rule('Strength base rest ~ 180-240 seconds.').
rule('Hypertrophy base rest ~ 60-120 seconds.').
rule('Endurance base rest ~ 30-60 seconds.').

% Machine-usable rest values (seconds)
base_rest(strength,    210).   % 3.5 minutes
base_rest(hypertrophy,  90).   % 1.5 minutes
base_rest(endurance,    45).   % 45 seconds