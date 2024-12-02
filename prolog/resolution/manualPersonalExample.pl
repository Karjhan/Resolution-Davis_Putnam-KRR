politician(X) :- corrupted(X).
corrupted(X) :- thief(X), criminal(X).
criminal(X) :- imprisoned(X).

thief(john).
