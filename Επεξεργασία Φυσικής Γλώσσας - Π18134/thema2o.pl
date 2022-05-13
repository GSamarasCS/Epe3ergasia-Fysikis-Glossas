/* Απαλακτική Εργασία Ιουνίου 2021 */
/* Θέμα 2ο */
/* Το παρακάτω πρόγραμμα αναγνωρίζει και υπολογίζει αριθμητικές εκφράσεις*/
/*όπως αναλύθηκε στο θεωρητικό μέρος. Να αναπτυχθεί ένα αντίστοιχο*/
/*πρόγραμμα όπου οι αριθμοί είναι διαδικοί και οι αριθμητικές εκφράσεις είναι*/
/*αντίστοιχα αριθμητικές εκφράσεις διαδικών αριθμών  */
/* de doulevei me olous tous arithmous */

expression(Value) --> number(Value).
expression(Value) --> number(X), [+], expression(V), {Value is X+V}.
expression(Value) --> number(X), [-], expression(V),{Value is X-V}.
expression(Value) --> number(X), [*], expression(V), {Value is X*V}.
expression(Value) --> number(X), [/], expression(V),{V\=0, Value is X/V}.
expression(Value) --> left_parenthesis, expression(Value), right_parenthesis.
left_parenthesis --> ['('].
right_parenthesis --> [')'].
number(X) --> digit(X).
number(Value) --> digit(X), number(Y), {numberofdigits(Y,N), Value is X*2^N+Y}.
digit(0) --> [0].
digit(1) --> [1].
numberofdigits(Y,1) :- Z is Y/2, Z<1, !.
numberofdigits(Y,N) :-
Z is (Y - mod(Y,2))/2,
numberofdigits(Z,N1),
N is N1+1, !.

/* Ερώτηση 2 */
% expression(V,[1,0,1,+,1,0],[]).
% expression(V,[1,1,-,1,1,0],[]).
% expression(V,[1,1,1,1,*,1,1],[]).
% expression(V,[1,1,1,1,/,1,1],[]).
