/* Απαλακτική Εργασία Ιουνίου 2021 */
/* Θέμα 1ο */
/* α) Με ποιά γραμματική σε μορφή DCG μπορούμε να αναγνωρίσουμε την */
/* πρόταση : [the, waiter, brought, the, meal, to, the, table], σύμφωνα με το παρακάτω σχήμα;  */


:- discontiguous noun/2.

s --> np, vp.
np --> det,noun.
pp --> prep,np.
vp --> verb,np,pp.
det-->[the].
noun-->[waiter].
noun-->[meal].
verb-->[brought].
prep-->[to].
noun-->[table].

/* Ερώτηση 1α */
/* s([the, waiter, brought, the, meal, to, the, table],[]). */

/* β) Με ποιά γραμματική σε μορφή DCG μπορούμε να παράγουμε σε μορφή */
/* functor το συντακτικό δένδρο για την αναγνώριση της πρότασης : */
/* [the, waiter, brought, the, meal, to, the, table], σύμφωνα με το παρακάτω σχήμα; */

:- discontiguous noun/3.

s(s(NP,VP))-->np(NP),vp(VP).
np(np(D,N))-->det(D),noun(N).
pp(pp(PREP,NP))-->prep(PREP),np(NP).
vp(vp(V,NP,PP))-->verb(V),np(NP),pp(PP).
det(det(the))-->[the].
prep(prep(to))-->[to].
noun(noun(waiter))-->[waiter].
verb(verb(brought))-->[brought].
noun(noun(meal))-->[meal].
noun(noun(table))-->[table].


/* Ερώτηση 1β */
/* s(S,[the, waiter, brought, the, meal, to, the, table],[]). */