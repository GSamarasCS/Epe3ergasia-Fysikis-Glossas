/*----------------------------------------------------------------------*/
/* PROJECT ON */ /* NATUTAL LANGUAGE PROCESSING (NLP) */
/*----------------------------------------------------------------------*/
/* 'Natural Language Understanding : */ /* from a Ergasia3 to a
knowledge base' */
/*----------------------------------------------------------------------*/
/* 1. Lexical Analysis */ /* 2. Syntactic analysis */ /* 3. Semantic
analysis */ /* 4. Updating the Knowledge Base */ /* 5. Inserting Info
to knowledge base using tell(Sentence) */ /* 6. asking questions about
the Ergasia3 using ask(Question) */ /* Top query */ /* ?-
understand('Ergasia3.txt'). */
/*-------------------------------------------------------------------------------*/
/* Ergasia3 in text.txt : */ /* 'timmy comes quickly to the house. --------------*/
/* timmy finds tim. tim is confused. tim needs help. timmy helps tim.------------*/
/* timmy gets confused. timmy is sad. tim hugs timmy. the problem is difficult.--*/ 
/* timmy tries again. timmy is angry. timmy solves the---------------------------*/
/* problem. timmothy sees the problem. timothy laughs loudly at tim.'.-----------*/
/*-------------------------------------------------------------------------------*/
:- discontiguous ask/1, q/5.
:- dynamic chased/2, hates/2, has/2, needs/2, scary/1.
/* Verbs (v) */
/*---------------------------------------------------------------------*/
:- dynamic loves/2, love/2, hate/2, have/2, kicks/2, jumps/2, drops/4.
/*---------------------------------------------------------------------*/
/* Auxiliary Verbs (av) */
/*---------------------------------------------------------------------*/
:- dynamic does/2, are/2, do/2.
/*---------------------------------------------------------------------*/
/* Intransitive Verbs (iv) */
/*---------------------------------------------------------------------*/
:- dynamic runs/1,hurts/1,walks/1,jumps/1,shoots/1.
:- dynamic runs/2,hurts/2,walks/2.
/*---------------------------------------------------------------------*/
/* Transitive Verbs (tv) */
/*---------------------------------------------------------------------*/
:- dynamic gives/2, gave/2, gives/3.
/*---------------------------------------------------------------------*/
/* Adjectives (adj) */
/*---------------------------------------------------------------------*/
:- dynamic tall/1,short/1,blonde/1,slim/1,fat/1.




understand(Ergasia3) :-
  lexical_analyse(Ergasia3, Sentences),
   write('the lexical analysis completed!'), nl,nl,nl,
  syntax_analyse(Sentences, Syntax),
   write('the syntactic analysis completed!'),nl,nl,nl,
  semantics_analyse(Sentences, Semantics),
   write('the semantic analysis completed!'), nl,nl,nl,
  update_knowledge_base(Semantics),
   nl,write('Knowledge Base Updated!'),nl,nl,nl.



 /*==========================================================*/
 /* LEXICAL ANALYSIS */
 /*==========================================================*/
 /*----------------------------------------------------------------------*/
 /* Reads a text from a file and produces a list of sentences. */
 /* Each sentence is a list of words */
 /*----------------------------------------------------------------------*/
 % First Read from File then analyse
 lexical_analyse(Text, Result) :-
  %Text = 'Ergasia3.txt'
  see(Text), /* open this file to read */
  read(X), /* read from File */
  seen, /* close the input File */
  analyse(X,Result), /* Lexical Analysis */
  write_results(Result), /* Wite Result */
  !. /* stop now */
 /*----------------------------------------------------------------------*/
 /* analyse */
 /* 1. turn imput into list of ascii codes */
 /*2. group together ascii codes belonging to same word */
 /*----------------------------------------------------------------------*/
 % If we reach End of File then stop
 analyse(end_of_file,[]) :- !.
 % analyse
 analyse(Input,All) :- input_to_sentences(Input,All).
 /*----------------------------------------------------------------------*/
 /* Input to ascii */
 /* turn input into list of ASCII codes, pass list to tokeniser */
 /*----------------------------------------------------------------------*/
 input_to_sentences(Input, List_of_Words):-
  name(Input,Ascii_List),
  tokenise(Ascii_List, List_of_Words).
 /*----------------------------------------------------------------------*/
 /* TOKENISER */
 /*----------------------------------------------------------------------*/
 % If no ASCII codes left then stop
 tokenise([],[]):-!.
 % identify first sentence, move onto rest of sentences
 tokenise(Ascii_List,All):-
  one_sentence(Ascii_List, Sentence, Rest, T),
  T=end_of_sentence,
  tokenise(Rest, List_of_Sentences),
  append([Sentence],List_of_Sentences,All).
 /*----------------------------------------------------------------------*/
 /* ONE SENTENCE */
 /* one_sentence(Ascii_List, Sentence, Rest, end_of_sentence) */
 /* From List of Ascii codes identify a sentence */
 /*----------------------------------------------------------------------*/
 % full-stop = end of sentence
 one_sentence(Ascii_List, [Word], Rest_Ascii, end_of_sentence):-
  one_word(middle_of_word, Ascii_List, Ascii_Word, Rest_Ascii, T),
  T=end_of_sentence,
  name(Word,Ascii_Word), !.
 % end of text = no codes left
 one_sentence([], [], [], end_of_text):- !.
 /*----------------------------------------------------------------------*/
 /* if not the end of a sentence then add code to output list and */
 /* call recursively */
 /*----------------------------------------------------------------------*/
 one_sentence(Ascii_List, Sentence, Rest_Text, Type):-
  one_word(start_of_word, Ascii_List, Ascii_Word, Rest_Ascii, T),
  T=end_of_word,
  name(Word,Ascii_Word),
  one_sentence(Rest_Ascii, Rest_Words, Rest_Text, Type),
  append([Word], Rest_Words, Sentence).
 /*---------------------------------------------------------------------*/
 /* End of ONE SENTENCE */
 /*---------------------------------------------------------------------*/
 /*----------------------------------------------------------------------*/
 /* ONE WORD */
 /*----------------------------------------------------------------------*/
 % Terminate recursion :
 % end of text = no codes left
 one_word(middle_of_word, [], [], [], end_of_text):- !.
 % full-stop = end of sentence
 one_word(middle_of_word, [46|T], [], T, end_of_sentence):- !.
 % space = end of word
 one_word(middle_of_word, [32|T], [], T, end_of_word):- !.
 /*----------------------------------------------------------------------*/
 /* if not the end of a word then add code to output list and */
 /* recurse */
 /*----------------------------------------------------------------------*/
 % ignore Carriage return (Ascii 13)
 one_word(Any, [13|T], Word, Rest_Codes, Type):-
  one_word(Any, T, Word, Rest_Codes, Type).
 % ignore Line feed (Ascii 10)
 one_word(Any, [10|T], Word, Rest_Codes, Type):-
  one_word(Any, T, Word, Rest_Codes, Type).
 % ignore leading space
 one_word(start_of_word, [32|T], Word, Rest_Codes, Type):-
  one_word(start_of_word, T, Word, Rest_Codes, Type).
 % We have moves to analysing the word
 one_word(_, [Ascii_Code|T], [Ascii_Code|Word], Rest_Codes, Type):-
  Ascii_Code \= 32,
  one_word(middle_of_word, T, Word, Rest_Codes, Type).
 /*---------------------------------------------------------------------*/
 /* End of ONE WORD */
 /*---------------------------------------------------------------------*/
 /*==========================================================*/
 /* END OF LEXICAL ANALYSIS */
 /*==========================================================*/

 /*==========================================================*/
 /* SYNTACTIC ANALYSIS */
 /*==========================================================*/
 /*----------------------------------------------------------------------*/
 /*Takes as input a list of sentences and produces their */
 /* syntax trees */
 /*----------------------------------------------------------------------*/
 syntax_analyse(Sentences, Structures) :-
  syntactic_analysis(Sentences, Structures), % Syntactic Analysis
  write_results(Structures), % Wite Result
  !. % stop now
 syntactic_analysis([],[]) :- !.
 syntactic_analysis([Sentence|Sentences], [Structure|Structures]) :-
  snt(Structure, Sentence, []),
  syntactic_analysis(Sentences, Structures), !.
 /*==========================================================*/
 /* GRAMMAR RULES */
 /*==========================================================*/
 /*---------------------------------------------------------------------*/
 /*The s rules of Syntactic Analysis */
 /*---------------------------------------------------------------------*/
 /* To test: */
 /* ?- snt(Structure,[the,problem,is,difficult],[]). */
 /* Structure = */
 /* s(np(d(the),n(problem)),vp(av(is),adj(difficult)) */
 /*---------------------------------------------------------------------*/
 /*---------------------------------------------------------------------*/
 /* Sentence (snt) */
 /* Proper Nouns (pn) */
 /* Intransitive Verbs (iv) */
 /* Auxiliary Verbs (av) */
 /* Verbs (v) */
 /* Transitive Verbs (tv) */
 /* Adverb (adv) */
 /* Adjectives (adj) */
 /* Determiner (det) */
 /* Noun (n) */
 /* Preposition (pr)*/
 /* Noun Phrase (np) */
 /* Verb Phrase (vb) */
 /*---------------------------------------------------------------------*/
 /*---------------------------------------------------------------------*/
 /* Sentence (snt) */
 /*---------------------------------------------------------------------*/
 snt(s(NP,VP)) --> np(NP), vp(VP).

 /*---------------------------------------------------------------------*/
 /* Noun Phrase (np) */
 /*---------------------------------------------------------------------*/
 np(np(N)) --> pn(N).
 np(np(D,N)) --> det(D), n(N).
 np(np(N)) --> n(N).
 np(np(P,D,N)) --> pr(P), det(D), n(N).


 /*---------------------------------------------------------------------*/
 /* Verb Phrase (vb) */
 /*---------------------------------------------------------------------*/
 % Intransitive verbs :
 vp(vp(V)) --> iv(V).
 vp(vp(V,ADV)) --> iv(V), adv(ADV).
 vp(vp(V,D,NP)) --> iv(V), adv(ADV), np(NP).
 % Auxiliary verbs
 vp(vp(AV,A)) --> av(AV), adj(A).
 % Transitive verbs :
 vp(vp(TV, PN, NP)) --> v(TV), np(PN), np(NP).
 % verbs
 vp(vp(V,NP)) --> v(V), np(NP).
 vp(vp(V,D,NP)) --> v(V), det(D), np(NP).
 vp(vp(V,D,NP,NP)) --> v(V), det(D), np(NP), np(NP).
 /*==========================================================*/
 /* VOCABULARY OF EXAMPLE */
 /*==========================================================*/

 /*---------------------------------------------------------------------*/
 /* Intransitive Verbs (iv) */
 /*---------------------------------------------------------------------*/
 % needed for example :
 iv(iv(comes))-->[comes].
 iv(iv(come))-->[come].
 iv(iv(coming))-->[coming].
 iv(iv(gets))-->[gets].
 iv(iv(get))-->[get].
 iv(iv(getting))-->[getting].
 iv(iv(laughs))-->[laughs].
 iv(iv(laugh))-->[laugh].
 iv(iv(laughing))-->[laughing].
 iv(iv(tries))-->[tries].
 iv(v(try))-->[try].
 /*---------------------------------------------------------------------*/
 /* Auxiliary Verbs (av) */
 /*---------------------------------------------------------------------*/
 % needed for example :
 av(av(is))-->[is].
 % extension of vocabulary :
 av(av(does))-->[does].
 av(av(are))-->[are].
 av(av(do))-->[do].


 /*---------------------------------------------------------------------*/
 /* Verbs (v) */
 /*---------------------------------------------------------------------*/
 % needed for example :
 v(v(comes))-->[comes].
 v(v(come))-->[come].
 v(v(finds))-->[finds].
 v(v(find))-->[find].
 v(v(helps))-->[helps].
 v(v(help))-->[help].
 v(v(hugs))-->[hugs].
 v(v(hug))-->[hug].
 v(v(tries))-->[tries].
 v(v(try))-->[try].
 v(v(solves))-->[solves].
 v(v(solve))-->[solve].
 v(v(sees))-->[sees].
 v(v(see))-->[see].
 v(v(needs))-->[needs].
 v(v(need))-->[need].
 v(v(gets))-->[gets].
 v(v(get))-->[get].
 /*---------------------------------------------------------------------*/
 /* Adjectives (adj) */
 /*---------------------------------------------------------------------*/
 % needed for example :
 adj(adj(angry))-->[angry].
 adj(adj(sad))-->[sad].
 adj(adj(difficult))-->[difficult].
 adj(adj(confused))-->[confused].
 /*---------------------------------------------------------------------*/
 /* Adverb (adv) */
 /*---------------------------------------------------------------------*/
 % needed for example :
 adv(adv(quickly))-->[quickly].
 adv(adv(loudly))-->[loudly].
 adv(adv(again))-->[again].
 adv(adv(confused))-->[confused].
 /*---------------------------------------------------------------------*/
 /* Noun (n) */
 /*---------------------------------------------------------------------*/
 % needed for example
 n(n(house))-->[house].
 n(n(help))-->[help].
 n(n(problem))-->[problem].
 n(n(kids))-->[kids].
 /*---------------------------------------------------------------------*/
 /* Proper Nouns (pn) */
 /*---------------------------------------------------------------------*/
 % needed for example :
 pn(pn(tim))-->[tim].
 pn(pn(timmy))-->[timmy].
 pn(pn(timothy))-->[timothy].
 /*---------------------------------------------------------------------*/
 /* Determiner (det) */
 /*---------------------------------------------------------------------*/

 det(det(the)) -->[the].

 /*---------------------------------------------------------------------*/
 /* preposition (p) */
 /*---------------------------------------------------------------------*/
 % needed for example :
 pr(pr(at))-->[at].
 pr(pr(to))-->[to].
/*==========================================================*/
/* END OF SYNTACTIC ANALYSIS */
/*==========================================================*/


/*==========================================================*/
/* SEMANTIC ANALYSIS */
/*==========================================================*/

/*----------------------------------------------------------------------*/
/*Takes as input a list of sentences and produces their */
/* semantics - easier if done along with syntactic analysis */
/*----------------------------------------------------------------------*/
semantics_analyse(Sentences, AllSemantics) :-
 semantics_analysis(Sentences, AllSemantics), % Semantic Analysis
 write_results(AllSemantics), % Write Result
 !. % stop now
semantics_analysis([],[]) :- !.
semantics_analysis([Sentence|Sentences], [Sem|Semantics]) :-
 sem(_, Sem, Sentence, []),
 semantics_analysis(Sentences, Semantics), !.
/*==========================================================*/
/* SEMANTICS CREATION RULES */
/*==========================================================*/
sem(1,Sem) --> sem_np(N), sem_vp(1,V,N1), {Sem=..[V,N,N1]}.

sem(2,Sem) --> sem_np(N), sem_vp(2,_,A), {Sem=..[A,N]}.


sem(3,Sem) --> sem_np(N), sem_iv(V,s), {Sem=..[V,N]}.


sem(4,Sem) --> sem_np(N), sem_iv(V,s), sem_adv(A), {Sem=..[V,N,A]}.


sem(5,Sem) --> sem_np(N), sem_tv(V,s), sem_np(N1), sem_np(N2), {Sem=..[V,N,N1,N2]}.


sem(6,Sem) --> sem_np(N), sem_vp(1,V,N1), sem_pp(P), sem_np(N2), {Sem=..[V,N,N1,P,N2]}.


sem(7,Sem) --> sem_np(N), sem_iv(V,s), sem_adv(A), sem_pp(P), sem_np(N1), {Sem=..[V,N,A,P,N1]}.



/* noun phrase */
sem_np(N) --> sem_pn(N).
sem_np(N) --> sem_det(_), sem_n(N).
sem_np(N) --> sem_n(N).
%sem_np(P,N) --> sem_pr(P), sem_det(_), sem_n(N).
sem_pp(P) --> sem_pr(P).
/* verb phrase */
sem_vp(1,V,N) --> sem_v(V,s), sem_np(N).
sem_vp(2,is,A) --> sem_av(is), sem_adj(A).


/*==========================================================*/
/* SEMANTICS VOCABULARY */
/*==========================================================*/
/*---------------------------------------------------------------------*/
/* Intransitive Verbs (sem_iv) */
/*---------------------------------------------------------------------*/
% needed for example :
sem_iv(walks,s) -->[walks].
sem_iv(comes,s)-->[comes].
sem_iv(comes,q)-->[coming].
sem_iv(gets,s)-->[gets].
sem_iv(gets,q)-->[getting].
sem_iv(laughs,s)-->[laughs].
sem_iv(laughs,q)-->[laughing].
sem_iv(tries,s)-->[tries].
sem_iv(tries,q)-->[trying].
% extension of vocabulary :
sem_iv(runs,s) -->[runs].
sem_iv(runs,q) -->[running].
sem_iv(hurts,s) -->[hurts].
sem_iv(hurts,q) -->[hurting].
sem_iv(jumps,s) -->[jumps].
sem_iv(jumps,q) -->[jumping].
sem_iv(shoots,s) -->[shoots].
sem_iv(shoots,q) -->[shooting].
/*---------------------------------------------------------------------*/
/* Auxiliary Verbs (sem_av) */
/*---------------------------------------------------------------------*/
% needed for example :
sem_av(is) -->[is].
% extension of vocabulary :
sem_av(does) -->[does].
sem_av(do) -->[do].
sem_av(does) -->[did].
sem_av(are) -->[are].
/*---------------------------------------------------------------------*/
/* Transitive Verbs (sem_tv) */
/*---------------------------------------------------------------------*/
% needed for example :
% extension of vocabulary :
sem_tv(gives,s) -->[gives].
sem_tv(gives,q) -->[give].
sem_tv(gave,s) -->[gave].
sem_tv(gives,q2) -->[giving].
/*---------------------------------------------------------------------*/
/* Verbs (sem_v) */
/*---------------------------------------------------------------------*/
% needed for example :
sem_v(finds,s) -->[finds].
sem_v(finds,q) -->[find].
sem_v(hugs,q) -->[hug].
sem_v(hugs,s) -->[hugs].
sem_v(sees,q) -->[see].
sem_v(sees,s) -->[sees].
sem_v(helps,s) -->[helps].
sem_v(helps,q) -->[help].
sem_v(tries,s) -->[tries].
sem_v(tries,q) -->[try].
sem_v(solves,s) -->[solves].
sem_v(solves,q) -->[solve].
sem_v(needs,s) -->[needs].
sem_v(needs,q) -->[need].
sem_v(gets,s) -->[gets].
sem_v(gets,q) -->[get].
% extension of vocabulary :
sem_v(loves,s) -->[loves].
sem_v(loves,q) -->[love].
sem_v(hates,s) -->[hates].
sem_v(hates,q) -->[hate].
sem_v(kicks,s) -->[kicks].
sem_v(kicks,q) -->[kick].
sem_v(jumps,s) -->[jumps].
sem_v(jumps,q) -->[jump].
/*---------------------------------------------------------------------*/
/* Adjectives (sem_adj) */
/*---------------------------------------------------------------------*/
% needed for example :
sem_adj(angry)-->[angry].
sem_adj(sad)-->[sad].
sem_adj(difficult)-->[difficult].
sem_adj(confused)-->[confused].
% extension of vocabulary :
sem_adj(tall) -->[tall].
sem_adj(short) -->[short].
sem_adj(scary) -->[scary].
sem_adj(blonde) -->[blonde].
sem_adj(slim) -->[slim].
sem_adj(fat) -->[fat].
/*---------------------------------------------------------------------*/
/* Adverb (sem_adv) */
/*---------------------------------------------------------------------*/
% needed for example :
sem_adv(quickly) -->[quickly].
sem_adv(loudly)-->[loudly].
sem_adv(again)-->[again].
sem_adv(confused)-->[confused].
% extension of vocabulary :
sem_adv(slowly) -->[slowly].
sem_adv(independently) -->[independently].
/*---------------------------------------------------------------------*/
/* Noun (sem_n) */
/*---------------------------------------------------------------------*/
% needed for example
sem_n(house)-->[house].
sem_n(help)-->[help].
sem_n(problem)-->[problem].
sem_n(kids) -->[kids].
% extension of vocabulary
sem_n(book) -->[book].
sem_n(books) -->[books].
sem_n(feather) -->[feather].
sem_n(feathers) -->[feathers].
sem_n(baby) -->[baby].
sem_n(babies) -->[babies].
sem_n(boy) -->[boy].
sem_n(boys) -->[boys].
sem_n(girl) -->[girl].
sem_n(girls) -->[girls].
sem_n(icecream) -->[icecream].
sem_n(icecreams) -->[icecreams].
sem_n(mary) -->[mary].
sem_n(john) -->[john].
sem_n(tomy) -->[tomy].
sem_n(theo) -->[theo].
sem_n(george) -->[george].
sem_n(X) -->sem_pn(X). % a proper noun is also a noun
/*---------------------------------------------------------------------*/
/* Proper Nouns (sem_pn) */
/*---------------------------------------------------------------------*/
% needed for example :
sem_pn(tim) -->[tim].
sem_pn(timmy) -->[timmy].
sem_pn(timothy) -->[timothy].
% extension of vocabulary :
sem_pn(mary) -->[mary].
sem_pn(tomy) -->[tomy].
/*---------------------------------------------------------------------*/
/* Determiner (det) */
/*---------------------------------------------------------------------*/
% needed for example :
sem_det(the) -->[the].
sem_det(a) -->[a].
sem_det(an) -->[an].
/*---------------------------------------------------------------------*/
/* prepositions (pr) */
/*---------------------------------------------------------------------*/
% needed for example :
sem_pr(towards) -->[towards].
sem_pr(to) -->[to].
sem_pr(at) -->[at].
% extension of vocabulary :
/*==========================================================*/
/* END OF SEMANTICS VOCABULARY */
/*==========================================================*/

/*==========================================================*/
/* KNOWLEDGE BASE SESSION */
/*==========================================================*/
/*---------------------------------------------------------------------*/
/* update knowledge base */
/*---------------------------------------------------------------------*/
update_knowledge_base([]) :- !.
update_knowledge_base([S|Sem]) :-
 assert(kb_fact(S)),
 write(kb_fact(S)),write(' asserted'),nl,
 update_knowledge_base(Sem), !.
/*---------------------------------------------------------------------*/
/* all facts of knowledge base */
/*---------------------------------------------------------------------*/
show_kb :- listing(kb_fact/1).
/*---------------------------------------------------------------------*/
/* insert additional information to knowledge base */
/*---------------------------------------------------------------------*/
/*---------------------------------------------------------------------*/
/* examples : */
/* ?- tell([john,loves,icecream]). */
/* ?- tell([the,cat,is,fat]). */
/* ?- tell([george,walks,quickly,towards,the,cat]). */
/* ?- tell([theo,hugs,the,dog]). */                                          
/*---------------------------------------------------------------------*/
tell(Sentence):-
 sem(_, Sem, Sentence, []),
 assert(kb_fact(Sem)),
 nl,write(kb_fact(Sem)), nl, write(' added to knowledge base.'),nl, !.
/*---------------------------------------------------------------------*/
/* ask knowledge base */
/*---------------------------------------------------------------------*/
% Yes-No questions
/*---------------------------------------------------------------------*/
/* examples : */
/* ?- ask([is,the,problem,difficult]). */
/* ?- ask([does,timmy,solve,the,problem]). */
/* ?- ask([is,tim,sad]). */

/*---------------------------------------------------------------------*/
ask(X):- q(_, tf, Sem, X, []),
 if_then_else(kb_fact(Sem), write('Yes.'), write('No.') ), !.
/*---------------------------------------------------------------------*/
/* yes/no queries */
/*---------------------------------------------------------------------*/
q(1,tf,Sem) --> sem_av(does), sem_pn(N), sem_v(V,q), sem_np(N1), {Sem=..[V,N,N1]}.
q(1,tf,Sem) --> sem_av(did), sem_pn(N), sem_v(V,q), sem_np(N1), {Sem=..[V,N,N1]}.
q(2,tf,Sem) --> sem_av(is), sem_np(N), sem_adj(A),
 {Sem=..[A,N]}.
q(3,tf,Sem) --> sem_av(does), sem_pn(N), sem_v(have), sem_n(N1),
 {Sem=..[have,N,N1]}.
q(4,tf,Sem) --> sem_av(is), sem_pn(N), sem_iv(V,q),
 {Sem=..[V,N]}.
q(5,tf,Sem) --> sem_av(is), sem_pn(N), sem_iv(V,q), sem_adv(A),
 {Sem=..[V,N,A]}.
q(6,tf,Sem) --> sem_av(does), sem_pn(N), sem_tv(V,q), sem_pn(N1),
sem_np(N2), {Sem=..[V,N,N1,N2]}.
q(6,tf,Sem) --> sem_av(does), sem_pn(N), sem_tv(V,q), sem_pn(N1), sem_np(N2),
 {Sem=..[V,N,N1,N2]}.

% other questions
/*---------------------------------------------------------------------*/
/* examples : */
/* ?- ask([who,is,difficult]). */
/* ?- ask([who,needs,help]). */
/* ?- ask([who,is,confused]). */
/*---------------------------------------------------------------------*/
ask(X):- q(_,fact, Fact, X, []), write(Fact), !.
/*---------------------------------------------------------------------*/
/* fact queries */
/*---------------------------------------------------------------------*/
q(1,fact,F) --> [who], sem_v(V,s),sem_n(N1),
 {Sem=..[V,F,N1], kb_fact(Sem)}.
q(1,fact,F) --> [what], sem_av(does),sem_pn(N),sem_v(V,q),
 {Sem=..[V,N,F], kb_fact(Sem)}.
q(2,fact,F) --> [who], sem_av(is), sem_adj(A),
 {Sem=..[A,F],kb_fact(Sem)}.
q(3,fact,F) --> [who], sem_vp(1,V,N1),
 {Sem=..[V,F,N1],kb_fact(Sem)}.
q(3,fact,F) --> [who], sem_av(does),sem_pn(N),sem_v(V,q),
 {Sem=..[V,N,F],kb_fact(Sem)}.
q(4,fact,F) --> [who], sem_av(is),sem_iv(V,q),
 {Sem=..[V,F],kb_fact(Sem)}.
q(5,fact,F) --> [how], sem_av(does),sem_pn(N),sem_iv(V,s),
 {Sem=..[V,N,F],kb_fact(Sem)}.
q(5,fact,F) --> [how], sem_av(is),sem_pn(N),sem_iv(V,q),
 {Sem=..[V,N,F],kb_fact(Sem)}.
q(5,fact,F) --> [who], sem_iv(V,s),sem_adv(A),
 {Sem=..[V,F,A],kb_fact(Sem)}.
q(6,fact,F) --> [who], sem_tv(V,s),sem_pn(N1),sem_np(N2),
 {Sem=..[V,F,N1,N2],kb_fact(Sem)}.
q(6,fact,F) --> [who], sem_av(is),sem_pn(N),sem_tv(V,q2),sem_np(N2),[to],
 {Sem=..[V,N,F,N2] ,kb_fact(Sem)}.
q(6,fact,F) --> [what],[is],sem_pn(N),sem_tv(V,q2),[to],sem_pn(N1),
 {Sem=..[V,N,N1,F] ,kb_fact(Sem)}.

/*==========================================================*/
/* END OF KNOWLEDGE BASE SESSION */
/*==========================================================*/


/*==========================================================*/
/* GENERAL LIBRARY */
/*==========================================================*/
/*---------------------------------------------------------------------*/
/* Write Results */
/*---------------------------------------------------------------------*/
write_results([]) :- nl, !.
write_results([H|T]) :-
 write(H), nl,
 write_results(T).
if_then_else(Condition,A,_):- call(Condition), call(A), !.
if_then_else(_,_,B):- call(B), !.
