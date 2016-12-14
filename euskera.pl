% Autor:
% Fecha: 01/09/2016



% X tiene que ir "" para que prolog lo lea como una lista de codigos ANSII. El predicado euskera/1 dice que algo es euskera si hacemos el procedimiento lista/2 y el comprobar_palabras/1.




euskera(X,Restos,Origen):-
                           euskera(X,Restos),
                           origen_etimologicoeuskera(Restos,Origen).

origen_etimologicoeuskera([],[]).

origen_etimologicoeuskera([Restos|MasRestos],[Origen|MasOrigen]):-
                                                 atom_chars(Restos,Deletreo),
                                                 combinacioneseuskera(Restos,Deletreo,Origen),
                                                 !,
                                                 origen_etimologicoeuskera(MasRestos,MasOrigen).

combinacioneseuskera(Restos,[J],desconocido(Restos)).


combinacioneseuskera(Restos,[t,l|W],griego_o_nahuatl(Restos)).


combinacioneseuskera(Restos,[p,n|W],griego(Restos)).

combinacioneseuskera(Restos,[f,t|W],griego_o_arabe(Restos)).

combinacioneseuskera(Restos,[g,m|W],griego(Restos)).

combinacioneseuskera(Restos,[g,d|W],griego(Restos)).

combinacioneseuskera(Restos,[p,s|W],griego_o_latin(Restos)).

% combinacioneseuskera(Restos,[u,a,i|W],amerindio_o_catalan(Restos)).



combinacioneseuskera(Restos,[X,Y|W],Origen):-
                                       combinacioneseuskera(Restos,[Y|W],Origen).



euskera(X,Restos):-
               transcribir(X,W),
               !,
               lista(W,Y),
               comprobar_palabraseuskera(Y,N),
               recuperar_descarteseuskera(N,Restos).

recuperar_descarteseuskera([],[]).

recuperar_descarteseuskera([N|M],[Restos|MasRestos]):-
               atom_codes(Restos,N),
               recuperar_descarteseuskera(M,MasRestos).


transcribir([],[]).

transcribir([W|Y],[W|Z]):-
                          W<65,
                          transcribir(Y,Z).

transcribir([W|Y],[A|Z]):-
                          W=<90,
                          W>=65,
                          A is W + 32,
                          transcribir(Y,Z).

transcribir([W|Y],[A|Z]):-
                          W=<220,
                          W>=193,
                          A is W + 32,
                          transcribir(Y,Z).

transcribir([W|Y],[W|Z]):-
                          W>90,
                          transcribir(Y,Z).











% El procedimiento lista/2 elimina espacios y signos de puntuacion. Devuelve una lista que contiene las listas individuales de cada palabra con sus caracteres ANSII, pero ya sin espacios ni puntuacion.
% El procedimiento comprobar_palabras/2  coge cada una de esas listas individuales y lo manda al proceso de silabeo, una a una.


%corte(Inicial, Resfinal, Resprov, Sublista)

lista(X, Y):- corte(X, Y, _, []), !.


%%%%METODO AUXILIAR%%%%

%Reglas base
corte([], Resprov, Resfinal, []):-append(Resprov, [], Resfinal).
corte([], Resfinal, Resprov, Sublista):-Sublista\=[], append(Resprov, [Sublista], Resfinal).

%listas vacias cuando aparece dos veces consecutivas 1000 cambiar las dos reglas anteriores por corte([], Resfinal, Resprov, Sublista):-append(Resprov, [Sublista], Resfinal).


% Esta regla sirve para evitar meter listas vacias si te encuentras dos veces seguidas 1000

corte([SignPunt|Resto], Resfinal, Resprov, Sublista):-
                                                    signPunt_euskera(SignPunt),
                                                    Sublista=[],
                                                    corte(Resto, Resfinal, Resprov, []).

% Reglas recursivas
corte([SignPunt|Resto], Resfinal, Resprov, Sublista):-
                                                       signPunt_euskera(SignPunt),
                                                       append(Resprov, [Sublista], Res),
                                                       corte(Resto, Resfinal, Res, []).

corte([Cab|Resto], Resfinal, Resprov, Sublista):-
                                                   not(signPunt_euskera(Cab)),
                                                   append(Sublista, [Cab], Prov),
                                                   corte(Resto, Resfinal, Resprov, Prov).

% Signos de puntuacion considerados

signPunt_euskera(32).
signPunt_euskera(33).
signPunt_euskera(34).
signPunt_euskera(38).
signPunt_euskera(40).
signPunt_euskera(41).
signPunt_euskera(44).
signPunt_euskera(45).
signPunt_euskera(46).
signPunt_euskera(47).
signPunt_euskera(58).
signPunt_euskera(59).
signPunt_euskera(63).
signPunt_euskera(132).
signPunt_euskera(147).
signPunt_euskera(148).
signPunt_euskera(161).
signPunt_euskera(191).
signPunt_euskera(171).
signPunt_euskera(187).
signPunt_euskera(8220).
signPunt_euskera(8221).
signPunt_euskera(8211).
signPunt_euskera(91).
signPunt_euskera(93).
signPunt_euskera(92).
signPunt_euskera(8212).
signPunt_euskera(60).
signPunt_euskera(62).
signPunt_euskera(8230).

signPunt_euskera(X):-
             X>47,
             X<58.







comprobar_palabraseuskera([],[]).


comprobar_palabraseuskera([Z|W],Sobras):-
                       silabeareuskera(Z,F),
                       comprobar_palabraseuskera(W,Sobras),
                       !.

comprobar_palabraseuskera([Z|W],[Z|Sobras]):-
                       not(silabeareuskera(Z,F)),
                       comprobar_palabraseuskera(W,Sobras),
                       !.








% El proceso de silabeo se incia con la transformacion de lista de codigo ANSII a letras sueltas para poder silabear, y lo manda ya en forma de letras al proceso auxiliar de silabeo


silabeareuskera(X,Z):-
               atom_codes(Y,X),
               atom_chars(Y,W),
               olfatear_euskera(W),
               %% contar_acentos(W,Numero),
               %% numero_acentos(Numero),
               silabear_auxeuskera(W,Z).

/*
numero_acentos(Numero):-
                           Numero=<1.

numero_acentos(Numero):-
                          Numero>1,
                          !,
                          fail.
*/






olfatear_euskera([]).


/*
olfatear_euskera([n,b|T]):-
                           !,
                           fail.

olfatear_euskera([n,p|T]):-
                           !,
                           fail.

olfatear_euskera([m,v|T]):-
                           !,
                           fail.


*/

olfatear_euskera([X|Y]):-
                 olfatear_euskera(Y).

silabeoeuskera(X,Z):-
               silabear_auxeuskera(X,Z).

% El proceso auxiliar de silabeo

silabear_auxeuskera([],[]).

silabear_auxeuskera([Z|T],[Silaba|Silaba_resto]):-
                 posibilidadeseuskera(Z,Silabeo,[Z|T],Resto),
                 atom_chars(Silaba,Silabeo),
                 silabear_auxeuskera(Resto,Silaba_resto).









% Letras en euskera





vocal_euskera(X) :-
                 vocalDebil_eus(X).
                 
vocal_euskera(X) :-
                 vocalFuerte_eus(X).





cons_feuskera(r).
cons_feuskera(s).
cons_feuskera(l).
cons_feuskera(n).
cons_feuskera(d).
cons_feuskera(z).
cons_feuskera(k).
cons_feuskera(t).
consFinalTotal(s).

grupoFinal_eus(t,z).
grupoFinal_eus(s,t).
grupoFinal_eus(t,s).
grupoFinal_eus(n,t).
grupoFinal_eus(n,t,z).

bilabial(p).
bilabial(b).

liquida(r).
liquida(l).

%% Consontantes emparejadas

% b

consParejaeuskera(b,[b,Liquida|_]):-
                              liquida(Liquida).

% f

consParejaeuskera(f,[f,Liquida|_]):-
                              liquida(Liquida).

% g

consParejaeuskera(g,[g,Liquida|_]):-
                              liquida(Liquida).

% c

consParejaeuskera(k,[k,Liquida|_]):-
                              liquida(Liquida).


% g

consParejaeuskera(p,[p,Liquida|_]):-
                              liquida(Liquida).

% d

consParejaeuskera(d,[d,r|_]).

% t

consParejaeuskera(t,[t,r|_]).
consParejaeuskera(t,[t,x|_]).
consParejaeuskera(t,[t,z|_]).
consParejaeuskera(t,[t,s|_]).
% ch

% consParejaeuskera(c,[c,h|_]).

% ll

consParejaeuskera(l,[l,l|_]).

% rr

% consParejaeuskera(r,[r,r|_]).

% qu

% consParejaeuskera(q,[q,u|_]).

vocalFuerte_eus(a).
vocalFuerte_eus(e).
vocalFuerte_eus(o).

vocalDebil_eus(i).
vocalDebil_eus(u).

diptongoeuskera(VocalFuerte,[VocalFuerte,VocalDebil|_]):-
                                                         vocalFuerte_eus(VocalFuerte),
                                                         vocalDebil_eus(VocalDebil).
                                                         
diptongoeuskera(VocalDebil,[VocalDebil,VocalFuerte|_]):-
                                                         vocalFuerte_eus(VocalFuerte),
                                                         vocalDebil_eus(VocalDebil).
diptongoeuskera(VocalDebil,[VocalDebil,VocalDebil2|_]):-
                                                         vocalDebil_eus(VocalDebil),
                                                         vocalDebil_eus(VocalDebil2).
                                                         
hiatoeuskera(VocalFuerte,[VocalFuerte,VocalFuerte2|_]):-
                                                         vocalFuerte_eus(VocalFuerte),
                                                         vocalFuerte_eus(VocalFuerte2).

/*

%% Vocales emparejadas: DIPTONGOS

diptongoeuskera(a,[a,i|_]).

diptongoeuskera(a,[a,u|_]).

% diptongoeuskera(a,[a,y|_]).

diptongoeuskera(e,[e,i|_]).

diptongoeuskera(e,[e,u|_]).

% diptongoeuskera(e,[e,y|_]).

diptongoeuskera(i,[i,a|_]).


diptongoeuskera(i,[i,e|_]).

diptongoeuskera(i,[i,o|_]).

diptongoeuskera(i,[i,u|_]).

diptongoeuskera(o,[o,i|_]).

% diptongoeuskera(o,[o,y|_]).

diptongoeuskera(u,[u,a|_]).

diptongoeuskera(u,[u,e|_]).
diptongoeuskera(u,[u,i|_]).
diptongoeuskera(u,[u,o|_]).



hiatoeuskera(a,[a,e|_]).


hiatoeuskera(Vocal_a,[Vocal_a,Vocal_o|_]):-
                               letras_equivalenteseuskera(a,Vocal_a),
                               letras_equivalenteseuskera(o,Vocal_o).

hiatoeuskera(a,[a,ú|_]).

hiatoeuskera(Vocal_a,[Vocal_a,Vocal_a|_]):-
                               letras_equivalenteseuskera(a,Vocal_a),
                               letras_equivalenteseuskera(a,Vocal_a).

hiatoeuskera(Vocal_e,[Vocal_e,Vocal_a|_]):-
                               letras_equivalenteseuskera(e,Vocal_e),
                               letras_equivalenteseuskera(a,Vocal_a).

hiatoeuskera(Vocal_e,[Vocal_e,Vocal_e|_]):-
                               letras_equivalenteseuskera(e,Vocal_e),
                               letras_equivalenteseuskera(e,Vocal_e).

hiatoeuskera(e,[e,í|_]).

hiatoeuskera(Vocal_e,[Vocal_e,Vocal_o|_]):-
                               letras_equivalenteseuskera(e,Vocal_e),
                               letras_equivalenteseuskera(o,Vocal_o).

hiatoeuskera(e,[e,ú|_]).

hiatoeuskera(í,[í,a|_]).

hiatoeuskera(í,[í,e|_]).

hiatoeuskera(í,[í,o|_]).

hiatoeuskera(Vocal_o,[Vocal_o,Vocal_a|_]):-
                               letras_equivalenteseuskera(o,Vocal_o),
                               letras_equivalenteseuskera(a,Vocal_a).

hiatoeuskera(Vocal_o,[Vocal_o,Vocal_e|_]):-
                               letras_equivalenteseuskera(o,Vocal_o),
                               letras_equivalenteseuskera(e,Vocal_e).

hiatoeuskera(Vocal_o,[Vocal_o,í|_]):-
                               letras_equivalenteseuskera(o,Vocal_o).

hiatoeuskera(Vocal_o,[Vocal_o,Vocal_o2|_]):-
                               letras_equivalenteseuskera(o,Vocal_o),
                               letras_equivalenteseuskera(o,Vocal_o2).

hiatoeuskera(ú,[ú,a|_]).

hiatoeuskera(ú,[ú,o|_]).

*/

triptongoeuskera(i,[i,a,e]).
triptongoeuskera(i,[i,e,a]).
triptongoeuskera(o,[o,a,i]).
triptongoeuskera(e,[e,o,i]).
triptongoeuskera(e,[e,a,i]).
triptongoeuskera(i,[i,o,e]).
triptongoeuskera(i,[i,o,a]).
triptongoeuskera(u,[u,a,i]).
triptongoeuskera(i,[i,o,i]).




%% Consontantes pseudofinales: contextos

% Letra m

/*
pseudocons_feuskera(m,[m,Bilabial|_]):-
                                  bilabial(Bilabial).
*/

%% dudas sobre el caso -mn- son helenismos o latinismos, aunque muchos y hasta adapt zirkumnabegatu
pseudocons_feuskera(m,[m,n|_]).



% Letra g
%% dudas sobre el caso -gn- son helenismos o latinismos, aunque muchos

pseudocons_feuskera(g,[g,n|_]).
pseudocons_feuskera(g,[g,m|_]).

% Letra p

dentaleuskera(t).
dentaleuskera(z).
pseudocons_feuskera(p,[p,t|_]):-
                                  dentaleuskera(Dentales).




% Letra x

pseudocons_feuskera(x,[x,t|_]).
pseudocons_feuskera(x,[x,k|_]).




conseuskera(b).
conseuskera(d).
conseuskera(f).
conseuskera(g).
conseuskera(h).
conseuskera(j).
conseuskera(k).
conseuskera(l).
conseuskera(m).
conseuskera(n).
conseuskera(p).
conseuskera(r).
conseuskera(s).
conseuskera(t).
conseuskera(x).
conseuskera(z).
% son semiconsonantes?
conseuskera(i).
conseuskera(u).



% Silabas en euskera

% posibilidades/4: letra que encabeza la silaba/silaba/letras que siguen/resto de la palabra (sin la silaba que acabamos de aislar, pero si con el contexto.

%% PREFIJOS

% sub

posibilidadeseuskera(s,[s,u,b,s],[s,u,b,s,Cons|W],Resto):-
                                                                conseuskera(Cons),
                                                                append([s,u,b,s],Resto,[s,u,b,s,Cons|W]),
                                                                !.

posibilidadeseuskera(s,[s,u,b],[s,u,b,Cons|W],Resto):-
                                                                conseuskera(Cons),
                                                                append([s,u,b],Resto,[s,u,b,Cons|W]),
                                                              !.



% ob

posibilidadeseuskera(o,[o,b,s],[o,b,s,Cons|W],Resto):-
                                                                conseuskera(Cons),
                                                                append([o,b,s],Resto,[o,b,s,Cons|W]),
                                                                 !.

posibilidadeseuskera(o,[o,b],[o,b,Cons|W],Resto):-
                                                                conseuskera(Cons),
                                                                not(consParejaeuskera(b,[b,Cons])),
                                                                append([o,b],Resto,[o,b,Cons|W]),
                                                                 !.


%ab

posibilidadeseuskera(a,[a,b,s],[a,b,s,Cons|W],Resto):-
                                                                conseuskera(Cons),
                                                                append([a,b,s],Resto,[a,b,s,Cons|W]),
                                                                !.

posibilidadeseuskera(a,[a,b],[a,b,Cons|W],Resto):-
                                                               conseuskera(Cons),
                                                                not(consParejaeuskera(b,[b,Cons])),
                                                                 append([a,b],Resto,[a,b,Cons|W]),
                                                                !.
/*
%ex

posibilidadeseuskera(e,[e,x],[e,x,Cons|W],Resto):-
                                                                conseuskera(Cons),
                                                                append([e,x],Resto,[e,x,Cons|W]),
                                                                 !.
                                                                 
*/





%%% silabas que empiezan por vocal

%% OJO PARA TODO EL CÓDIGO QUE SIGUE: OI, AU y AI son los unicos dipt que forman dipt tal cual. Los desmas son extranj: comprobar


% a

posibilidadeseuskera(Vocal,[Vocal],[Vocal],Resto):-
                                                                vocal_euskera(Vocal),
                                                                append([Vocal],Resto,[Vocal]),
                                                                !.

/*
posibilidadeseuskera(y,[y],[y],Resto):-
                                                               append([y],Resto,[y]),
                                                                !.
*/

% a-la

posibilidadeseuskera(Vocal,[Vocal],[Vocal,Cons,Vocal1|W],Resto):-
                                                                vocal_euskera(Vocal),
                                                                conseuskera(Cons),
                                                                vocal_euskera(Vocal1),
                                                                append([Vocal],Resto,[Vocal,Cons,Vocal1|W]),
                                                                !.

% a-pro-xi-mar-se; a-dri

posibilidadeseuskera(Vocal,[Vocal],[Vocal,ConsPareja,Cons_tr,Vocal2|W],Resto):-
                                                                 vocal_euskera(Vocal),
                                                                 consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                                               append([Vocal],Resto,[Vocal,ConsPareja,Cons_tr,Vocal2|W]),
                                                                !.



% a-e-re-o

posibilidadeseuskera(Vocal,[Vocal],[Vocal,Vocal2|W],Resto):-
                                                                hiatoeuskera(Vocal,[Vocal,Vocal2|_]),
                                                                 append([Vocal],Resto,[Vocal,Vocal2|W]),
                                                                 !.

% en

posibilidadeseuskera(Vocal,[Vocal,Cons_f],[Vocal,Cons_f],Resto):-
                                                                vocal_euskera(Vocal),
                                                                cons_feuskera(Cons_f),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f]),
                                                                !.
% itz
posibilidadeseuskera(Vocal,[Vocal,Cons_f, Cons_f2],[Vocal,Cons_f,Cons_f2],Resto):-
                                                                vocal_euskera(Vocal),
                                                                grupoFinal_eus(Cons_f,Cons_f2),
                                                                append([Vocal,Cons_f,Cons_f2],Resto,[Vocal,Cons_f,Cons_f2]),
                                                                !.
% intz
posibilidadeseuskera(Vocal,[Vocal,Cons_f, Cons_f2,Cons_f3],[Vocal,Cons_f,Cons_f2,Cons_f3],Resto):-
                                                                vocal_euskera(Vocal),
                                                                grupoFinal_eus(Cons_f,Cons_f2,Cons_f3),
                                                                append([Vocal,Cons_f,Cons_f2,Cons_f3],Resto,[Vocal,Cons_f,Cons_f2,Cons_f3]),
                                                                !.
                                                                
% is-la

posibilidadeseuskera(Vocal,[Vocal,Cons_f],[Vocal,Cons_f,ConsBis,VocalBis|W],Resto):-
                                                                 vocal_euskera(Vocal),
                                                                (cons_feuskera(Cons_f);pseudocons_feuskera(Cons_f,[Cons_f,ConsBis|W])),
                                                                conseuskera(ConsBis),
                                                                vocal_euskera(VocalBis),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f,ConsBis,VocalBis|W]),
                                                                !.


% antro, an-cla

posibilidadeseuskera(Vocal,[Vocal,Cons_f],[Vocal,Cons_f,ConsPareja,Cons_tr|W],Resto):-
                                                                 vocal_euskera(Vocal),
                                                                (cons_feuskera(Cons_f);pseudocons_feuskera(Cons_f,[Cons_f,ConsPareja|_])),
                                                                consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f,ConsPareja,Cons_tr|W]),
                                                                !.

%ins-tau-rar

posibilidadeseuskera(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal,ConsBis|W],Resto):-
                                                                 vocal_euskera(Vocal),
                                                                cons_feuskera(Cons_f),
                                                                 consFinalTotal(ConsFinalTotal),
                                                                conseuskera(ConsBis),
                                                                append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal,ConsBis|W]),
                                                                !.


% COMPROBAR
% ay, uy: unicas palabras encontradas con dipt que solo son eso: el dipt, pero meto todos los casos pq nada lo prohibe. No meto el dipt ascendente ni el doble pq no parece probable

posibilidadeseuskera(Vocal,[Vocal,Vocal2],[Vocal,Vocal2],Resto):-
                                                                diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                                                append([Vocal,Vocal2],Resto,[Vocal,Vocal2]),
                                                                !.

% ai-rar, au-lar, eusebio

posibilidadeseuskera(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,Cons,Vocal1|W],Resto):-
                                                                        diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                                                        conseuskera(Cons),
                                                                        vocal_euskera(Vocal1),
                                                                        append([Vocal,Vocal2],Resto,[Vocal,Vocal2,Cons,Vocal1|W]),
                                                                         !.


% ai-trar

posibilidadeseuskera(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,ConsPareja,Cons_tr|W],Resto):-
                                                                        diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                                                        consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr]),
                                                                        append([Vocal,Vocal2],Resto,[Vocal,Vocal2,ConsPareja,Cons_tr|W]),
                                                                         !.



% a-ma-ri-AIS

posibilidadeseuskera(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f],Resto):-
                                                                diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                                                cons_feuskera(Cons_f),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f]),
                                                                !.
% aitz

posibilidadeseuskera(Vocal,[Vocal,Vocal2,Cons_f,Cons_f2],[Vocal,Vocal2,Cons_f,Cons_f2],Resto):-
                                                                diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                                                grupoFinal_eus(Cons_f,Cons_f2),
                                                                append([Vocal,Vocal2,Cons_f,Cons_f2],Resto,[Vocal,Vocal2,Cons_f,Cons_f2]),
                                                                !.
% aintz

posibilidadeseuskera(Vocal,[Vocal,Vocal2,Cons_f,Cons_f2,Cons_f3],[Vocal,Vocal2,Cons_f,Cons_f2,Cons_f3],Resto):-
                                                                diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                                                grupoFinal_eus(Cons_f,Cons_f2,Cons_f3),
                                                                append([Vocal,Vocal2,Cons_f,Cons_f2,Cons_f3],Resto,[Vocal,Vocal2,Cons_f,Cons_f2,Cons_f3]),
                                                                !.

% AIS-lar

posibilidadeseuskera(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f,Cons|W],Resto):-
                                                                diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                                                (cons_feuskera(Cons_f);pseudocons_feuskera(Cons_f,[Cons_f,Cons|_])),
                                                                conseuskera(Cons),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f,Cons|W]),
                                                                !.

% aun-que

posibilidadeseuskera(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f,ConsPareja,Trabada|W],Resto):-
                                                                diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                                                (cons_feuskera(Cons_f);pseudocons_feuskera(Cons_f,[Cons_f,ConsPareja|_])),
                                                                consParejaeuskera(ConsPareja,[ConsPareja,Trabada|_]),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f,ConsPareja,Trabada|W]),
                                                                !.

% ea: hiato aislado. Solo encontrado "ea", pero meto todo porque nada lo impide

posibilidadeseuskera(Vocal,[Vocal],[Vocal,Vocal2],Resto):-
                                                               hiatoeuskera(Vocal,[Vocal,Vocal2|_]),
                                                                append([Vocal],Resto,[Vocal,Vocal2]),
                                                                !.

% posibilidades para CONS+VOCAL AISLADO

posibilidadeseuskera(Cons,[Cons,Vocal],[Cons,Vocal],Resto):-
                                          conseuskera(Cons),
                                          vocal_euskera(Vocal),
                                          append([Cons,Vocal],Resto,[Cons,Vocal]),
                                          !.
% hitz
posibilidadeseuskera(Cons,[Cons,Vocal,Cons_f,Cons_f2],[Cons,Vocal,Cons_f,Cons_f2],Resto):-
                                          grupoFinal_eus(Cons_f,Cons_f2),
                                          vocal_euskera(Vocal),
                                          append([Cons,Vocal,Cons_f,Cons_f2],Resto,[Cons,Vocal,Cons_f,Cons_f2]),
                                          !.

% hintz
posibilidadeseuskera(Cons,[Cons,Vocal,Cons_f,Cons_f2,Cons_f3],[Cons,Vocal,Cons_f,Cons_f2,Cons_f3],Resto):-
                                          grupoFinal_eus(Cons_f,Cons_f2,Cons_f3),
                                          vocal_euskera(Vocal),
                                          append([Cons,Vocal,Cons_f,Cons_f2,Cons_f3],Resto,[Cons,Vocal,Cons_f,Cons_f2,Cons_f3]),
                                          !.
                                          
                                          % blas
% krist
posibilidadeseuskera(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f,Cons_f2],[ConsPareja,Cons_tr,Vocal,Cons_f,Cons_f2],Resto):-
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocal_euskera(Vocal),
                                          grupoFinal_eus(Cons_f,Cons_f2),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f,Cons_f2],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,Cons_f2]),
                                          !.
                                          


% posibilidaddes para CONS+VOCAL EN PALABRA


posibilidadeseuskera(Cons,[Cons,Vocal],[Cons,Vocal,Cons1,Vocal1|W],Resto):-
                                          conseuskera(Cons),
                                          vocal_euskera(Vocal),
                                          conseuskera(Cons1),
                                          vocal_euskera(Vocal1),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Cons1,Vocal1|W]),
                                          !.

% posibilidaddes para CONS+VOCAL EN PALABRA: HIATO

posibilidadeseuskera(Cons,[Cons,Vocal],[Cons,Vocal,Vocal2|W],Resto):-
                                          conseuskera(Cons),
                                         hiatoeuskera(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Vocal2|W]),
                                          !.


% posibilidades para CONS+VOCAL+VOCAL AISLADO: HIATO

posibilidadeseuskera(Cons,[Cons,Vocal],[Cons,Vocal,Vocal2],Resto):-
                                          conseuskera(Cons),
                                          hiatoeuskera(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Vocal2]),
                                          !.

%% Caso particular: trabadas en mitad de palabra

% nu-blar  pa-dre

posibilidadeseuskera(Cons,[Cons,Vocal],[Cons,Vocal,ConsPareja,Cons_tr|W],Resto):-
                                          conseuskera(Cons),
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr]),
                                          vocal_euskera(Vocal),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,ConsPareja,Cons_tr|W]),
                                          !.


% CONS+VOCAL+CONS_FIN EN FINAL DE PALABRA: del-fin


posibilidadeseuskera(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f],Resto):-
                                          conseuskera(Cons),
                                          vocal_euskera(Vocal),
                                          cons_feuskera(Cons_f),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f]),
                                          !.

% CONS+VOCAL+CONS_F: pen-sar


posibilidadeseuskera(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f,Cons1,Vocal1|W],Resto):-
                                          conseuskera(Cons),
                                          vocal_euskera(Vocal),
                                          (cons_feuskera(Cons_f);pseudocons_feuskera(Cons_f,[Cons_f,Cons1|_])),
                                          conseuskera(Cons1),
                                          vocal_euskera(Vocal1),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f,Cons1,Vocal1|W]),
                                          !.

% CONS+VOCAL+CONS_F: pen-trar


posibilidadeseuskera(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f,ConsPareja,Cons_tr|W],Resto):-
                                          conseuskera(Cons),
                                          vocal_euskera(Vocal),
                                          (cons_feuskera(Cons_f);pseudocons_feuskera(Cons_f,[Cons_f,ConsTipo|W])),
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_treuskera]),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f,ConsPareja,Cons_tr|W]),
                                          !.

% CONS-truir


posibilidadeseuskera(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal],[Cons,Vocal,Cons_f,ConsFinalTotal,Cons1|W],Resto):-
                                          conseuskera(Cons),
                                          vocal_euskera(Vocal),
                                          cons_feuskera(Cons_f),
                                          consFinalTotal(ConsFinalTotal),
                                          conseuskera(Cons1),
                                          append([Cons,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,Cons1|W]),
                                          !.


% posibilidades para CONS+VOCAL+VOCAL AISLADO:

posibilidadeseuskera(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2],Resto):-
                                          conseuskera(Cons),
                                          diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2]),
                                          !.

% posibilidaddes para CONS+VOCAL+VOCAL EN PALABRA:

posibilidadeseuskera(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2,Cons1,Vocal1|W],Resto):-
                                          conseuskera(Cons),
                                          diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                          conseuskera(Cons1),
                                          vocal_euskera(Vocal1),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2,Cons1,Vocal1|W]),
                                          !.
% nai-blar; nai-trar

posibilidadeseuskera(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2,ConsPareja,Cons_tr|W],Resto):-
                                          conseuskera(Cons),
                                          diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2,ConsPareja,Cons_tr|W]),
                                          !.

% CONS+VOCAL+VOCAL+CONS_FIN con dipt descendente AISLADO: a-ma-bais

posibilidadeseuskera(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f],Resto):-
                                          conseuskera(Cons),
                                          diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                          cons_feuskera(Cons_f),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f]),
                                          !.


% CONS+VOCAL+VOCAL+CONS_F: DIPT DESCENDENTE mais-car



posibilidadeseuskera(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f,Cons1,Vocal1|W],Resto):-
                                          conseuskera(Cons),
                                          diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                          (cons_feuskera(Cons_f);pseudocons_feuskera(Cons_f,[Cons_f,Cons1|_])),
                                          conseuskera(Cons1),
                                          vocal_euskera(Vocal1),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f,Cons1,Vocal1|W]),
                                          !.

% CONS+VOCAL+VOCAL+CONS_F: DIPT DESCENDENTE mais-bla



posibilidadeseuskera(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f,ConsTipo,Cons_tr|W],Resto):-
                                          conseuskera(Cons),
                                          diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                          (cons_feuskera(Cons_f);pseudocons_feuskera(Cons_f,[Cons_f,ConsTipo|_])),
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsPareja,Cons_tr|W]),
                                          !.

% CONS+VOCAL+VOCAL+CONS_F*CONSFINALTOTAL: DIPT DESCENDENTE mains-bla



posibilidadeseuskera(Cons,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal,Cons1|W],Resto):-
                                          conseuskera(Cons),
                                          diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                          cons_feuskera(Cons_f),
                                          conseuskera(Cons1),
                                          append([Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal,Cons1|W]),
                                          !.

% bla

posibilidadeseuskera(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal],Resto):-
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocal_euskera(Vocal),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal]),
                                          !.

% gra-pa

posibilidadeseuskera(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,Cons,Vocal1|W],Resto):-
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocal_euskera(Vocal),
                                          vocal_euskera(Vocal1),
                                          conseuskera(Cons),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,Cons,Vocal1|W]),
                                          !.


% pro-e-mio

posibilidadeseuskera(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,Vocal2|W],Resto):-
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          hiatoeuskera(Vocal,[Vocal,Vocal2|_]),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2|W]),
                                          !.



% bla-bla

posibilidadeseuskera(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,ConsPareja1,Cons_treuskera1|W],Resto):-
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocal_euskera(Vocal),
                                          consParejaeuskera(ConsPareja1,[ConsPareja1,Cons_treuskera1|_]),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,ConsPareja1,Cons_treuskera1|W]),
                                          !.

% blas

posibilidadeseuskera(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f],Resto):-
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocal_euskera(Vocal),
                                          cons_feuskera(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f]),
                                          !.


% blan-ca

posibilidadeseuskera(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f,Cons1,Vocal1|W],Resto):-
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocal_euskera(Vocal),
                                          vocal_euskera(Vocal1),
                                          (cons_feuskera(Cons_f);pseudocons_feuskera(Cons_f,[Cons_f,Cons1|_])),
                                          conseuskera(Cons1),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,Cons1,Vocal1|W]),
                                          !.

% blan-bla

posibilidadeseuskera(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsPareja1,Cons_treuskera1|W],Resto):-
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocal_euskera(Vocal),
                                          (cons_feuskera(Cons_f);pseudocons_feuskera(Cons_f,[Cons_f,ConsTipo|_])),
                                          consParejaeuskera(ConsPareja1,[ConsPareja1,Cons_treuskera1|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsPareja1,Cons_treuskera1|W]),
                                          !.




% blans-ca; blans-bla

posibilidadeseuskera(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal,Cons1|W],Resto):-
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocal_euskera(Vocal),
                                          cons_feuskera(Cons_f),
                                          consFinalTotal(ConsFinalTotal),
                                          conseuskera(Cons1),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal,Cons1|W]),
                                          !.
                                          



% blau?

posibilidadeseuskera(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2],Resto):-
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2]),
                                          !.

% brai-le

posibilidadeseuskera(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons,Vocal1|W],Resto):-
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                          vocal_euskera(Vocal1),
                                          conseuskera(Cons),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons,Vocal1|W]),
                                          !.


% blau-bla

posibilidadeseuskera(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2,ConsParejaBis,Cons_treuskeraBis|W],Resto):-
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                          consParejaeuskera(ConsParejaBis,[ConsParejaBis,Cons_treuskeraBis|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,ConsParejaBis,Cons_treuskeraBis|W]),
                                          !.

% blaun

posibilidadeseuskera(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto):-
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                          cons_feuskera(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f]),
                                       !.

% brains-le

posibilidadeseuskera(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal,Cons|W],Resto):-
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                          conseuskera(Cons),
                                          consFinalTotal(ConsFinalTotal),
                                          (cons_feuskera(Cons_f);pseudocons_feuskera(Cons_f,[Cons_f,Cons|_])),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal,Cons|W]),
                                          !.

% brain-le

posibilidadeseuskera(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,Cons|W],Resto):-
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoeuskera(Vocal,[Vocal,Vocal2|_]),
                                          conseuskera(Cons),
                                          (cons_feuskera(Cons_f);pseudocons_feuskera(Cons_f,[Cons_f,Cons|_])),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,Cons|W]),
                                          !.






% Triptongo

posibilidadeseuskera(Cons,[Cons,Vocal,Vocal2,Vocal3],[Cons,Vocal,Vocal2,Vocal3],Resto):-
                                          conseuskera(Cons),
                                          triptongoeuskera(Vocal,[Vocal,Vocal2,Vocal3]),
                                          append([Cons,Vocal,Vocal2,Vocal3],Resto,[Cons,Vocal,Vocal2,Vocal3]),
                                          !.
                                          

% Triptongo en cons_treuskera

posibilidadeseuskera(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],Resto):-
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                           triptongoeuskera(Vocal,[Vocal,Vocal2,Vocal3]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3]),
                                          !.

% Triptongo +cons final

posibilidadeseuskera(Cons,[Cons,Vocal,Vocal2,Vocal3,Cons_f],[Cons,Vocal,Vocal2,Vocal3,Cons_f],Resto):-
                                          conseuskera(Cons),
                                           triptongoeuskera(Vocal,[Vocal,Vocal2,Vocal3]),
                                          cons_feuskera(Cons_f),
                                          append([Cons,Vocal,Vocal2,Vocal3,Cons_f],Resto,[Cons,Vocal,Vocal2,Vocal3,Cons_f]),
                                          !.

% Triptongo en cons_treuskera + cons final

posibilidadeseuskera(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],Resto):-
                                          consParejaeuskera(ConsPareja,[ConsPareja,Cons_tr|_]),
                                           triptongoeuskera(Vocal,[Vocal,Vocal2,Vocal3]),
                                          cons_feuskera(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f]),
                                          !.


% Triptongo en mitad de palabra

posibilidadeseuskera(Cons,[Cons,Vocal,Vocal2,Vocal3],[Cons,Vocal,Vocal2,Vocal3|W],Resto):-
                                          conseuskera(Cons),
                                          triptongoeuskera(Vocal,[Vocal,Vocal2,Vocal3]),
                                          append([Cons,Vocal,Vocal2,Vocal3],Resto,[Cons,Vocal,Vocal2,Vocal3|W]),
                                          !.