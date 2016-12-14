% Autor:
% Datum: 22/11/2009




% X tiene que ir "" para que prolog lo lea como una lista de codigos ANSII. El predicado español/1 dice que algo es español si hacemos el procedimiento lista/2 y el comprobar_palabras/1.




frances(X,Restos,Origen):-
                           frances(X,Restos),
                           origen_etimologicoFrances(Restos,Origen).

origen_etimologicoFrances([],[]).

origen_etimologicoFrances([Restos|MasRestos],[Origen|MasOrigen]):-
                                                 atom_chars(Restos,Deletreo),
                                                 combinacionesFrances(Restos,Deletreo,Origen),
                                                 !,
                                                 origen_etimologicoFrances(MasRestos,MasOrigen).



combinacionesFrances(Restos,[Cons,y|W],griego(Restos)):-
                                                           consFrances(Cons).
                                                           
combinacionesFrances(Restos,[p,h,t,Vocal|W],griego(Restos)):-
                                                           vocalFrances(Vocal).
                                                           
combinacionesFrances(Restos,[c,h,r,Vocal|W],griego(Restos)):-
                                                           vocalFrances(Vocal).
                                                           
combinacionesFrances(Restos,[J],desconocido(Restos)).





combinacionesFrances(Restos,[X,Y|W],Origen):-
                                       combinacionesFrances(Restos,[Y|W],Origen).



frances(X,Restos):-
               transcribir(X,W),
               !,
               lista(W,Y),
               comprobar_palabrasFrances(Y,N),
               recuperar_descartesFrances(N,Restos).

recuperar_descartesFrances([],[]).

recuperar_descartesFrances([N|M],[Restos|MasRestos]):-
               atom_codes(Restos,N),
               recuperar_descartesFrances(M,MasRestos).


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
                                                    signPunt_Frances(SignPunt),
                                                    Sublista=[],
                                                    corte(Resto, Resfinal, Resprov, []).

% Reglas recursivas
corte([SignPunt|Resto], Resfinal, Resprov, Sublista):-
                                                       signPunt_Frances(SignPunt),
                                                       append(Resprov, [Sublista], Res),
                                                       corte(Resto, Resfinal, Res, []).

corte([Cab|Resto], Resfinal, Resprov, Sublista):-
                                                   not(signPunt_Frances(Cab)),
                                                   append(Sublista, [Cab], Prov),
                                                   corte(Resto, Resfinal, Resprov, Prov).

% Signos de puntuacion considerados

signPunt_Frances(32).
signPunt_Frances(33).
signPunt_Frances(34).
signPunt_Frances(38).
signPunt_Frances(40).
signPunt_Frances(41).
signPunt_Frances(44).
signPunt_Frances(45).
signPunt_Frances(46).
signPunt_Frances(47).
signPunt_Frances(58).
signPunt_Frances(59).
signPunt_Frances(63).
signPunt_Frances(132).
signPunt_Frances(147).
signPunt_Frances(148).
signPunt_Frances(161).
signPunt_Frances(191).
signPunt_Frances(171).
signPunt_Frances(187).
signPunt_Frances(8220).
signPunt_Frances(8221).
signPunt_Frances(8211).
signPunt_Frances(91).
signPunt_Frances(93).
signPunt_Frances(92).
signPunt_Frances(8212).
signPunt_Frances(60).
signPunt_Frances(62).
signPunt_Frances(8230).

signPunt_Frances(X):-
             X>47,
             X<58.







comprobar_palabrasFrances([],[]).


comprobar_palabrasFrances([Z|W],Sobras):-
                       silabearFrances(Z,F),
                       comprobar_palabrasFrances(W,Sobras),
                       !.

comprobar_palabrasFrances([Z|W],[Z|Sobras]):-
                       not(silabearFrances(Z,F)),
                       comprobar_palabrasFrances(W,Sobras),
                       !.








% El proceso de silabeo se incia con la transformacion de lista de codigo ANSII a letras sueltas para poder silabear, y lo manda ya en forma de letras al proceso auxiliar de silabeo


silabearFrances(X,Z):-
               atom_codes(Y,X),
               atom_chars(Y,W),
               olfatear_frances(W),
               silabear_auxFrances(W,Z).


olfatear_frances([]).

olfatear_frances([a,ü|T]):-
                           !,
                           fail.
                           

olfatear_frances([X|Y]):-
                 olfatear_frances(Y).

silabeoFrances(X,Z):-
               silabear_auxFrances(X,Z).

% El proceso auxiliar de silabeo

silabear_auxFrances([],[]).

silabear_auxFrances([Z|T],[Silaba|Silaba_resto]):-
                 posibilidadesFrances(Z,Silabeo,[Z|T],Resto),
                 atom_chars(Silaba,Silabeo),
                 silabear_auxFrances(Resto,Silaba_resto).









% Letras en español




vocal_Frances(i).
vocal_Frances(u).
vocal_Frances(a).
vocal_Frances(e).
vocal_Frances(o).

vocalFrances(C):-
                 vocal_Frances(C),
                 !.

vocalFrances(C):-
                 letras_equivalentesFrances(A,C),
                 vocal_Frances(A),
                 !.


letras_equivalentesFrances(e,é).



letras_equivalentesFrances(e,ë).
letras_equivalentesFrances(i,ï).
letras_equivalentesFrances(o,ü).


letras_equivalentesFrances(a,à).
letras_equivalentesFrances(e,è).
letras_equivalentesFrances(u,ù).

letras_equivalentesFrances(a,â).
letras_equivalentesFrances(e,ê).
letras_equivalentesFrances(i,î).
letras_equivalentesFrances(o,ô).
letras_equivalentesFrances(u,û).

letras_equivalentesFrances(A,A).


cons_fFrances(s).
cons_fFrances(l).
cons_fFrances(t).
cons_fFrances(x).
cons_fFrances(r).
cons_fFrances(c).
cons_fFrances(n).
cons_fFrances(f).
cons_fFrances(p).

consFinalTotal_frances(p,[m,p,t]).
consFinalTotal_frances(c,[n,c,t]).
consFinalTotal_frances(s,[r,s,Cons]).

consFinalTotal_frances(s,[n,s,Cons]):-
consFrances(Cons).



refinalTotal_frances(s,[n,s]).
refinalTotal_frances(t,[n,t]).
refinalTotal_frances(t,[s,t]).
refinalTotal_frances(t,[c,t]).
refinalTotal_frances(t,[r,t]).
refinalTotal_frances(c,[n,c]).
refinalTotal_frances(p,[m,p]).
refinalTotal_frances(d,[n,d]).
refinalTotal_frances(g,[n,g]).

refinalTotal_frances(s,[Cons,s]):-
                                  cons_fFrances(Cons).

refinalTotal_frances(s,[Cons,Cons2,s]):-
                                        refinalTotal_frances(Cons2,[Cons,Cons2]).



bilabial(p).
bilabial(b).

liquida(r).
liquida(l).



%% Consontantes emparejadas

consParejaFrances(A,C):-
                        consParejaFrances_1(A,C);
                        consParejaFrances_2(A,C).

% b

consParejaFrances_1(b,[b,Liquida|_]):-
                              liquida(Liquida).

% f

consParejaFrances_1(f,[f,Liquida|_]):-
                              liquida(Liquida).

% g

consParejaFrances_1(g,[g,Liquida|_]):-
                              liquida(Liquida).

% c

consParejaFrances_1(c,[c,Liquida|_]):-
                              liquida(Liquida).

% g

consParejaFrances_1(p,[p,Liquida|_]):-
                              liquida(Liquida).

% d

consParejaFrances_1(d,[d,r|_]).

% t

consParejaFrances_1(t,[t,r|_]).

% vr

consParejaFrances_1(v,[v,r|_]).


% ch

consParejaFrances_1(c,[c,h|_]).

% ph

consParejaFrances_1(p,[p,h|_]).

% th

consParejaFrances_1(t,[t,h|_]).

% gn

consParejaFrances_1(g,[g,n|_]).


% qu

consParejaFrances_1(q,[q,u|_]).

% s liquida

consParejaFrances_2(s,[s,OclusivaSorda|_]):-
oclusiva_sordaFrances(OclusivaSorda).

oclusiva_sordaFrances(p).
oclusiva_sordaFrances(c).
oclusiva_sordaFrances(t).



%% Vocales emparejadas: DIPTONGOS

diptongoFrances(Vocal_a,[Vocal_a,Vocal_i|_]):-
                                        letras_equivalentesFrances(a,Vocal_a),
                                        letras_equivalentesFrances(i,Vocal_i).
                                        
diptongoFrances(Vocal_a,[Vocal_a,Vocal_u|_]):-
                                        letras_equivalentesFrances(a,Vocal_a),
                                        letras_equivalentesFrances(u,Vocal_u).

diptongoFrances(Vocal_e,[Vocal_e,Vocal_i|_]):-
                                         letras_equivalentesFrances(e,Vocal_e),
                                         letras_equivalentesFrances(i,Vocal_i)                                      .
diptongoFrances(Vocal_e,[Vocal_e,Vocal_u|_]):-
                                         letras_equivalentesFrances(e,Vocal_e),
                                         letras_equivalentesFrances(u,Vocal_u).

diptongoFrances(Vocal_i,[Vocal_i,Vocal_a|_]):-
                                   letras_equivalentesFrances(a,Vocal_a),
                                   letras_equivalentesFrances(i,Vocal_i).


diptongoFrances(Vocal_i,[Vocal_i,Vocal_e|_]):-
                                  letras_equivalentesFrances(e,Vocal_e),
                                  letras_equivalentesFrances(i,Vocal_i).

diptongoFrances(Vocal_i,[Vocal_i,Vocal_o|_]):-
                                  letras_equivalentesFrances(o,Vocal_o),
                                  letras_equivalentesFrances(i,Vocal_i).

diptongoFrances(Vocal_i,[Vocal_i,Vocal_u|_]):-
                                  letras_equivalentesFrances(u,Vocal_u),
                                  letras_equivalentesFrances(i,Vocal_i).

diptongoFrances(Vocal_o,[Vocal_o,Vocal_i|_]):-
                                        letras_equivalentesFrances(o,Vocal_o),
                                        letras_equivalentesFrances(i,Vocal_i).
                                        
diptongoFrances(Vocal_o,[Vocal_o,Vocal_u|_]):-
                                        letras_equivalentesFrances(o,Vocal_o),
                                        letras_equivalentesFrances(u,Vocal_u).


diptongoFrances(Vocal_u,[Vocal_u,Vocal_a|_]):-
                                     letras_equivalentesFrances(a,Vocal_a),
                                     letras_equivalentesFrances(u,Vocal_u).

diptongoFrances(Vocal_u,[Vocal_u,Vocal_e|_]):-
                                   letras_equivalentesFrances(e,Vocal_e),
                                     letras_equivalentesFrances(u,Vocal_u).
                                     
diptongoFrances(Vocal_u,[Vocal_u,Vocal_i|_]):-
                                   letras_equivalentesFrances(i,Vocal_i),
                                     letras_equivalentesFrances(u,Vocal_u).
                                     
diptongoFrances(Vocal_u,[Vocal_u,Vocal_o|_]):-
                                    letras_equivalentesFrances(o,Vocal_o),
                                     letras_equivalentesFrances(u,Vocal_u).
                                     
diptongoFrances(ü,[ü,Vocal_e|_]):-
                                    letras_equivalentesFrances(e,Vocal_e).

hiatoFrances(Vocal_a,[Vocal_a,Vocal_e|_]):-
                               letras_equivalentesFrances(a,Vocal_a),
                               letras_equivalentesFrances(e,Vocal_e).


hiatoFrances(Vocal_a,[Vocal_a,Vocal_o|_]):-
                               letras_equivalentesFrances(a,Vocal_a),
                               letras_equivalentesFrances(o,Vocal_o).


hiatoFrances(Vocal_e,[Vocal_e,Vocal_a|_]):-
                               letras_equivalentesFrances(e,Vocal_e),
                               letras_equivalentesFrances(a,Vocal_a).

hiatoFrances(Vocal_e,[Vocal_e,Vocal_e|_]):-
                               letras_equivalentesFrances(e,Vocal_e),
                               letras_equivalentesFrances(e,Vocal_e).

hiatoFrances(Vocal_e,[Vocal_e,Vocal_o|_]):-
                               letras_equivalentesFrances(e,Vocal_e),
                               letras_equivalentesFrances(o,Vocal_o).

hiatoFrances(ï,[ï,Vocal|_]):-
                             vocalFrances(Vocal).
                             
hiatoFrances(é,[é,e|_]).
                             
hiatoFrances(Vocal,[Vocal,ÿ|_]):-
                             vocalFrances(Vocal).

hiatoFrances(Vocal_o,[Vocal_o,Vocal_a|_]):-
                               letras_equivalentesFrances(o,Vocal_o),
                               letras_equivalentesFrances(a,Vocal_a).

hiatoFrances(Vocal_o,[Vocal_o,Vocal_e|_]):-
                               letras_equivalentesFrances(o,Vocal_o),
                               letras_equivalentesFrances(e,Vocal_e).
                               
triptongoFrances(i,[i,e,u]).
triptongoFrances(o,[o,u,Vocal_e]):-
                                    letras_equivalentesFrances(e,Vocal_e).



/*
triptongoFrances(i,[i,Vocal_a,i]):-
                                             letras_equivalentesFrances(a,Vocal_a).

triptongoFrances(i,[i,Vocal_e,i]):-
                                             letras_equivalentesFrances(e,Vocal_e).

triptongoFrances(u,[u,Vocal_a,y]):-
                                               letras_equivalentesFrances(a,Vocal_a).

triptongoFrances(u,[u,á,i]).

triptongoFrances(u,[u,e,y]).

*/
















%% Consontantes pseudofinales: contextos

% Letra m

pseudocons_fFrances(m,[m,Bilabial|_]):-
                                  bilabial(Bilabial).


pseudocons_fFrances(m,[m,n|_]).

pseudocons_fFrances(m,[m,m|_]).


% Letra p

dentalFrances(t).
dentalFrances(c).

pseudocons_fFrances(p,[p,t|_]).
pseudocons_fFrances(p,[p,c|_]).
pseudocons_fFrances(p,[p,p|_]).

% Letra g

pseudocons_fFrances(g,[g,m|_]).

% Letra x

pseudocons_fFrances(x,[x,t|_]).


% Letra c

pseudocons_fFrances(c,[c,c|_]).



pseudocons_fFrances(c,[c,t|_]).




consFrances(b).
consFrances(c).
consFrances(d).
consFrances(f).
consFrances(g).
consFrances(h).
consFrances(j).
consFrances(k).
consFrances(l).
consFrances(m).
consFrances(n).
consFrances(p).
consFrances(r).
consFrances(s).
consFrances(t).
consFrances(v).
consFrances(x).
consFrances(y).
consFrances(z).
consFrances(ç).
consFrances(i).

%% PREFIJOS



posibilidadesFrances(s,[s,u,b,s],[s,u,b,s,Cons|W],Resto):-
                                                                not(vocalFrances(Cons)),
                                                                append([s,u,b,s],Resto,[s,u,b,s,Cons|W]),
                                                                !.




posibilidadesFrances(s,[s,u,b],[s,u,b,Cons|W],Resto):-
                                                                not(vocalFrances(Cons)),
                                                                append([s,u,b],Resto,[s,u,b,Cons|W]),
                                                              !.




posibilidadesFrances(o,[o,b,s],[o,b,s,Cons|W],Resto):-
                                                                not(vocalFrances(Cons)),
                                                                append([o,b,s],Resto,[o,b,s,Cons|W]),
                                                                 !.





posibilidadesFrances(o,[o,b],[o,b,Cons|W],Resto):-
                                                                not(vocalFrances(Cons)),
                                                                not(consParejaFrances(b,[b,Cons])),
                                                                append([o,b],Resto,[o,b,Cons|W]),
                                                                 !.


posibilidadesFrances(a,[a,b],[a,b,Cons|W],Resto):-
                                                                not(vocalFrances(Cons)),
                                                                not(consParejaFrances(b,[b,Cons])),
                                                                append([a,b],Resto,[a,b,Cons|W]),
                                                                 !.




posibilidadesFrances(a,[a,b,s],[a,b,s,Cons|W],Resto):-
                                                                not(vocalFrances(Cons)),
                                                                                                                                append([a,b,s],Resto,[a,b,s,Cons|W]),
                                                                !.










%ex

posibilidadesFrances(e,[e,x],[e,x,Cons|W],Resto):-
                                                                consFrances(Cons),
                                                                Cons\=i,
                                                                append([e,x],Resto,[e,x,Cons|W]),
                                                                 !.


% antro, an-cla

posibilidadesFrances(Vocal,[Vocal,Cons_f],[Vocal,Cons_f,ConsPareja,Cons_tr,Vocal2|W],Resto):-
                                                                 vocalFrances(Vocal),
                                                                (cons_fFrances(Cons_f);pseudocons_fFrances(Cons_f,[Cons_f,ConsPareja|_])),
                                                                not(consParejaFrances_1(Cons_f,[Cons_f,ConsPareja])),
                                                                consParejaFrances_1(ConsPareja,[ConsPareja,Cons_tr]),
                                                                vocalFrances(Vocal2),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f,ConsPareja,Cons_tr,Vocal2|W]),
                                                                !.
                                                                
% blans-ca

posibilidadesFrances(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                          consParejaFrances(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalFrances(Vocal),
                                          consFinalTotal_frances(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                          consFrances(Cons1),
                                          not(consParejaFrances_1(ConsFinalTotal,[ConsFinalTotal,ConsSiguiente])),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W]),
                                          !.
                                          

% ins-tru



posibilidadesFrances(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,Cons_tr|W],Resto):-
                                          vocalFrances(Vocal),
                                          not(consParejaFrances_1(ConsFinalTotal,[ConsFinalTotal,ConsSiguiente])),
                                          consParejaFrances_1(ConsSiguiente,[ConsSiguiente,Cons_tr]),
                                          consFinalTotal_frances(ConsFinalTotal,[Cons_f,ConsFinalTotal]),
                                          append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,Cons_tr|W]),
                                          !.
                                                                



                                                                
%cients

posibilidadesFrances(Cons,[Cons,Vocal,Vocal2,Cons_f,Refinal,Final],[Cons,Vocal,Vocal2,Cons_f,Refinal,Final],Resto):-
                                          consFrances(Cons),
                                          diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                          refinalTotal_frances(Final,[Cons_f,Refinal,Final]),
                                          append([Cons,Vocal,Vocal2,Cons_f,Refinal,Final],Resto,[Cons,Vocal,Vocal2,Cons_f,Refinal,Final]),
                                          !.
% rioll

posibilidadesFrances(Cons,[Cons,Vocal,Vocal2,Cons_f,ConsRefinal],[Cons,Vocal,Vocal2,Cons_f,ConsRefinal],Resto):-
                                          consFrances(Cons),
                                          diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                          refinalTotal_frances(ConsRefinal,[Cons_f,ConsRefinal]),
                                          append([Cons,Vocal,Vocal2,Cons_f,ConsRefinal],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsRefinal]),
                                          !.

% riolls

posibilidadesFrances(Cons,[Cons,Vocal,Vocal2,Cons_f,ConsRefinal,ConsF],[Cons,Vocal,Vocal2,Cons_f,ConsRefinal,ConsF],Resto):-
                                          consFrances(Cons),
                                          diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                          refinalTotal_frances(ConsF,[Cons_f,ConsRefinal,ConsF]),
                                          append([Cons,Vocal,Vocal2,Cons_f,ConsRefinal,ConsF],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsRefinal,ConsF]),
                                          !.


posibilidadesFrances(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2],Resto):-
                                          consFrances(Cons),
                                          diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2]),
                                          !.



% posibilidaddes para CONS+VOCAL EN PALABRA


posibilidadesFrances(Cons,[Cons,Vocal],[Cons,Vocal,Cons1,Vocal1|W],Resto):-
                                          consFrances(Cons),
                                          vocalFrances(Vocal),
                                          consFrances(Cons1),
                                          vocalFrances(Vocal1),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Cons1,Vocal1|W]),
                                          !.

%tirps

posibilidadesFrances(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal,Refinal],[Cons,Vocal,Cons_f,ConsFinalTotal,Refinal],Resto):-
                                                                  consFrances(Cons),
                                                                 vocalFrances(Vocal),
                                                               refinalTotal_frances(Refinal,[Cons_f,ConsFinalTotal,Refinal]),
                                                                append([Cons,Vocal,Cons_f,ConsFinalTotal,Refinal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,Refinal]),
                                                                !.



%mels

posibilidadesFrances(Cons,[Cons,Vocal,Cons_f,Refinal],[Cons,Vocal,Cons_f,Refinal],Resto):-
                                                                   vocalFrances(Vocal),
                                                               refinalTotal_frances(Refinal,[Cons_f,Refinal]),
                                                                append([Cons,Vocal,Cons_f,Refinal],Resto,[Cons,Vocal,Cons_f,Refinal]),
                                                                !.

%ments

posibilidadesFrances(Cons,[Cons,Vocal,Cons_f,Refinal,ConsF],[Cons,Vocal,Cons_f,Refinal,ConsF],Resto):-
                                                                   vocalFrances(Vocal),
                                                               refinalTotal_frances(Refinal,[Cons_f,Refinal,ConsF]),
                                                                append([Cons,Vocal,Cons_f,Refinal,ConsF],Resto,[Cons,Vocal,Cons_f,Refinal,ConsF]),
                                                                !.


% post-quam



posibilidadesFrances(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal],[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,Cons_tr|W],Resto):-
                                          consFrances(Cons),
                                          vocalFrances(Vocal),
                                          consParejaFrances_1(ConsSiguiente,[ConsSiguiente,Cons_tr]),
                                          consFinalTotal_frances(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                          append([Cons,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,Cons_tr|W]),
                                          !.

% trans-pa



posibilidadesFrances(Cons,[Cons,Cons_tr,Vocal,Cons_f,ConsFinalTotal],[Cons,Cons_tr,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,VocalBis|W],Resto):-
                                          vocalFrances(Vocal),
                                          consParejaFrances(Cons,[Cons,Cons_tr]),
                                          consFinalTotal_frances(ConsFinalTotal,[Cons_f,ConsFinalTotal]),
                                          consFrances(ConsSiguiente),
                                          vocalFrances(VocalBis),
                                          append([Cons,Cons_tr,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Cons_tr,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,VocalBis|W]),
                                          !.



% Au-un

posibilidadesFrances(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,Vocal3|W],Resto):-
                                                                diptongoFrances(Vocal,[Vocal,Vocal2]),
                                                                hiatoFrances(Vocal2,[Vocal2,Vocal3]),
                                                                append([Vocal,Vocal2],Resto,[Vocal,Vocal2,Vocal3|W]),
                                                                !.


%nunc

posibilidadesFrances(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal],[Cons,Vocal,Cons_f,ConsFinalTotal],Resto):-
                                                                  consFrances(Cons),
                                                                 vocalFrances(Vocal),
                                                                consFinalTotal_frances(ConsFinalTotal,[Cons_f,ConsFinalTotal]),
                                                                append([Cons,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal]),
                                                                !.

%tirps

posibilidadesFrances(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal,Refinal],[Cons,Vocal,Cons_f,ConsFinalTotal,Refinal],Resto):-
                                                                  consFrances(Cons),
                                                                 vocalFrances(Vocal),
                                                              refinalTotal_frances(Refinal,[Cons_f,ConsFinalTotal,Refinal]),
                                                                append([Cons,Vocal,Cons_f,ConsFinalTotal,Refinal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,Refinal]),
                                                                !.




% brai-le

posibilidadesFrances(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2,ConsBis,VocalBis|W],Resto):-
                                          consParejaFrances(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                          consFrances(ConsBis),
                                          vocalFrances(VocalBis),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,ConsBis,VocalBis|W]),
                                          !.

% post-quam



posibilidadesFrances(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal],[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,Cons_tr|W],Resto):-
                                          consFrances(Cons),
                                          vocalFrances(Vocal),
                                          consParejaFrances_1(ConsSiguiente,[ConsSiguiente,Cons_tr]),
                                          consFinalTotal_frances(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                          append([Cons,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,Cons_tr|W]),
                                          !.

% posibilidades para CONS+VOCAL AISLADO

posibilidadesFrances(Cons,[Cons,Vocal],[Cons,Vocal],Resto):-
                                          consFrances(Cons),
                                          vocalFrances(Vocal),
                                          append([Cons,Vocal],Resto,[Cons,Vocal]),
                                          !.

% posibilidaddes para CONS+VOCAL EN PALABRA


posibilidadesFrances(Cons,[Cons,Vocal],[Cons,Vocal,Cons1,Vocal1|W],Resto):-
                                          consFrances(Cons),
                                          vocalFrances(Vocal),
                                          consFrances(Cons1),
                                          vocalFrances(Vocal1),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Cons1,Vocal1|W]),
                                          !.

% CONS+VOCAL+CONS_F: pen-sar. OJO: para el latin es importante que esta regla
% aparezca aqui. Res-su-co; al ir la s tb como vocal, si cambiamos el orden, lo
% interpreta como re-s-su-co.


posibilidadesFrances(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f,Cons1,Vocal1|W],Resto):-
                                          consFrances(Cons),
                                          vocalFrances(Vocal),
                                          (cons_fFrances(Cons_f);pseudocons_fFrances(Cons_f,[Cons_f,Cons1|_])),
                                          consFrances(Cons1),
                                          vocalFrances(Vocal1),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f,Cons1,Vocal1|W]),
                                          !.


% posibilidades para CONS+VOCAL+VOCAL AISLADO:

posibilidadesFrances(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2],Resto):-
                                          consFrances(Cons),
                                          diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2]),
                                          !.



% posibilidaddes para CONS+VOCAL+VOCAL EN PALABRA:

posibilidadesFrances(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2,Cons1,Vocal1|W],Resto):-
                                          consFrances(Cons),
                                          diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                          consFrances(Cons1),
                                          vocalFrances(Vocal1),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2,Cons1,Vocal1|W]),
                                          !.



% posibilidaddes para CONS+VOCAL EN PALABRA: HIATO

posibilidadesFrances(Cons,[Cons,Vocal],[Cons,Vocal,Vocal2|W],Resto):-
                                          consFrances(Cons),
                                          hiatoFrances(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Vocal2|W]),
                                          !.



% nai-blar; nai-trar

posibilidadesFrances(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2,ConsPareja,Cons_tr|W],Resto):-
                                          consFrances(Cons),
                                          diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                          consParejaFrances_1(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2,ConsPareja,Cons_tr|W]),
                                          !.

% CONS+VOCAL+VOCAL+CONS_FIN con dipt descendente AISLADO: a-ma-bais

posibilidadesFrances(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f],Resto):-
                                          consFrances(Cons),
                                          diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                          cons_fFrances(Cons_f),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f]),
                                          !.



% CONS+VOCAL+VOCAL+CONS_F: DIPT DESCENDENTE mais-car



posibilidadesFrances(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f,Cons1,Vocal1|W],Resto):-
                                          consFrances(Cons),
                                          diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                          (cons_fFrances(Cons_f);pseudocons_fFrances(Cons_f,[Cons_f,Cons1|_])),
                                          consFrances(Cons1),
                                          vocalFrances(Vocal1),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f,Cons1,Vocal1|W]),
                                          !.

% CONS+VOCAL+VOCAL+CONS_F: DIPT DESCENDENTE mais-bla



posibilidadesFrances(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f,ConsTipo,Cons_tr|W],Resto):-
                                          consFrances(Cons),
                                          diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                          (cons_fFrances(Cons_f);pseudocons_fFrances(Cons_f,[Cons_f,ConsTipo|_])),
                                          consParejaFrances_1(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsPareja,Cons_tr|W]),
                                          !.


% CONS+VOCAL+VOCAL+CONS_F*CONSFINALTOTAL: DIPT DESCENDENTE mains-bla



posibilidadesFrances(Cons,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                          consFrances(Cons),
                                          diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                          consFinalTotal_frances(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                          append([Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal,ConsSiguiente|_]),
                                          !.


%panc-to

posibilidadesFrances(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal],[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                                                  consFrances(Cons),
                                                                 vocalFrances(Vocal),
                                                                 consFinalTotal_frances(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                                                append([Cons,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W]),
                                                                !.



%neins

posibilidadesFrances(Cons,[Cons,Vocal,Vocal2,Cons_f,Refinal],[Cons,Vocal,Vocal2,Cons_f,Refinal],Resto):-
                                          consFrances(Cons),
                                          diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                          refinalTotal_frances(Refinal,[Cons_f,Refinal]),
                                          append([Cons,Vocal,Vocal2,Cons_f,Refinal],Resto,[Cons,Vocal,Vocal2,Cons_f,Refinal]),
                                          !.

%cients

posibilidadesFrances(Cons,[Cons,Vocal,Vocal2,Cons_f,Refinal,Final],[Cons,Vocal,Vocal2,Cons_f,Refinal,Final],Resto):-
                                          consFrances(Cons),
                                          diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                          refinalTotal_frances(Refinal,[Cons_f,Refinal,Final]),
                                          append([Cons,Vocal,Vocal2,Cons_f,Refinal,Final],Resto,[Cons,Vocal,Vocal2,Cons_f,Refinal,Final]),
                                          !.

% CONS+VOCAL+CONS_F: pen-trar


posibilidadesFrances(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f,ConsPareja,Cons_tr|W],Resto):-
                                          consFrances(Cons),
                                          vocalFrances(Vocal),
                                          (cons_fFrances(Cons_f);pseudocons_fFrances(Cons_f,[Cons_f,ConsPareja|W])),
                                          consParejaFrances_1(ConsPareja,[ConsPareja,Cons_tr]),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f,ConsPareja,Cons_tr|W]),
                                          !.




% nu-blar  pa-dre

posibilidadesFrances(Cons,[Cons,Vocal],[Cons,Vocal,ConsPareja,Cons_tr|W],Resto):-
                                          consFrances(Cons),
                                          consParejaFrances_1(ConsPareja,[ConsPareja,Cons_tr]),
                                          vocalFrances(Vocal),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,ConsPareja,Cons_tr|W]),
                                          !.


% CONS+VOCAL+CONS_FIN EN FINAL DE PALABRA: del-fin


posibilidadesFrances(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f],Resto):-
                                          consFrances(Cons),
                                          vocalFrances(Vocal),
                                          cons_fFrances(Cons_f),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f]),
                                          !.







%%% silabas que empiezan por vocal

%% OJO PARA TODO EL CÓDIGO QUE SIGUE: OI, AU y AI son los unicos dipt que forman dipt tal cual. Los desmas son extranj: comprobar

% a


posibilidadesFrances(Vocal,[Vocal],[Vocal],Resto):-
                                                                vocalFrances(Vocal),
                                                                append([Vocal],Resto,[Vocal]),
                                                                !.
% a-la

posibilidadesFrances(Vocal,[Vocal],[Vocal,Cons,Vocal1|W],Resto):-
                                                                vocalFrances(Vocal),
                                                                consFrances(Cons),
                                                                vocalFrances(Vocal1),
                                                                append([Vocal],Resto,[Vocal,Cons,Vocal1|W]),
                                                                !.
%anc-to

posibilidadesFrances(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                                                 vocalFrances(Vocal),
                                                                 consFinalTotal_frances(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                                                append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W]),
                                                                !.








% a-pro-xi-mar-se; a-dri

posibilidadesFrances(Vocal,[Vocal],[Vocal,ConsPareja,Cons_tr,Vocal2|W],Resto):-
                                                                 vocalFrances(Vocal),
                                                                 consParejaFrances_1(ConsPareja,[ConsPareja,Cons_tr|_]),
                                                               append([Vocal],Resto,[Vocal,ConsPareja,Cons_tr,Vocal2|W]),
                                                                !.





% ai-rar, au-lar, eusebio

posibilidadesFrances(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,Cons,Vocal1|W],Resto):-
                                                                        diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                                                        consFrances(Cons),
                                                                        vocalFrances(Vocal1),
                                                                        append([Vocal,Vocal2],Resto,[Vocal,Vocal2,Cons,Vocal1|W]),
                                                                         !.


% ai-trar

posibilidadesFrances(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,ConsPareja,Cons_tr|W],Resto):-
                                                                        diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                                                        consParejaFrances_1(ConsPareja,[ConsPareja,Cons_tr]),
                                                                        append([Vocal,Vocal2],Resto,[Vocal,Vocal2,ConsPareja,Cons_tr|W]),
                                                                         !.




% a-ma-ri-AIS

posibilidadesFrances(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f],Resto):-
                                                                diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                                                cons_fFrances(Cons_f),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f]),
                                                                !.


% AIS-lar

posibilidadesFrances(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f,Cons|W],Resto):-
                                                                diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                                                (cons_fFrances(Cons_f);pseudocons_fFrances(Cons_f,[Cons_f,Cons|_])),
                                                                consFrances(Cons),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f,Cons|W]),
                                                                !.

% AIS-que

posibilidadesFrances(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f,Cons,Constr|W],Resto):-
                                                                diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                                                (cons_fFrances(Cons_f);pseudocons_fFrances(Cons_f,[Cons_f,Cons|_])),
                                                                consParejaFrances_1(Cons,[Cons,Constr]),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f,Cons,Constr|W]),
                                                                !.


% Au-un

posibilidadesFrances(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,Vocal3|W],Resto):-
                                                                diptongoFrances(Vocal,[Vocal,Vocal2]),
                                                                hiatoFrances(Vocal2,[Vocal2,Vocal3]),
                                                                append([Vocal,Vocal2],Resto,[Vocal,Vocal2,Vocal3|W]),
                                                                !.






% a-e-re-o

posibilidadesFrances(Vocal,[Vocal],[Vocal,Vocal2|W],Resto):-
                                                                 hiatoFrances(Vocal,[Vocal,Vocal2|_]),
                                                                 append([Vocal],Resto,[Vocal,Vocal2|W]),
                                                                 !.

% en

posibilidadesFrances(Vocal,[Vocal,Cons_f],[Vocal,Cons_f],Resto):-
                                                                vocalFrances(Vocal),
                                                                cons_fFrances(Cons_f),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f]),
                                                                !.


% is-la

posibilidadesFrances(Vocal,[Vocal,Cons_f],[Vocal,Cons_f,ConsBis,VocalBis|W],Resto):-
                                                                 vocalFrances(Vocal),
                                                                (cons_fFrances(Cons_f);pseudocons_fFrances(Cons_f,[Cons_f,ConsBis|W])),
                                                                consFrances(ConsBis),
                                                                vocalFrances(VocalBis),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f,ConsBis,VocalBis|W]),
                                                                !.




%ens-ta

posibilidadesFrances(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                                                 vocalFrances(Vocal),
                                                                  consFinalTotal_frances(ConsFinalTotal,[Cons_f,ConsFinalTotal]),
                                                                append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal|W]),
                                                                !.

%ens

posibilidadesFrances(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal],Resto):-
                                                                 vocalFrances(Vocal),
                                                                refinalTotal_frances(ConsFinalTotal,[Cons_f,ConsFinalTotal]),
                                                                append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal]),
                                                                !.




% ay, uy: unicas palabras encontradas con dipt que solo son eso: el dipt, pero meto todos los casos pq nada lo prohibe. No meto el dipt ascendente ni el doble pq no parece probable

posibilidadesFrances(Vocal,[Vocal,Vocal2],[Vocal,Vocal2],Resto):-
                                                                diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                                                append([Vocal,Vocal2],Resto,[Vocal,Vocal2]),
                                                                !.


% ea: hiato aislado. Solo encontrado "ea", pero meto todo porque nada lo impide

posibilidadesFrances(Vocal,[Vocal],[Vocal,Vocal2],Resto):-
                                                                hiatoFrances(Vocal,[Vocal,Vocal2|_]),
                                                                append([Vocal],Resto,[Vocal,Vocal2]),
                                                                !.


% posibilidades para CONS+VOCAL+VOCAL AISLADO: HIATO

posibilidadesFrances(Cons,[Cons,Vocal],[Cons,Vocal,Vocal2],Resto):-
                                          consFrances(Cons),
                                          hiatoFrances(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Vocal2]),
                                          !.

% tue-or

posibilidadesFrances(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2,Vocal3|W],Resto):-
                                          consFrances(Cons),
                                          diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                          hiatoFrances(Vocal2,[Vocal2,Vocal3|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2,Vocal3|W]),
                                          !.

%% Caso particular: trabadas en mitad de palabra






% bla

posibilidadesFrances(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal],Resto):-
                                          consParejaFrances(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalFrances(Vocal),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal]),
                                          !.






% gra-pa

posibilidadesFrances(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,Cons,Vocal1|W],Resto):-
                                          consParejaFrances(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalFrances(Vocal),
                                          vocalFrances(Vocal1),
                                          consFrances(Cons),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,Cons,Vocal1|W]),
                                          !.



% blau?

posibilidadesFrances(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2],Resto):-
                                          consParejaFrances(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2]),
                                          !.


% blau-bla

posibilidadesFrances(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2,ConsParejaBis,Cons_trBis|W],Resto):-
                                          consParejaFrances(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                          consParejaFrances(ConsParejaBis,[ConsParejaBis,Cons_trBis|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,ConsParejaBis,Cons_trBis|W]),
                                          !.



% blaun

posibilidadesFrances(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto):-
                                          consParejaFrances(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                          cons_fFrances(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f]),
                                       !.



% brains

posibilidadesFrances(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsRefinal],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsRefinal],Resto):-
                                          consParejaFrances(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                          consFrances(Cons),
                                           refinalTotal_frances(ConsRefinal,[Cons_f,ConsRefinal]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsRefinal],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsRefinal]),
                                          !.



% brain-le

posibilidadesFrances(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,Cons|W],Resto):-
                                          consParejaFrances(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoFrances(Vocal,[Vocal,Vocal2|_]),
                                          consFrances(Cons),
                                          (cons_fFrances(Cons_f);pseudocons_fFrances(Cons_f,[Cons_f,Cons|_])),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,Cons|W]),
                                          !.






% pro-e-mio

posibilidadesFrances(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,Vocal2|W],Resto):-
                                          consParejaFrances(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          hiatoFrances(Vocal,[Vocal,Vocal2|_]),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2|W]),
                                          !.





% bla-bla

posibilidadesFrances(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,ConsPareja1,Cons_tr1|W],Resto):-
                                          consParejaFrances(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalFrances(Vocal),
                                          consParejaFrances_1(ConsPareja1,[ConsPareja1,Cons_tr1|_]),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,ConsPareja1,Cons_tr1|W]),
                                          !.



% blas

posibilidadesFrances(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f],Resto):-
                                          consParejaFrances(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalFrances(Vocal),
                                          cons_fFrances(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f]),
                                          !.

%blast

posibilidadesFrances(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsRefinal],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsRefinal],Resto):-
                                          consParejaFrances(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalFrances(Vocal),
                                          refinalTotal_frances(ConsRefinal,[Cons_f,ConsRefinal]),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f,ConsRefinal],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsRefinal]),
                                          !.

%blast

posibilidadesFrances(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsRefinal,Final],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsRefinal,Final],Resto):-
                                          consParejaFrances(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalFrances(Vocal),
                                          refinalTotal_frances(Final,[Cons_f,ConsRefinal,Final]),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f,ConsRefinal,Final],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsRefinal,Final]),
                                          !.







% blan-ca

posibilidadesFrances(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f,Cons1,Vocal1|W],Resto):-
                                          consParejaFrances(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalFrances(Vocal),
                                          vocalFrances(Vocal1),
                                          (cons_fFrances(Cons_f);pseudocons_fFrances(Cons_f,[Cons_f,Cons1|_])),
                                          consFrances(Cons1),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,Cons1,Vocal1|W]),
                                          !.


% blan-bla

posibilidadesFrances(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsPareja1,Cons_tr1|W],Resto):-
                                          consParejaFrances(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalFrances(Vocal),
                                          (cons_fFrances(Cons_f);pseudocons_fFrances(Cons_f,[Cons_f,ConsPareja1|_])),
                                          consParejaFrances(ConsPareja1,[ConsPareja1,Cons_tr1|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsPareja1,Cons_tr1|W]),
                                          !.






% Triptongo



posibilidadesFrances(Cons,[Cons,Vocal,Vocal2,Vocal3],[Cons,Vocal,Vocal2,Vocal3,ConsBis,VocalBis],Resto):-
                                          consFrances(Cons),
                                          consFrances(ConsBis),
                                          triptongoFrances(Vocal,[Vocal,Vocal2,Vocal3]),
                                          vocalFrances(VocalBis),
                                          append([Cons,Vocal,Vocal2,Vocal3],Resto,[Cons,Vocal,Vocal2,Vocal3,ConsBis,VocalBis]),
                                          !.

posibilidadesFrances(Cons,[Cons,Vocal,Vocal2,Vocal3],[Cons,Vocal,Vocal2,Vocal3,ConsPareja,Cons_tr|W],Resto):-
                                          consFrances(Cons),
                                          triptongoFrances(Vocal,[Vocal,Vocal2,Vocal3]),
                                          consParejaFrances(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          append([Cons,Vocal,Vocal2,Vocal3],Resto,[Cons,Vocal,Vocal2,Vocal3,ConsPareja,Cons_tr|W]),
                                          !.

posibilidadesFrances(Cons,[Cons,Vocal,Vocal2,Vocal3],[Cons,Vocal,Vocal2,Vocal3],Resto):-
                                          consFrances(Cons),
                                          consFrances(ConsBis),
                                         triptongoFrances(Vocal,[Vocal,Vocal2,Vocal3]),
                                          vocalFrances(VocalBis),
                                          append([Cons,Vocal,Vocal2,Vocal3],Resto,[Cons,Vocal,Vocal2,Vocal3]),
                                          !.

% Triptongo en cons_tr

posibilidadesFrances(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],Resto):-
                                          consParejaFrances(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          triptongoFrances(Vocal,[Vocal,Vocal2,Vocal3]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3]),
                                          !.

% Triptongo +cons final

posibilidadesFrances(Cons,[Cons,Vocal,Vocal2,Vocal3,Cons_f],[Cons,Vocal,Vocal2,Vocal3,Cons_f],Resto):-
                                          consFrances(Cons),
                                          triptongoFrances(Vocal,[Vocal,Vocal2,Vocal3]),
                                          cons_fFrances(Cons_f),
                                          append([Cons,Vocal,Vocal2,Vocal3,Cons_f],Resto,[Cons,Vocal,Vocal2,Vocal3,Cons_f]),
                                          !.

% Triptongo en cons_tr + cons final

posibilidadesFrances(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],Resto):-
                                          consParejaFrances(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          triptongoFrances(Vocal,[Vocal,Vocal2,Vocal3]),
                                          cons_fFrances(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f]),
                                          !.


% Triptongo +cons final

posibilidadesFrances(Cons,[Cons,Vocal,Vocal2,Vocal3],[Cons,Vocal,Vocal2,Vocal3,ConsBis,VocalBis|W],Resto):-
                                          consFrances(Cons),
                                          triptongoFrances(Vocal,[Vocal,Vocal2,Vocal3]),
                                          consFrances(ConsBis),
                                          vocalFrances(VocalBis),
                                          append([Cons,Vocal,Vocal2,Vocal3],Resto,[Cons,Vocal,Vocal2,Vocal3,ConsBis,VocalBis|W]),
                                          !.
                                          
% Triptongo +cons final

posibilidadesFrances(Cons,[Cons,Vocal,Vocal2,Vocal3,Cons_f,Refinal],[Cons,Vocal,Vocal2,Vocal3,Cons_f,Refinal],Resto):-
                                          consFrances(Cons),
                                          triptongoFrances(Vocal,[Vocal,Vocal2,Vocal3]),
                                         refinalTotal_frances(Refinal,[Cons_f,Refinal]),
                                          append([Cons,Vocal,Vocal2,Vocal3,Cons_f,Refinal],Resto,[Cons,Vocal,Vocal2,Vocal3,Cons_f,Refinal]),
                                          !.


%l'ortografia

posibilidadesFrances(Cons,[Cons],[Cons,'\'',Vocal|W],Resto):-
                                          consFrances(Cons),
                                          (vocalFrances(Vocal); Vocal='h'),
                                          append([Cons,'\''],Resto,[Cons,'\'',Vocal|W]),
                                          !.
                                          
posibilidadesFrances(q,[q,u],[q,u,'\'',Vocal|W],Resto):-
                                          vocalFrances(Vocal),
                                          append([Cons,'\''],Resto,[Cons,'\'',Vocal|W]),
                                          !.
                                          
posibilidadesFrances(q,[q,u],[q,u,'’',Vocal|W],Resto):-
                                          vocalFrances(Vocal),
                                          append([Cons,'’'],Resto,[Cons,'’',Vocal|W]),
                                          !.

posibilidadesFrances(q,[q,u],[q,u,'´',Vocal|W],Resto):-
                                          vocalFrances(Vocal),
                                          append([Cons,'´'],Resto,[Cons,'´',Vocal|W]),
                                          !.



posibilidadesFrances(Cons,[Cons,'’'],[Cons,'’',Vocal|W],Resto):-
                                          consFrances(Cons),
                                          (vocalFrances(Vocal); Vocal='h'),
                                          append([Cons,'’'],Resto,[Cons,'’',Vocal|W]),
                                          !.

posibilidadesFrances(Cons,[Cons,'´'],[Cons,'´',Vocal|W],Resto):-
                                         consFrances(Cons),
                                          (vocalFrances(Vocal); Vocal='h'),
                                          append([Cons,'´'],Resto,[Cons,'´',Vocal|W]),
                                          !.
