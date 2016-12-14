% X tiene que ir "" para que prolog lo lea como una lista de codigos ANSII. El predicado español/1 dice que algo es español si hacemos el procedimiento lista/2 y el comprobar_palabras/1.




portugues(X,Restos,Origen):-
                           portugues(X,Restos),
                           origen_etimologicoPortugues(Restos,Origen).

origen_etimologicoPortugues([],[]).

origen_etimologicoPortugues([Restos|MasRestos],[Origen|MasOrigen]):-
                                                 atom_chars(Restos,Deletreo),
                                                 combinacionesPortugues(Restos,Deletreo,Origen),
                                                 !,
                                                 origen_etimologicoPortugues(MasRestos,MasOrigen).

combinacionesPortugues(Restos,[J],desconocido(Restos)).


combinacionesPortugues(Restos,[t,l|W],griego_o_nahuatl(Restos)).





combinacionesPortugues(Restos,[X,Y|W],Origen):-
                                       combinacionesPortugues(Restos,[Y|W],Origen).



portugues(X,Restos):-
               transcribir(X,W),
               !,
               lista(W,Y),
               comprobar_palabrasPortugues(Y,N),
               recuperar_descartesPortugues(N,Restos).

recuperar_descartesPortugues([],[]).

recuperar_descartesPortugues([N|M],[Restos|MasRestos]):-
               atom_codes(Restos,N),
               recuperar_descartesPortugues(M,MasRestos).


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
                          W>=192,
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
                                                    signPunt_portugues(SignPunt),
                                                    Sublista=[],
                                                    corte(Resto, Resfinal, Resprov, []).

% Reglas recursivas
corte([SignPunt|Resto], Resfinal, Resprov, Sublista):-
                                                       signPunt_portugues(SignPunt),
                                                       append(Resprov, [Sublista], Res),
                                                       corte(Resto, Resfinal, Res, []).

corte([Cab|Resto], Resfinal, Resprov, Sublista):-
                                                   not(signPunt_portugues(Cab)),
                                                   append(Sublista, [Cab], Prov),
                                                   corte(Resto, Resfinal, Resprov, Prov).

% Signos de puntuacion considerados

signPunt_portugues(32).
signPunt_portugues(33).
signPunt_portugues(34).
signPunt_portugues(38).
signPunt_portugues(40).
signPunt_portugues(41).
signPunt_portugues(44).
signPunt_portugues(45).
signPunt_portugues(46).
signPunt_portugues(47).
signPunt_portugues(58).
signPunt_portugues(59).
signPunt_portugues(63).
signPunt_portugues(132).
signPunt_portugues(147).
signPunt_portugues(148).
signPunt_portugues(161).
signPunt_portugues(191).
signPunt_portugues(171).
signPunt_portugues(187).
signPunt_portugues(8220).
signPunt_portugues(8221).
signPunt_portugues(8211).
signPunt_portugues(91).
signPunt_portugues(93).
signPunt_portugues(92).
signPunt_portugues(8212).
signPunt_portugues(60).
signPunt_portugues(62).
signPunt_portugues(8230).

signPunt_portugues(X):-
             X>47,
             X<58.







comprobar_palabrasPortugues([],[]).


comprobar_palabrasPortugues([Z|W],Sobras):-
                       silabearPortugues(Z,F),
                       comprobar_palabrasPortugues(W,Sobras),
                       !.

comprobar_palabrasPortugues([Z|W],[Z|Sobras]):-
                       not(silabearPortugues(Z,F)),
                       comprobar_palabrasPortugues(W,Sobras),
                       !.








% El proceso de silabeo se incia con la transformacion de lista de codigo ANSII a letras sueltas para poder silabear, y lo manda ya en forma de letras al proceso auxiliar de silabeo


silabearPortugues(X,Z):-
               atom_codes(Y,X),
               atom_chars(Y,W),
                           olfatear_portugues(W),
               silabear_auxPortugues(W,Z).
                           
olfatear_portugues([]).

olfatear_portugues([q,u,u|T]):-
                           !,
                           fail.
                           
olfatear_portugues([l,l|T]):-
                           !,
                           fail.
                           
olfatear_portugues([Vocal,h,Vocal2|T]):-
                                       (vocalPortugues(Vocal),vocalPortugues(Vocal2));
                                        not(consParejaPortugues(Vocal,[Vocal,h])),
                                       !,
                                       fail.
                                                   
                                                   
olfatear_portugues([H|T]):-
                                                olfatear_portugues(T).
                           

silabeoPortugues(X,Z):-
               silabear_auxPortugues(X,Z).

% El proceso auxiliar de silabeo

silabear_auxPortugues([],[]).

silabear_auxPortugues([Z|T],[Silaba|Silaba_resto]):-
                 posibilidadesPortugues(Z,Silabeo,[Z|T],Resto),
                 atom_chars(Silaba,Silabeo),
                 silabear_auxPortugues(Resto,Silaba_resto).









% Letras en español

vocalPortugues(i).
vocalPortugues(u).
vocalPortugues(a).
vocalPortugues(e).
vocalPortugues(o).
vocalPortugues(ã).
vocalPortugues(õ).
vocalPortugues(á).
vocalPortugues(é).
vocalPortugues(í).
vocalPortugues(ó).
vocalPortugues(ú).

vocalPortugues(à).

vocalPortugues(â).
vocalPortugues(ê).
vocalPortugues(ô).



letras_equivalentesPortugues(a,á).
letras_equivalentesPortugues(e,é).
letras_equivalentesPortugues(i,í).
letras_equivalentesPortugues(o,ó).
letras_equivalentesPortugues(u,ú).

letras_equivalentesPortugues(a,à).

letras_equivalentesPortugues(a,â).
letras_equivalentesPortugues(e,ê).
letras_equivalentesPortugues(o,ô).

letras_equivalentesPortugues(A,A):-
                                   vocalPortugues(A).







bilabial(p).
bilabial(b).

liquida(r).
liquida(l).

%% Consontantes emparejadas

% b

consParejaPortugues(b,[b,Liquida|_]):-
                              liquida(Liquida).

% f

consParejaPortugues(f,[f,Liquida|_]):-
                              liquida(Liquida).

% g

consParejaPortugues(g,[g,Liquida|_]):-
                              liquida(Liquida).

% c

consParejaPortugues(c,[c,Liquida|_]):-
                              liquida(Liquida).
                              
consParejaPortugues(c,[c,ç|_]).

% p

consParejaPortugues(p,[p,Liquida|_]):-
                              liquida(Liquida).
                              
% p

consParejaPortugues(p,[p,t|_]).

consParejaPortugues(p,[p,c|_]).
                              
% t

consParejaPortugues(t,[t,Liquida|_]):-
                              liquida(Liquida).


% d

consParejaPortugues(d,[d,r|_]).


% ch

consParejaPortugues(c,[c,h|_]).

% ll

consParejaPortugues(l,[l,h|_]).

% ll

consParejaPortugues(n,[n,h|_]).

% vr

consParejaPortugues(v,[v,r|_]).

% qu

consParejaPortugues(q,[q,u|_]).

% gu

consParejaPortugues(g,[g,u|_]).




%% Vocales emparejadas: DIPTONGOS

diptongoPortugues(Vocal_a,[Vocal_a,Vocal_i|_]):-
                                        letras_equivalentesPortugues(a,Vocal_a),
                                        letras_equivalentesPortugues(i,Vocal_i).

                                        

diptongoPortugues(Vocal_a,[Vocal_a,Vocal_u|_]):-
                                        letras_equivalentesPortugues(a,Vocal_a),
                                        letras_equivalentesPortugues(u,Vocal_u).



diptongoPortugues(Vocal_e,[Vocal_e,Vocal_i|_]):-
                                                letras_equivalentesPortugues(i,Vocal_i),
                                                letras_equivalentesPortugues(e,Vocal_e).
                                         
diptongoPortugues(Vocal_e,[Vocal_e,Vocal_u|_]):-
                                         letras_equivalentesPortugues(e,Vocal_e),
                                         letras_equivalentesPortugues(u,Vocal_u).





diptongoPortugues(Vocal_o,[Vocal_o,Vocal_i|_]):-
                                        letras_equivalentesPortugues(o,Vocal_o),
                                        letras_equivalentesPortugues(i,Vocal_i).


diptongoPortugues(Vocal_o,[Vocal_o,Vocal_u|_]):-
                                        letras_equivalentesPortugues(o,Vocal_o),
                                        letras_equivalentesPortugues(u,Vocal_u).




diptongoPortugues(Vocal_u,[Vocal_u,Vocal_i|_]):-
                                   letras_equivalentesPortugues(i,Vocal_i),
                                   letras_equivalentesPortugues(u,Vocal_u).

                                   
diptongoPortugues(Vocal_i,[Vocal_i,Vocal_u|_]):-
                                   letras_equivalentesPortugues(i,Vocal_i),
                                   letras_equivalentesPortugues(u,Vocal_u).
                                   
diptongoPortugues(Vocal_i,[Vocal_i,Vocal_a|_]):-
                                   letras_equivalentesPortugues(a,Vocal_a),
                                   letras_equivalentesPortugues(i,Vocal_i).
                                   
diptongoPortugues(Vocal_i,[Vocal_i,Vocal_e|_]):-
                                   letras_equivalentesPortugues(e,Vocal_e),
                                   letras_equivalentesPortugues(i,Vocal_i).
                                   
diptongoPortugues(Vocal_i,[Vocal_i,Vocal_o|_]):-
                                   letras_equivalentesPortugues(o,Vocal_o),
                                   letras_equivalentesPortugues(i,Vocal_i).
                                   
diptongoPortugues(Vocal_u,[Vocal_u,Vocal_a|_]):-
                                   letras_equivalentesPortugues(a,Vocal_a),
                                    letras_equivalentesPortugues(u,Vocal_u).

                                   
diptongoPortugues(Vocal_u,[Vocal_u,Vocal_o|_]):-
                                   letras_equivalentesPortugues(o,Vocal_o),
                                   letras_equivalentesPortugues(u,Vocal_u).
                                   
diptongoPortugues(ã,[ã,e|_]).

diptongoPortugues(ã,[ã,o|_]).

diptongoPortugues(õ,[õ,e|_]).


hiatoPortugues(Vocal_a,[Vocal_a,Vocal_e|_]):-
                               letras_equivalentesPortugues(a,Vocal_a),
                               letras_equivalentesPortugues(e,Vocal_e).


hiatoPortugues(Vocal_a,[Vocal_a,Vocal_o|_]):-
                               letras_equivalentesPortugues(a,Vocal_a),
                               letras_equivalentesPortugues(o,Vocal_o).

hiatoPortugues(Vocal_a,[Vocal_a,Vocal_a|_]):-
                               letras_equivalentesPortugues(a,Vocal_a),
                               letras_equivalentesPortugues(a,Vocal_a).

hiatoPortugues(Vocal_e,[Vocal_e,Vocal_a|_]):-
                               letras_equivalentesPortugues(e,Vocal_e),
                               letras_equivalentesPortugues(a,Vocal_a).

hiatoPortugues(Vocal_e,[Vocal_e,Vocal_e|_]):-
                               letras_equivalentesPortugues(e,Vocal_e),
                               letras_equivalentesPortugues(e,Vocal_e).

hiatoPortugues(Vocal_e,[Vocal_e,Vocal_o|_]):-
                               letras_equivalentesPortugues(e,Vocal_e),
                               letras_equivalentesPortugues(o,Vocal_o).



hiatoPortugues(Vocal_o,[Vocal_o,Vocal_a|_]):-
                               letras_equivalentesPortugues(o,Vocal_o),
                               letras_equivalentesPortugues(a,Vocal_a).

hiatoPortugues(Vocal_o,[Vocal_o,Vocal_e|_]):-
                               letras_equivalentesPortugues(o,Vocal_o),
                               letras_equivalentesPortugues(e,Vocal_e).

hiatoPortugues(Vocal_o,[Vocal_o,í|_]):-
                               letras_equivalentesPortugues(o,Vocal_o).

hiatoPortugues(Vocal_o,[Vocal_o,Vocal_o2|_]):-
                               letras_equivalentesPortugues(o,Vocal_o),
                               letras_equivalentesPortugues(o,Vocal_o2).


hiatoPortugues(u,[u,Vocal_e|_]):-
                                   letras_equivalentesPortugues(e,Vocal_e).
                                   



triptongoPortugues(e,[e,ã,o]).

triptongoPortugues(i,[i,ã,o]).

triptongoPortugues(i,[i,a,i]).

triptongoPortugues(u,[u,a,i]).

triptongoPortugues(i,[i,õ,e]).

triptongoPortugues(Vocal_a,[Vocal_a,Vocal_i,Vocal_a2]):-
                                                        letras_equivalentesPortugues(a,Vocal_a),
                                                        letras_equivalentesPortugues(a,Vocal_a2),
                                                        letras_equivalentesPortugues(i,Vocal_i).















parejaFinalTotal([n,s]).

cons_fTotalPortugues(m).
cons_fTotalPortugues(z).
cons_fTotalPortugues(l).
cons_fTotalPortugues(r).
cons_fTotalPortugues(s).


cons_fPortugues(n).
cons_fPortugues(l).
cons_fPortugues(r).
cons_fPortugues(s).


%% Consontantes pseudofinales: contextos

% Letra m

pseudocons_fPortugues(m,[m,Bilabial|_]):-
                                  bilabial(Bilabial).

% Letra g


pseudocons_fPortugues(g,[g,n|_]).


% Letra p

dentalPortugues(t).
dentalPortugues(ç).

pseudocons_fPortugues(p,[p,Dentales|_]):-
                                  dentalPortugues(Dentales).




% Letra x

pseudocons_fPortugues(x,[x,t|_]).


% Letra c

pseudocons_fPortugues(c,[c,t|_]).






consPortugues(b).
consPortugues(c).
consPortugues(d).
consPortugues(f).
consPortugues(g).
consPortugues(h).
consPortugues(j).
consPortugues(k).
consPortugues(l).
consPortugues(m).
consPortugues(n).
consPortugues(p).
consPortugues(r).
consPortugues(s).
consPortugues(t).
consPortugues(v).
consPortugues(x).
consPortugues(y).
consPortugues(z).
consPortugues(ç).
consPortugues(i).





% Silabas en español

% posibilidades/4: letra que encabeza la silaba/silaba/letras que siguen/resto de la palabra (sin la silaba que acabamos de aislar, pero si con el contexto.

%% PREFIJOS

% sub

posibilidadesPortugues(s,[s,u,b,s],[s,u,b,s,Cons|W],Resto):-
                                                                consPortugues(Cons),
                                                                append([s,u,b,s],Resto,[s,u,b,s,Cons|W]),
                                                                !.

posibilidadesPortugues(s,[s,u,b],[s,u,b,Cons|W],Resto):-
                                                                consPortugues(Cons),
                                                                append([s,u,b],Resto,[s,u,b,Cons|W]),
                                                              !.



% ob

posibilidadesPortugues(o,[o,b,s],[o,b,s,Cons|W],Resto):-
                                                                consPortugues(Cons),
                                                                append([o,b,s],Resto,[o,b,s,Cons|W]),
                                                                 !.

posibilidadesPortugues(o,[o,b],[o,b,Cons|W],Resto):-
                                                                consPortugues(Cons),
                                                                not(consParejaPortugues(b,[b,Cons])),
                                                                append([o,b],Resto,[o,b,Cons|W]),
                                                                 !.


%ab

posibilidadesPortugues(a,[a,b,s],[a,b,s,Cons|W],Resto):-
                                                                consPortugues(Cons),
                                                                append([a,b,s],Resto,[a,b,s,Cons|W]),
                                                                !.

posibilidadesPortugues(a,[a,b],[a,b,Cons|W],Resto):-
                                                               consPortugues(Cons),
                                                                not(consParejaPortugues(b,[b,Cons])),
                                                                 append([a,b],Resto,[a,b,Cons|W]),
                                                                !.

% ad

posibilidadesPortugues(a,[a,d],[a,d,Cons|W],Resto):-
                                                               consPortugues(Cons),
                                                                not(consParejaPortugues(d,[d,Cons])),
                                                                 append([a,d],Resto,[a,d,Cons|W]),
                                                                !.
                                                                
% ad

posibilidadesPortugues(a,[a,d,s],[a,d,s,Cons|W],Resto):-
                                                               consPortugues(Cons),
                                                                append([a,d,s],Resto,[a,d,s,Cons|W]),
                                                                !.


%ex

posibilidadesPortugues(e,[e,x],[e,x,Cons|W],Resto):-
                                                                consPortugues(Cons),
                                                                append([e,x],Resto,[e,x,Cons|W]),
                                                                 !.
                                                                 
                                                                 






%%% silabas que empiezan por vocal

%% OJO PARA TODO EL CÓDIGO QUE SIGUE: OI, AU y AI son los unicos dipt que forman dipt tal cual. Los desmas son extranj: comprobar


% a

posibilidadesPortugues(Vocal,[Vocal],[Vocal],Resto):-
                                                                vocalPortugues(Vocal),
                                                                append([Vocal],Resto,[Vocal]),
                                                                !.


% a-la

posibilidadesPortugues(Vocal,[Vocal],[Vocal,Cons,Vocal1|W],Resto):-
                                                                vocalPortugues(Vocal),
                                                                consPortugues(Cons),
                                                                vocalPortugues(Vocal1),
                                                                append([Vocal],Resto,[Vocal,Cons,Vocal1|W]),
                                                                !.

% a-pro-xi-mar-se; a-dri

posibilidadesPortugues(Vocal,[Vocal],[Vocal,ConsPareja,Cons_tr,Vocal2|W],Resto):-
                                                                 vocalPortugues(Vocal),
                                                                 consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                                               append([Vocal],Resto,[Vocal,ConsPareja,Cons_tr,Vocal2|W]),
                                                                !.



% a-e-re-o

posibilidadesPortugues(Vocal,[Vocal],[Vocal,Vocal2|W],Resto):-
                                                                hiatoPortugues(Vocal,[Vocal,Vocal2|_]),
                                                                 append([Vocal],Resto,[Vocal,Vocal2|W]),
                                                                 !.

% en

posibilidadesPortugues(Vocal,[Vocal,Cons_f],[Vocal,Cons_f],Resto):-
                                                                vocalPortugues(Vocal),
                                                                cons_fTotalPortugues(Cons_f),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f]),
                                                                !.
% is-la

posibilidadesPortugues(Vocal,[Vocal,Cons_f],[Vocal,Cons_f,ConsBis,VocalBis|W],Resto):-
                                                                 vocalPortugues(Vocal),
                                                                (cons_fPortugues(Cons_f);pseudocons_fPortugues(Cons_f,[Cons_f,ConsBis|W])),
                                                                consPortugues(ConsBis),
                                                                vocalPortugues(VocalBis),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f,ConsBis,VocalBis|W]),
                                                                !.


% antro, an-cla

posibilidadesPortugues(Vocal,[Vocal,Cons_f],[Vocal,Cons_f,ConsPareja,Cons_tr|W],Resto):-
                                                                 vocalPortugues(Vocal),
                                                                (cons_fPortugues(Cons_f);pseudocons_fPortugues(Cons_f,[Cons_f,ConsPareja|_])),
                                                                consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f,ConsPareja,Cons_tr|W]),
                                                                !.

%ins-tau-rar

posibilidadesPortugues(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal,ConsBis|W],Resto):-
                                                                 vocalPortugues(Vocal),
                                                                 parejaFinalTotal([Cons_f,ConsFinalTotal]),
                                                                consPortugues(ConsBis),
                                                                append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal,ConsBis|W]),
                                                                !.
                                                                
%ins-tau-rar

posibilidadesPortugues(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal],Resto):-
                                                                 vocalPortugues(Vocal),
                                                                 parejaFinalTotal([Cons_f,ConsFinalTotal]),
                                                                consPortugues(ConsBis),
                                                                append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal]),
                                                                !.



% ay, uy: unicas palabras encontradas con dipt que solo son eso: el dipt, pero meto todos los casos pq nada lo prohibe. No meto el dipt ascendente ni el doble pq no parece probable

posibilidadesPortugues(Vocal,[Vocal,Vocal2],[Vocal,Vocal2],Resto):-
                                                                diptongoPortugues(Vocal,[Vocal,Vocal2|_]),
                                                                append([Vocal,Vocal2],Resto,[Vocal,Vocal2]),
                                                                !.

% ai-rar, au-lar, eusebio

posibilidadesPortugues(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,Cons,Vocal1|W],Resto):-
                                                                        diptongoPortugues(Vocal,[Vocal,Vocal2|_]),
                                                                        consPortugues(Cons),
                                                                        vocalPortugues(Vocal1),
                                                                        append([Vocal,Vocal2],Resto,[Vocal,Vocal2,Cons,Vocal1|W]),
                                                                         !.


% ai-trar

posibilidadesPortugues(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,ConsPareja,Cons_tr|W],Resto):-
                                                                        diptongoPortugues(Vocal,[Vocal,Vocal2|_]),
                                                                        consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr]),
                                                                        append([Vocal,Vocal2],Resto,[Vocal,Vocal2,ConsPareja,Cons_tr|W]),
                                                                         !.




% a-ma-ri-AIS

posibilidadesPortugues(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f],Resto):-
                                                                diptongoPortugues(Vocal,[Vocal,Vocal2|_]),
                                                                 cons_fTotalPortugues(Cons_f),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f]),
                                                                !.


% AIS-lar

posibilidadesPortugues(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f,Cons|W],Resto):-
                                                                diptongoPortugues(Vocal,[Vocal,Vocal2|_]),
                                                                (cons_fPortugues(Cons_f);pseudocons_fPortugues(Cons_f,[Cons_f,Cons|_])),
                                                                consPortugues(Cons),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f,Cons|W]),
                                                                !.

% aun-que

posibilidadesPortugues(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f,ConsPareja,Trabada|W],Resto):-
                                                                diptongoPortugues(Vocal,[Vocal,Vocal2|_]),
                                                                (cons_fPortugues(Cons_f);pseudocons_fPortugues(Cons_f,[Cons_f,ConsPareja|_])),
                                                                consParejaPortugues(ConsPareja,[ConsPareja,Trabada|_]),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f,ConsPareja,Trabada|W]),
                                                                !.

% ea: hiato aislado. Solo encontrado "ea", pero meto todo porque nada lo impide

posibilidadesPortugues(Vocal,[Vocal],[Vocal,Vocal2],Resto):-
                                                               hiatoPortugues(Vocal,[Vocal,Vocal2|_]),
                                                                append([Vocal],Resto,[Vocal,Vocal2]),
                                                                !.

% posibilidades para CONS+VOCAL AISLADO

posibilidadesPortugues(Cons,[Cons,Vocal],[Cons,Vocal],Resto):-
                                          consPortugues(Cons),
                                          vocalPortugues(Vocal),
                                          append([Cons,Vocal],Resto,[Cons,Vocal]),
                                          !.

% posibilidaddes para CONS+VOCAL EN PALABRA


posibilidadesPortugues(Cons,[Cons,Vocal],[Cons,Vocal,Cons1,Vocal1|W],Resto):-
                                          consPortugues(Cons),
                                          vocalPortugues(Vocal),
                                          consPortugues(Cons1),
                                          vocalPortugues(Vocal1),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Cons1,Vocal1|W]),
                                          !.

% posibilidaddes para CONS+VOCAL EN PALABRA: HIATO

posibilidadesPortugues(Cons,[Cons,Vocal],[Cons,Vocal,Vocal2|W],Resto):-
                                          consPortugues(Cons),
                                         hiatoPortugues(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Vocal2|W]),
                                          !.


% posibilidades para CONS+VOCAL+VOCAL AISLADO: HIATO

posibilidadesPortugues(Cons,[Cons,Vocal],[Cons,Vocal,Vocal2],Resto):-
                                          consPortugues(Cons),
                                          hiatoPortugues(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Vocal2]),
                                          !.

%% Caso particular: trabadas en mitad de palabra

% nu-blar  pa-dre

posibilidadesPortugues(Cons,[Cons,Vocal],[Cons,Vocal,ConsPareja,Cons_tr|W],Resto):-
                                          consPortugues(Cons),
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr]),
                                          vocalPortugues(Vocal),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,ConsPareja,Cons_tr|W]),
                                          !.


% CONS+VOCAL+CONS_FIN EN FINAL DE PALABRA: del-fin


posibilidadesPortugues(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f],Resto):-
                                          consPortugues(Cons),
                                          vocalPortugues(Vocal),
                                           cons_fTotalPortugues(Cons_f),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f]),
                                          !.

% CONS+VOCAL+CONS_F: pen-sar


posibilidadesPortugues(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f,Cons1,Vocal1|W],Resto):-
                                          consPortugues(Cons),
                                          vocalPortugues(Vocal),
                                          (cons_fPortugues(Cons_f);pseudocons_fPortugues(Cons_f,[Cons_f,Cons1|_])),
                                          consPortugues(Cons1),
                                          vocalPortugues(Vocal1),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f,Cons1,Vocal1|W]),
                                          !.

% CONS+VOCAL+CONS_F: pen-trar


posibilidadesPortugues(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f,ConsPareja,Cons_tr|W],Resto):-
                                          consPortugues(Cons),
                                          vocalPortugues(Vocal),
                                          (cons_fPortugues(Cons_f);pseudocons_fPortugues(Cons_f,[Cons_f,ConsTipo|W])),
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_trEspanol]),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f,ConsPareja,Cons_tr|W]),
                                          !.

% CONS-truir


posibilidadesPortugues(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal],[Cons,Vocal,Cons_f,ConsFinalTotal,Cons1|W],Resto):-
                                          consPortugues(Cons),
                                          vocalPortugues(Vocal),
                                          parejaFinalTotal([Cons_f,ConsFinalTotal]),
                                          consPortugues(Cons1),
                                          append([Cons,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,Cons1|W]),
                                          !.
                                          
% CONS


posibilidadesPortugues(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal],[Cons,Vocal,Cons_f,ConsFinalTotal],Resto):-
                                          consPortugues(Cons),
                                          vocalPortugues(Vocal),
                                          parejaFinalTotal([Cons_f,ConsFinalTotal]),
                                          consPortugues(Cons1),
                                          append([Cons,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal]),
                                          !.


% posibilidades para CONS+VOCAL+VOCAL AISLADO:

posibilidadesPortugues(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2],Resto):-
                                          consPortugues(Cons),
                                          diptongoPortugues(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2]),
                                          !.

% posibilidaddes para CONS+VOCAL+VOCAL EN PALABRA:

posibilidadesPortugues(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2,Cons1,Vocal1|W],Resto):-
                                          consPortugues(Cons),
                                          diptongoPortugues(Vocal,[Vocal,Vocal2|_]),
                                          consPortugues(Cons1),
                                          vocalPortugues(Vocal1),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2,Cons1,Vocal1|W]),
                                          !.
% nai-blar; nai-trar

posibilidadesPortugues(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2,ConsPareja,Cons_tr|W],Resto):-
                                          consPortugues(Cons),
                                          diptongoPortugues(Vocal,[Vocal,Vocal2|_]),
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2,ConsPareja,Cons_tr|W]),
                                          !.

% CONS+VOCAL+VOCAL+CONS_FIN con dipt descendente AISLADO: a-ma-bais

posibilidadesPortugues(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f],Resto):-
                                          consPortugues(Cons),
                                          diptongoPortugues(Vocal,[Vocal,Vocal2|_]),
                                           cons_fTotalPortugues(Cons_f),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f]),
                                          !.


% CONS+VOCAL+VOCAL+CONS_F: DIPT DESCENDENTE mais-car



posibilidadesPortugues(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f,Cons1,Vocal1|W],Resto):-
                                          consPortugues(Cons),
                                          diptongoPortugues(Vocal,[Vocal,Vocal2|_]),
                                          (cons_fPortugues(Cons_f);pseudocons_fPortugues(Cons_f,[Cons_f,Cons1|_])),
                                          consPortugues(Cons1),
                                          vocalPortugues(Vocal1),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f,Cons1,Vocal1|W]),
                                          !.

% CONS+VOCAL+VOCAL+CONS_F: DIPT DESCENDENTE mais-bla



posibilidadesPortugues(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f,ConsTipo,Cons_tr|W],Resto):-
                                          consPortugues(Cons),
                                          diptongoPortugues(Vocal,[Vocal,Vocal2|_]),
                                          (cons_fPortugues(Cons_f);pseudocons_fPortugues(Cons_f,[Cons_f,ConsTipo|_])),
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsPareja,Cons_tr|W]),
                                          !.

% CONS+VOCAL+VOCAL+CONS_F*CONSFINALTOTAL: DIPT DESCENDENTE mains-bla



posibilidadesPortugues(Cons,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal,Cons1|W],Resto):-
                                          consPortugues(Cons),
                                          diptongoPortugues(Vocal,[Vocal,Vocal2|_]),
                                          cons_fPortugues(Cons_f),
                                          consPortugues(Cons1),
                                          append([Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal,Cons1|W]),
                                          !.

% bla

posibilidadesPortugues(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal],Resto):-
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalPortugues(Vocal),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal]),
                                          !.

% gra-pa

posibilidadesPortugues(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,Cons,Vocal1|W],Resto):-
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalPortugues(Vocal),
                                          vocalPortugues(Vocal1),
                                          consPortugues(Cons),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,Cons,Vocal1|W]),
                                          !.


% pro-e-mio

posibilidadesPortugues(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,Vocal2|W],Resto):-
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          hiatoPortugues(Vocal,[Vocal,Vocal2|_]),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2|W]),
                                          !.



% bla-bla

posibilidadesPortugues(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,ConsPareja1,Cons_trEspanol1|W],Resto):-
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalPortugues(Vocal),
                                          consParejaPortugues(ConsPareja1,[ConsPareja1,Cons_trEspanol1|_]),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,ConsPareja1,Cons_trEspanol1|W]),
                                          !.

% blas

posibilidadesPortugues(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f],Resto):-
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalPortugues(Vocal),
                                          cons_fTotalPortugues(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f]),
                                          !.


% blan-ca

posibilidadesPortugues(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f,Cons1,Vocal1|W],Resto):-
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalPortugues(Vocal),
                                          vocalPortugues(Vocal1),
                                          (cons_fPortugues(Cons_f);pseudocons_fPortugues(Cons_f,[Cons_f,Cons1|_])),
                                          consPortugues(Cons1),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,Cons1,Vocal1|W]),
                                          !.

% blan-bla

posibilidadesPortugues(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsPareja1,Cons_trEspanol1|W],Resto):-
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalPortugues(Vocal),
                                          (cons_fPortugues(Cons_f);pseudocons_fPortugues(Cons_f,[Cons_f,ConsTipo|_])),
                                          consParejaPortugues(ConsPareja1,[ConsPareja1,Cons_trEspanol1|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsPareja1,Cons_trEspanol1|W]),
                                          !.




% blans-ca; blans-bla

posibilidadesPortugues(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal,Cons1|W],Resto):-
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalPortugues(Vocal),
                                          parejaFinalTotal([Cons_f,ConsFinalTotal]),
                                          consPortugues(Cons1),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal,Cons1|W]),
                                          !.
                                          
% blans
posibilidadesPortugues(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],Resto):-
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalPortugues(Vocal),
                                          parejaFinalTotal([Cons_f,ConsFinalTotal]),
                                          consPortugues(Cons1),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal]),
                                          !.



% blau?

posibilidadesPortugues(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2],Resto):-
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoPortugues(Vocal,[Vocal,Vocal2|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2]),
                                          !.

% brai-le

posibilidadesPortugues(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons,Vocal1|W],Resto):-
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoPortugues(Vocal,[Vocal,Vocal2|_]),
                                          vocalPortugues(Vocal1),
                                          consPortugues(Cons),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons,Vocal1|W]),
                                          !.


% blau-bla

posibilidadesPortugues(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2,ConsParejaBis,Cons_trEspanolBis|W],Resto):-
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoPortugues(Vocal,[Vocal,Vocal2|_]),
                                          consParejaPortugues(ConsParejaBis,[ConsParejaBis,Cons_trEspanolBis|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,ConsParejaBis,Cons_trEspanolBis|W]),
                                          !.

% blaun

posibilidadesPortugues(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto):-
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoPortugues(Vocal,[Vocal,Vocal2|_]),
                                           cons_fTotalPortugues(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f]),
                                       !.
                                       


% brains-le

posibilidadesPortugues(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal,Cons|W],Resto):-
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoPortugues(Vocal,[Vocal,Vocal2|_]),
                                          consPortugues(Cons),
                                          parejaFinalTotal([Cons_f,ConsFinalTotal]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal,Cons|W]),
                                          !.

% brain-le

posibilidadesPortugues(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,Cons|W],Resto):-
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoPortugues(Vocal,[Vocal,Vocal2|_]),
                                          consPortugues(Cons),
                                          (cons_fPortugues(Cons_f);pseudocons_fPortugues(Cons_f,[Cons_f,Cons|_])),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,Cons|W]),
                                          !.






% Triptongo

posibilidadesPortugues(Cons,[Cons,Vocal,Vocal2,Vocal3],[Cons,Vocal,Vocal2,Vocal3],Resto):-
                                          consPortugues(Cons),
                                          triptongoPortugues(Vocal,[Vocal,Vocal2,Vocal3]),
                                          append([Cons,Vocal,Vocal2,Vocal3],Resto,[Cons,Vocal,Vocal2,Vocal3]),
                                          !.

% Triptongo en cons_trEspañol

posibilidadesPortugues(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],Resto):-
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                           triptongoPortugues(Vocal,[Vocal,Vocal2,Vocal3]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3]),
                                          !.

% Triptongo +cons final

posibilidadesPortugues(Cons,[Cons,Vocal,Vocal2,Vocal3,Cons_f],[Cons,Vocal,Vocal2,Vocal3,Cons_f],Resto):-
                                          consPortugues(Cons),
                                           triptongoPortugues(Vocal,[Vocal,Vocal2,Vocal3]),
                                          cons_fPortugues(Cons_f),
                                          append([Cons,Vocal,Vocal2,Vocal3,Cons_f],Resto,[Cons,Vocal,Vocal2,Vocal3,Cons_f]),
                                          !.

% Triptongo en cons_trEspañol + cons final

posibilidadesPortugues(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],Resto):-
                                          consParejaPortugues(ConsPareja,[ConsPareja,Cons_tr|_]),
                                           triptongoPortugues(Vocal,[Vocal,Vocal2,Vocal3]),
                                          cons_fPortugues(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f]),
                                          !.


