% Autor:
% Datum: 22/11/2009




% X tiene que ir "" para que prolog lo lea como una lista de codigos ANSII. El predicado español/1 dice que algo es español si hacemos el procedimiento lista/2 y el comprobar_palabras/1.




español(X,Restos,Origen):-
                           español(X,Restos),
                           origen_etimologicoEspañol(Restos,Origen).

origen_etimologicoEspañol([],[]).

origen_etimologicoEspañol([Restos|MasRestos],[Origen|MasOrigen]):-
                                                 atom_chars(Restos,Deletreo),
                                                 combinacionesEspañol(Restos,Deletreo,Origen),
                                                 !,
                                                 origen_etimologicoEspañol(MasRestos,MasOrigen).

combinacionesEspañol(Restos,[J],desconocido(Restos)).


combinacionesEspañol(Restos,[t,l|W],griego_o_nahuatl(Restos)).

combinacionesEspañol(Restos,[t,m|W],griego(Restos)).

combinacionesEspañol(Restos,[t,n|W],griego(Restos)).

combinacionesEspañol(Restos,[p,n|W],griego(Restos)).

combinacionesEspañol(Restos,[f,t|W],griego_o_arabe(Restos)).

combinacionesEspañol(Restos,[g,m|W],griego(Restos)).

combinacionesEspañol(Restos,[g,d|W],griego(Restos)).

combinacionesEspañol(Restos,[c,n|W],griego(Restos)).

combinacionesEspañol(Restos,[p,s|W],griego_o_latin(Restos)).

combinacionesEspañol(Restos,[u,a,i|W],amerindio_o_catalan(Restos)).



combinacionesEspañol(Restos,[X,Y|W],Origen):-
                                       combinacionesEspañol(Restos,[Y|W],Origen).



español(X,Restos):-
               transcribir(X,W),
               !,
               lista(W,Y),
               comprobar_palabrasEspañol(Y,N),
               recuperar_descartesEspañol(N,Restos).

recuperar_descartesEspañol([],[]).

recuperar_descartesEspañol([N|M],[Restos|MasRestos]):-
               atom_codes(Restos,N),
               recuperar_descartesEspañol(M,MasRestos).


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
                                                    signPunt_espanol(SignPunt),
                                                    Sublista=[],
                                                    corte(Resto, Resfinal, Resprov, []).

% Reglas recursivas
corte([SignPunt|Resto], Resfinal, Resprov, Sublista):-
                                                       signPunt_espanol(SignPunt),
                                                       append(Resprov, [Sublista], Res),
                                                       corte(Resto, Resfinal, Res, []).

corte([Cab|Resto], Resfinal, Resprov, Sublista):-
                                                   not(signPunt_espanol(Cab)),
                                                   append(Sublista, [Cab], Prov),
                                                   corte(Resto, Resfinal, Resprov, Prov).

% Signos de puntuacion considerados

signPunt_espanol(32).
signPunt_espanol(33).
signPunt_espanol(34).
signPunt_espanol(38).
signPunt_espanol(40).
signPunt_espanol(41).
signPunt_espanol(44).
signPunt_espanol(45).
signPunt_espanol(46).
signPunt_espanol(47).
signPunt_espanol(58).
signPunt_espanol(59).
signPunt_espanol(63).
signPunt_espanol(132).
signPunt_espanol(147).
signPunt_espanol(148).
signPunt_espanol(161).
signPunt_espanol(191).
signPunt_espanol(171).
signPunt_espanol(187).
signPunt_espanol(8220).
signPunt_espanol(8221).
signPunt_espanol(8211).
signPunt_espanol(91).
signPunt_espanol(93).
signPunt_espanol(92).
signPunt_espanol(8212).
signPunt_espanol(60).
signPunt_espanol(62).
signPunt_espanol(8230).

signPunt_espanol(X):-
             X>47,
             X<58.







comprobar_palabrasEspañol([],[]).


comprobar_palabrasEspañol([Z|W],Sobras):-
                       silabearEspañol(Z,F),
                       comprobar_palabrasEspañol(W,Sobras),
                       !.

comprobar_palabrasEspañol([Z|W],[Z|Sobras]):-
                       not(silabearEspañol(Z,F)),
                       comprobar_palabrasEspañol(W,Sobras),
                       !.








% El proceso de silabeo se incia con la transformacion de lista de codigo ANSII a letras sueltas para poder silabear, y lo manda ya en forma de letras al proceso auxiliar de silabeo


silabearEspañol(X,Z):-
               atom_codes(Y,X),
               atom_chars(Y,W),
               olfatear_español(W),
                           contar_acentos(W,Numero),
               numero_acentos(Numero),
               silabear_auxEspañol(W,Z).
                           

numero_acentos(Numero):-
                           Numero=<1.
                                                   
numero_acentos(Numero):-
                          Numero>1,
                          !,
                          fail.
                                                  

                                                  
                                        
               

vocal_tildadaEspañol(á).
vocal_tildadaEspañol(é).
vocal_tildadaEspañol(í).
vocal_tildadaEspañol(ó).
vocal_tildadaEspañol(ú).

               
contar_acentos([X|T],Numero):-
                                                contar_acentos(T,NumeroNuevo),
                                                vocal_tildadaEspañol(X),
                                           Numero is NumeroNuevo+1,
                                           !.
                                           

                       
contar_acentos([X|T],Numero):-
                       contar_acentos(T,Numero).
                       

contar_acentos([],0).
               

olfatear_español([]).

olfatear_español([q,u,a|T]):-
                           !,
                           fail.
                           
olfatear_español([q,u,o|T]):-
                           !,
                           fail.
                           
olfatear_español([q,u,u|T]):-
                           !,
                           fail.
                           
olfatear_español([z,e|T]):-
                           !,
                           fail.
                           
olfatear_español([z,i|T]):-
                           !,
                           fail.
                           
olfatear_español([n,b|T]):-
                           !,
                           fail.
                           
olfatear_español([n,p|T]):-
                           !,
                           fail.
                           
olfatear_español([m,v|T]):-
                           !,
                           fail.
                           
olfatear_español([s,s|T]):-
                           !,
                           fail.
olfatear_español([d,d|T]):-
                           !,
                           fail.
                           

                           
olfatear_español([X|Y]):-
                 olfatear_español(Y).

silabeoEspañol(X,Z):-
               silabear_auxEspañol(X,Z).

% El proceso auxiliar de silabeo

silabear_auxEspañol([],[]).

silabear_auxEspañol([Z|T],[Silaba|Silaba_resto]):-
                 posibilidadesEspañol(Z,Silabeo,[Z|T],Resto),
                 atom_chars(Silaba,Silabeo),
                 silabear_auxEspañol(Resto,Silaba_resto).









% Letras en español




vocal_Español(i).
vocal_Español(u).
vocal_Español(a).
vocal_Español(e).
vocal_Español(o).
vocal_Español(ü).

vocalEspañol(C):-
                 vocal_Español(C),
                 !.

vocalEspañol(C):-
                 letras_equivalentesEspañol(A,C),
                 vocal_Español(A),
                 !.

letras_equivalentesEspañol(a,á).
letras_equivalentesEspañol(e,é).
letras_equivalentesEspañol(i,í).
letras_equivalentesEspañol(o,ó).
letras_equivalentesEspañol(u,ú).

letras_equivalentesEspañol(A,A).

cons_fEspañol(r).
cons_fEspañol(s).
cons_fEspañol(l).
cons_fEspañol(n).
cons_fEspañol(d).
cons_fEspañol(z).

consFinalTotal(s).

bilabial(p).
bilabial(b).

liquida(r).
liquida(l).

%% Consontantes emparejadas

% b

consParejaEspañol(b,[b,Liquida|_]):-
                              liquida(Liquida).

% f

consParejaEspañol(f,[f,Liquida|_]):-
                              liquida(Liquida).

% g

consParejaEspañol(g,[g,Liquida|_]):-
                              liquida(Liquida).

% c

consParejaEspañol(c,[c,Liquida|_]):-
                              liquida(Liquida).

% g

consParejaEspañol(p,[p,Liquida|_]):-
                              liquida(Liquida).

% d

consParejaEspañol(d,[d,r|_]).

% t

consParejaEspañol(t,[t,r|_]).

% ch

consParejaEspañol(c,[c,h|_]).

% ll

consParejaEspañol(l,[l,l|_]).

% rr
% porque entonces acepta rrato. Mejor que lo considere car-ro.
% consParejaEspañol(r,[r,r|_]).

% qu

consParejaEspañol(q,[q,u|_]).

% gu

consParejaEspañol(g,[g,u|_]).



%% Vocales emparejadas: DIPTONGOS

diptongoEspañol(Vocal_a,[Vocal_a,i|_]):-
                                        letras_equivalentesEspañol(a,Vocal_a).
diptongoEspañol(Vocal_a,[Vocal_a,u|_]):-
                                        letras_equivalentesEspañol(a,Vocal_a).
diptongoEspañol(a,[a,y|_]).

diptongoEspañol(Vocal_e,[Vocal_e,i|_]):-
                                         letras_equivalentesEspañol(e,Vocal_e).
diptongoEspañol(Vocal_e,[Vocal_e,u|_]):-
                                         letras_equivalentesEspañol(e,Vocal_e).
diptongoEspañol(e,[e,y|_]).

diptongoEspañol(i,[i,Vocal_a|_]):-
                                   letras_equivalentesEspañol(a,Vocal_a).


diptongoEspañol(i,[i,Vocal_e|_]):-
                                  letras_equivalentesEspañol(e,Vocal_e).

diptongoEspañol(i,[i,Vocal_o|_]):-
                                  letras_equivalentesEspañol(o,Vocal_o).
                                  
diptongoEspañol(i,[i,Vocal_u|_]):-
                                  letras_equivalentesEspañol(u,Vocal_u).

diptongoEspañol(Vocal_o,[Vocal_o,i|_]):-
                                        letras_equivalentesEspañol(o,Vocal_o).

diptongoEspañol(o,[o,y|_]).

diptongoEspañol(u,[u,Vocal_a|_]):-
                                     letras_equivalentesEspañol(a,Vocal_a).

diptongoEspañol(u,[u,Vocal_e|_]):-
                                   letras_equivalentesEspañol(e,Vocal_e).
diptongoEspañol(u,[u,Vocal_i|_]):-
                                   letras_equivalentesEspañol(i,Vocal_i).
diptongoEspañol(u,[u,Vocal_o|_]):-
                                    letras_equivalentesEspañol(o,Vocal_o).
diptongoEspañol(ü,[ü,Vocal_e|_]):-
                                    letras_equivalentesEspañol(e,Vocal_e).
diptongoEspañol(ü,[ü,Vocal_i|_]):-
                                  letras_equivalentesEspañol(i,Vocal_i).

diptongoEspañol(u,[u,y|_]).

hiatoEspañol(Vocal_a,[Vocal_a,Vocal_e|_]):-
                               letras_equivalentesEspañol(a,Vocal_a),
                               letras_equivalentesEspañol(e,Vocal_e).

hiatoEspañol(a,[a,í|_]).

hiatoEspañol(Vocal_a,[Vocal_a,Vocal_o|_]):-
                               letras_equivalentesEspañol(a,Vocal_a),
                               letras_equivalentesEspañol(o,Vocal_o).

hiatoEspañol(a,[a,ú|_]).

hiatoEspañol(Vocal_a,[Vocal_a,Vocal_a|_]):-
                               letras_equivalentesEspañol(a,Vocal_a),
                               letras_equivalentesEspañol(a,Vocal_a).

hiatoEspañol(Vocal_e,[Vocal_e,Vocal_a|_]):-
                               letras_equivalentesEspañol(e,Vocal_e),
                               letras_equivalentesEspañol(a,Vocal_a).

hiatoEspañol(Vocal_e,[Vocal_e,Vocal_e|_]):-
                               letras_equivalentesEspañol(e,Vocal_e),
                               letras_equivalentesEspañol(e,Vocal_e).

hiatoEspañol(e,[e,í|_]).

hiatoEspañol(Vocal_e,[Vocal_e,Vocal_o|_]):-
                               letras_equivalentesEspañol(e,Vocal_e),
                               letras_equivalentesEspañol(o,Vocal_o).

hiatoEspañol(e,[e,ú|_]).

hiatoEspañol(í,[í,a|_]).

hiatoEspañol(í,[í,e|_]).

hiatoEspañol(í,[í,o|_]).

hiatoEspañol(Vocal_o,[Vocal_o,Vocal_a|_]):-
                               letras_equivalentesEspañol(o,Vocal_o),
                               letras_equivalentesEspañol(a,Vocal_a).

hiatoEspañol(Vocal_o,[Vocal_o,Vocal_e|_]):-
                               letras_equivalentesEspañol(o,Vocal_o),
                               letras_equivalentesEspañol(e,Vocal_e).

hiatoEspañol(Vocal_o,[Vocal_o,í|_]):-
                               letras_equivalentesEspañol(o,Vocal_o).

hiatoEspañol(Vocal_o,[Vocal_o,Vocal_o2|_]):-
                               letras_equivalentesEspañol(o,Vocal_o),
                               letras_equivalentesEspañol(o,Vocal_o2).

hiatoEspañol(ú,[ú,a|_]).

hiatoEspañol(ú,[ú,o|_]).



triptongoEspañol(i,[i,Vocal_a,i]):-
                                             letras_equivalentesEspañol(a,Vocal_a).

triptongoEspañol(i,[i,Vocal_e,i]):-
                                             letras_equivalentesEspañol(e,Vocal_e).

triptongoEspañol(u,[u,Vocal_a,y]):-
                                               letras_equivalentesEspañol(a,Vocal_a).

triptongoEspañol(u,[u,á,i]).

triptongoEspañol(u,[u,e,y]).
















%% Consontantes pseudofinales: contextos

% Letra m

pseudocons_fEspañol(m,[m,Bilabial|_]):-
                                  bilabial(Bilabial).


pseudocons_fEspañol(m,[m,n|_]).



% Letra g


pseudocons_fEspañol(g,[g,n|_]).


% Letra p

dentalEspañol(t).
dentalEspañol(c).

pseudocons_fEspañol(p,[p,Dentales|_]):-
                                  dentalEspañol(Dentales).




% Letra x

pseudocons_fEspañol(x,[x,t|_]).


% Letra c

pseudocons_fEspañol(c,[c,c|_]).



pseudocons_fEspañol(c,[c,t|_]).




consEspañol(b).
consEspañol(c).
consEspañol(d).
consEspañol(f).
consEspañol(g).
consEspañol(h).
consEspañol(j).
consEspañol(k).
consEspañol(l).
consEspañol(m).
consEspañol(n).
consEspañol(ñ).
consEspañol(p).
consEspañol(r).
consEspañol(s).
consEspañol(t).
consEspañol(v).
consEspañol(x).
consEspañol(y).
consEspañol(z).





% Silabas en español

% posibilidades/4: letra que encabeza la silaba/silaba/letras que siguen/resto de la palabra (sin la silaba que acabamos de aislar, pero si con el contexto.

%% PREFIJOS

% sub

posibilidadesEspañol(s,[s,u,b,s],[s,u,b,s,Cons|W],Resto):-
                                                                consEspañol(Cons),
                                                                append([s,u,b,s],Resto,[s,u,b,s,Cons|W]),
                                                                !.

posibilidadesEspañol(s,[s,u,b],[s,u,b,Cons|W],Resto):-
                                                                consEspañol(Cons),
                                                                append([s,u,b],Resto,[s,u,b,Cons|W]),
                                                              !.



% ob

posibilidadesEspañol(o,[o,b,s],[o,b,s,Cons|W],Resto):-
                                                                consEspañol(Cons),
                                                                append([o,b,s],Resto,[o,b,s,Cons|W]),
                                                                 !.

posibilidadesEspañol(o,[o,b],[o,b,Cons|W],Resto):-
                                                                consEspañol(Cons),
                                                                not(consParejaEspañol(b,[b,Cons])),
                                                                append([o,b],Resto,[o,b,Cons|W]),
                                                                 !.


%ab

posibilidadesEspañol(a,[a,b,s],[a,b,s,Cons|W],Resto):-
                                                                consEspañol(Cons),
                                                                append([a,b,s],Resto,[a,b,s,Cons|W]),
                                                                !.

posibilidadesEspañol(a,[a,b],[a,b,Cons|W],Resto):-
                                                               consEspañol(Cons),
                                                                not(consParejaEspañol(b,[b,Cons])),
                                                                 append([a,b],Resto,[a,b,Cons|W]),
                                                                !.

%ex

posibilidadesEspañol(e,[e,x],[e,x,Cons|W],Resto):-
                                                                consEspañol(Cons),
                                                                append([e,x],Resto,[e,x,Cons|W]),
                                                                 !.






%%% silabas que empiezan por vocal

%% OJO PARA TODO EL CÓDIGO QUE SIGUE: OI, AU y AI son los unicos dipt que forman dipt tal cual. Los desmas son extranj: comprobar


% a

posibilidadesEspañol(Vocal,[Vocal],[Vocal],Resto):-
                                                                vocalEspañol(Vocal),
                                                                append([Vocal],Resto,[Vocal]),
                                                                !.

posibilidadesEspañol(y,[y],[y],Resto):-
                                                               append([y],Resto,[y]),
                                                                !.

% a-la

posibilidadesEspañol(Vocal,[Vocal],[Vocal,Cons,Vocal1|W],Resto):-
                                                                vocalEspañol(Vocal),
                                                                consEspañol(Cons),
                                                                vocalEspañol(Vocal1),
                                                                append([Vocal],Resto,[Vocal,Cons,Vocal1|W]),
                                                                !.

% a-pro-xi-mar-se; a-dri

posibilidadesEspañol(Vocal,[Vocal],[Vocal,ConsPareja,Cons_tr,Vocal2|W],Resto):-
                                                                 vocalEspañol(Vocal),
                                                                 consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                                               append([Vocal],Resto,[Vocal,ConsPareja,Cons_tr,Vocal2|W]),
                                                                !.



% a-e-re-o

posibilidadesEspañol(Vocal,[Vocal],[Vocal,Vocal2|W],Resto):-
                                                                hiatoEspañol(Vocal,[Vocal,Vocal2|_]),
                                                                 append([Vocal],Resto,[Vocal,Vocal2|W]),
                                                                 !.

% en

posibilidadesEspañol(Vocal,[Vocal,Cons_f],[Vocal,Cons_f],Resto):-
                                                                vocalEspañol(Vocal),
                                                                cons_fEspañol(Cons_f),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f]),
                                                                !.
% is-la

posibilidadesEspañol(Vocal,[Vocal,Cons_f],[Vocal,Cons_f,ConsBis,VocalBis|W],Resto):-
                                                                 vocalEspañol(Vocal),
                                                                (cons_fEspañol(Cons_f);pseudocons_fEspañol(Cons_f,[Cons_f,ConsBis|W])),
                                                                consEspañol(ConsBis),
                                                                vocalEspañol(VocalBis),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f,ConsBis,VocalBis|W]),
                                                                !.


% antro, an-cla

posibilidadesEspañol(Vocal,[Vocal,Cons_f],[Vocal,Cons_f,ConsPareja,Cons_tr|W],Resto):-
                                                                 vocalEspañol(Vocal),
                                                                (cons_fEspañol(Cons_f);pseudocons_fEspañol(Cons_f,[Cons_f,ConsPareja|_])),
                                                                consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f,ConsPareja,Cons_tr|W]),
                                                                !.

%ins-tau-rar

posibilidadesEspañol(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal,ConsBis|W],Resto):-
                                                                 vocalEspañol(Vocal),
                                                                cons_fEspañol(Cons_f),
                                                                 consFinalTotal(ConsFinalTotal),
                                                                consEspañol(ConsBis),
                                                                append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal,ConsBis|W]),
                                                                !.



% ay, uy: unicas palabras encontradas con dipt que solo son eso: el dipt, pero meto todos los casos pq nada lo prohibe. No meto el dipt ascendente ni el doble pq no parece probable

posibilidadesEspañol(Vocal,[Vocal,Vocal2],[Vocal,Vocal2],Resto):-
                                                                diptongoEspañol(Vocal,[Vocal,Vocal2|_]),
                                                                append([Vocal,Vocal2],Resto,[Vocal,Vocal2]),
                                                                !.

% ai-rar, au-lar, eusebio

posibilidadesEspañol(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,Cons,Vocal1|W],Resto):-
                                                                        diptongoEspañol(Vocal,[Vocal,Vocal2|_]),
                                                                        consEspañol(Cons),
                                                                        vocalEspañol(Vocal1),
                                                                        append([Vocal,Vocal2],Resto,[Vocal,Vocal2,Cons,Vocal1|W]),
                                                                         !.


% ai-trar

posibilidadesEspañol(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,ConsPareja,Cons_tr|W],Resto):-
                                                                        diptongoEspañol(Vocal,[Vocal,Vocal2|_]),
                                                                        consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr]),
                                                                        append([Vocal,Vocal2],Resto,[Vocal,Vocal2,ConsPareja,Cons_tr|W]),
                                                                         !.




% a-ma-ri-AIS

posibilidadesEspañol(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f],Resto):-
                                                                diptongoEspañol(Vocal,[Vocal,Vocal2|_]),
                                                                cons_fEspañol(Cons_f),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f]),
                                                                !.


% AIS-lar

posibilidadesEspañol(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f,Cons|W],Resto):-
                                                                diptongoEspañol(Vocal,[Vocal,Vocal2|_]),
                                                                (cons_fEspañol(Cons_f);pseudocons_fEspañol(Cons_f,[Cons_f,Cons|_])),
                                                                consEspañol(Cons),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f,Cons|W]),
                                                                !.

% aun-que

posibilidadesEspañol(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f,ConsPareja,Trabada|W],Resto):-
                                                                diptongoEspañol(Vocal,[Vocal,Vocal2|_]),
                                                                (cons_fEspañol(Cons_f);pseudocons_fEspañol(Cons_f,[Cons_f,ConsPareja|_])),
                                                                consParejaEspañol(ConsPareja,[ConsPareja,Trabada|_]),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f,ConsPareja,Trabada|W]),
                                                                !.

% ea: hiato aislado. Solo encontrado "ea", pero meto todo porque nada lo impide

posibilidadesEspañol(Vocal,[Vocal],[Vocal,Vocal2],Resto):-
                                                               hiatoEspañol(Vocal,[Vocal,Vocal2|_]),
                                                                append([Vocal],Resto,[Vocal,Vocal2]),
                                                                !.

% posibilidades para CONS+VOCAL AISLADO

posibilidadesEspañol(Cons,[Cons,Vocal],[Cons,Vocal],Resto):-
                                          consEspañol(Cons),
                                          vocalEspañol(Vocal),
                                          append([Cons,Vocal],Resto,[Cons,Vocal]),
                                          !.

% posibilidaddes para CONS+VOCAL EN PALABRA


posibilidadesEspañol(Cons,[Cons,Vocal],[Cons,Vocal,Cons1,Vocal1|W],Resto):-
                                          consEspañol(Cons),
                                          vocalEspañol(Vocal),
                                          consEspañol(Cons1),
                                          vocalEspañol(Vocal1),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Cons1,Vocal1|W]),
                                          !.

% posibilidaddes para CONS+VOCAL EN PALABRA: HIATO

posibilidadesEspañol(Cons,[Cons,Vocal],[Cons,Vocal,Vocal2|W],Resto):-
                                          consEspañol(Cons),
                                         hiatoEspañol(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Vocal2|W]),
                                          !.


% posibilidades para CONS+VOCAL+VOCAL AISLADO: HIATO

posibilidadesEspañol(Cons,[Cons,Vocal],[Cons,Vocal,Vocal2],Resto):-
                                          consEspañol(Cons),
                                          hiatoEspañol(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Vocal2]),
                                          !.

%% Caso particular: trabadas en mitad de palabra

% nu-blar  pa-dre

posibilidadesEspañol(Cons,[Cons,Vocal],[Cons,Vocal,ConsPareja,Cons_tr|W],Resto):-
                                          consEspañol(Cons),
                                          consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr]),
                                          vocalEspañol(Vocal),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,ConsPareja,Cons_tr|W]),
                                          !.


% CONS+VOCAL+CONS_FIN EN FINAL DE PALABRA: del-fin


posibilidadesEspañol(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f],Resto):-
                                          consEspañol(Cons),
                                          vocalEspañol(Vocal),
                                          cons_fEspañol(Cons_f),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f]),
                                          !.

% CONS+VOCAL+CONS_F: pen-sar


posibilidadesEspañol(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f,Cons1,Vocal1|W],Resto):-
                                          consEspañol(Cons),
                                          vocalEspañol(Vocal),
                                          (cons_fEspañol(Cons_f);pseudocons_fEspañol(Cons_f,[Cons_f,Cons1|_])),
                                          consEspañol(Cons1),
                                          vocalEspañol(Vocal1),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f,Cons1,Vocal1|W]),
                                          !.

% CONS+VOCAL+CONS_F: pen-trar


posibilidadesEspañol(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f,ConsPareja,Cons_tr|W],Resto):-
                                          consEspañol(Cons),
                                          vocalEspañol(Vocal),
                                          (cons_fEspañol(Cons_f);pseudocons_fEspañol(Cons_f,[Cons_f,ConsTipo|W])),
                                          consParejaEspañol(ConsPareja,[ConsPareja,Cons_trEspanol]),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f,ConsPareja,Cons_tr|W]),
                                          !.

% CONS-truir


posibilidadesEspañol(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal],[Cons,Vocal,Cons_f,ConsFinalTotal,Cons1|W],Resto):-
                                          consEspañol(Cons),
                                          vocalEspañol(Vocal),
                                          cons_fEspañol(Cons_f),
                                          consFinalTotal(ConsFinalTotal),
                                          consEspañol(Cons1),
                                          append([Cons,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,Cons1|W]),
                                          !.


% posibilidades para CONS+VOCAL+VOCAL AISLADO:

posibilidadesEspañol(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2],Resto):-
                                          consEspañol(Cons),
                                          diptongoEspañol(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2]),
                                          !.

% posibilidaddes para CONS+VOCAL+VOCAL EN PALABRA:

posibilidadesEspañol(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2,Cons1,Vocal1|W],Resto):-
                                          consEspañol(Cons),
                                          diptongoEspañol(Vocal,[Vocal,Vocal2|_]),
                                          consEspañol(Cons1),
                                          vocalEspañol(Vocal1),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2,Cons1,Vocal1|W]),
                                          !.
% nai-blar; nai-trar

posibilidadesEspañol(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2,ConsPareja,Cons_tr|W],Resto):-
                                          consEspañol(Cons),
                                          diptongoEspañol(Vocal,[Vocal,Vocal2|_]),
                                          consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2,ConsPareja,Cons_tr|W]),
                                          !.

% CONS+VOCAL+VOCAL+CONS_FIN con dipt descendente AISLADO: a-ma-bais

posibilidadesEspañol(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f],Resto):-
                                          consEspañol(Cons),
                                          diptongoEspañol(Vocal,[Vocal,Vocal2|_]),
                                          cons_fEspañol(Cons_f),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f]),
                                          !.


% CONS+VOCAL+VOCAL+CONS_F: DIPT DESCENDENTE mais-car



posibilidadesEspañol(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f,Cons1,Vocal1|W],Resto):-
                                          consEspañol(Cons),
                                          diptongoEspañol(Vocal,[Vocal,Vocal2|_]),
                                          (cons_fEspañol(Cons_f);pseudocons_fEspañol(Cons_f,[Cons_f,Cons1|_])),
                                          consEspañol(Cons1),
                                          vocalEspañol(Vocal1),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f,Cons1,Vocal1|W]),
                                          !.

% CONS+VOCAL+VOCAL+CONS_F: DIPT DESCENDENTE mais-bla



posibilidadesEspañol(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f,ConsTipo,Cons_tr|W],Resto):-
                                          consEspañol(Cons),
                                          diptongoEspañol(Vocal,[Vocal,Vocal2|_]),
                                          (cons_fEspañol(Cons_f);pseudocons_fEspañol(Cons_f,[Cons_f,ConsTipo|_])),
                                          consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsPareja,Cons_tr|W]),
                                          !.

% CONS+VOCAL+VOCAL+CONS_F*CONSFINALTOTAL: DIPT DESCENDENTE mains-bla



posibilidadesEspañol(Cons,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal,Cons1|W],Resto):-
                                          consEspañol(Cons),
                                          diptongoEspañol(Vocal,[Vocal,Vocal2|_]),
                                          cons_fEspañol(Cons_f),
                                          consEspañol(Cons1),
                                          append([Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal,Cons1|W]),
                                          !.

% bla

posibilidadesEspañol(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal],Resto):-
                                          consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalEspañol(Vocal),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal]),
                                          !.

% gra-pa

posibilidadesEspañol(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,Cons,Vocal1|W],Resto):-
                                          consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalEspañol(Vocal),
                                          vocalEspañol(Vocal1),
                                          consEspañol(Cons),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,Cons,Vocal1|W]),
                                          !.


% pro-e-mio

posibilidadesEspañol(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,Vocal2|W],Resto):-
                                          consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          hiatoEspañol(Vocal,[Vocal,Vocal2|_]),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2|W]),
                                          !.



% bla-bla

posibilidadesEspañol(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,ConsPareja1,Cons_trEspanol1|W],Resto):-
                                          consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalEspañol(Vocal),
                                          consParejaEspañol(ConsPareja1,[ConsPareja1,Cons_trEspanol1|_]),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,ConsPareja1,Cons_trEspanol1|W]),
                                          !.

% blas

posibilidadesEspañol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f],Resto):-
                                          consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalEspañol(Vocal),
                                          cons_fEspañol(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f]),
                                          !.


% blan-ca

posibilidadesEspañol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f,Cons1,Vocal1|W],Resto):-
                                          consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalEspañol(Vocal),
                                          vocalEspañol(Vocal1),
                                          (cons_fEspañol(Cons_f);pseudocons_fEspañol(Cons_f,[Cons_f,Cons1|_])),
                                          consEspañol(Cons1),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,Cons1,Vocal1|W]),
                                          !.

% blan-bla

posibilidadesEspañol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsPareja1,Cons_trEspanol1|W],Resto):-
                                          consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalEspañol(Vocal),
                                          (cons_fEspañol(Cons_f);pseudocons_fEspañol(Cons_f,[Cons_f,ConsTipo|_])),
                                          consParejaEspañol(ConsPareja1,[ConsPareja1,Cons_trEspanol1|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsPareja1,Cons_trEspanol1|W]),
                                          !.




% blans-ca; blans-bla

posibilidadesEspañol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal,Cons1|W],Resto):-
                                          consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalEspañol(Vocal),
                                          cons_fEspañol(Cons_f),
                                          consFinalTotal(ConsFinalTotal),
                                          consEspañol(Cons1),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal,Cons1|W]),
                                          !.


% blau?

posibilidadesEspañol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2],Resto):-
                                          consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoEspañol(Vocal,[Vocal,Vocal2|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2]),
                                          !.

% brai-le

posibilidadesEspañol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons,Vocal1|W],Resto):-
                                          consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoEspañol(Vocal,[Vocal,Vocal2|_]),
                                          vocalEspañol(Vocal1),
                                          consEspañol(Cons),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons,Vocal1|W]),
                                          !.


% blau-bla

posibilidadesEspañol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2,ConsParejaBis,Cons_trEspanolBis|W],Resto):-
                                          consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoEspañol(Vocal,[Vocal,Vocal2|_]),
                                          consParejaEspañol(ConsParejaBis,[ConsParejaBis,Cons_trEspanolBis|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,ConsParejaBis,Cons_trEspanolBis|W]),
                                          !.

% blaun

posibilidadesEspañol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto):-
                                          consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoEspañol(Vocal,[Vocal,Vocal2|_]),
                                          cons_fEspañol(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f]),
                                       !.

% brains-le

posibilidadesEspañol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal,Cons|W],Resto):-
                                          consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoEspañol(Vocal,[Vocal,Vocal2|_]),
                                          consEspañol(Cons),
                                          consFinalTotal(ConsFinalTotal),
                                          (cons_fEspañol(Cons_f);pseudocons_fEspañol(Cons_f,[Cons_f,Cons|_])),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal,Cons|W]),
                                          !.

% brain-le

posibilidadesEspañol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,Cons|W],Resto):-
                                          consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoEspañol(Vocal,[Vocal,Vocal2|_]),
                                          consEspañol(Cons),
                                          (cons_fEspañol(Cons_f);pseudocons_fEspañol(Cons_f,[Cons_f,Cons|_])),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,Cons|W]),
                                          !.






% Triptongo

posibilidadesEspañol(Cons,[Cons,Vocal,Vocal2,Vocal3],[Cons,Vocal,Vocal2,Vocal3],Resto):-
                                          consEspañol(Cons),
                                          triptongoEspañol(Vocal,[Vocal,Vocal2,Vocal3]),
                                          append([Cons,Vocal,Vocal2,Vocal3],Resto,[Cons,Vocal,Vocal2,Vocal3]),
                                          !.

% Triptongo en cons_trEspañol

posibilidadesEspañol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],Resto):-
                                          consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                           triptongoEspañol(Vocal,[Vocal,Vocal2,Vocal3]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3]),
                                          !.

% Triptongo +cons final

posibilidadesEspañol(Cons,[Cons,Vocal,Vocal2,Vocal3,Cons_f],[Cons,Vocal,Vocal2,Vocal3,Cons_f],Resto):-
                                          consEspañol(Cons),
                                           triptongoEspañol(Vocal,[Vocal,Vocal2,Vocal3]),
                                          cons_fEspañol(Cons_f),
                                          append([Cons,Vocal,Vocal2,Vocal3,Cons_f],Resto,[Cons,Vocal,Vocal2,Vocal3,Cons_f]),
                                          !.

% Triptongo en cons_trEspañol + cons final

posibilidadesEspañol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],Resto):-
                                          consParejaEspañol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                           triptongoEspañol(Vocal,[Vocal,Vocal2,Vocal3]),
                                          cons_fEspañol(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f]),
                                          !.


