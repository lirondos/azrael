% Autor:
% Datum: 22/11/2009




% X tiene que ir "" para que prolog lo lea como una lista de codigos ANSII. El predicado espa�ol/1 dice que algo es espa�ol si hacemos el procedimiento lista/2 y el comprobar_palabras/1.




espa�ol(X,Restos,Origen):-
                           espa�ol(X,Restos),
                           origen_etimologicoEspa�ol(Restos,Origen).

origen_etimologicoEspa�ol([],[]).

origen_etimologicoEspa�ol([Restos|MasRestos],[Origen|MasOrigen]):-
                                                 atom_chars(Restos,Deletreo),
                                                 combinacionesEspa�ol(Restos,Deletreo,Origen),
                                                 !,
                                                 origen_etimologicoEspa�ol(MasRestos,MasOrigen).

combinacionesEspa�ol(Restos,[J],desconocido(Restos)).


combinacionesEspa�ol(Restos,[t,l|W],griego_o_nahuatl(Restos)).

combinacionesEspa�ol(Restos,[t,m|W],griego(Restos)).

combinacionesEspa�ol(Restos,[t,n|W],griego(Restos)).

combinacionesEspa�ol(Restos,[p,n|W],griego(Restos)).

combinacionesEspa�ol(Restos,[f,t|W],griego_o_arabe(Restos)).

combinacionesEspa�ol(Restos,[g,m|W],griego(Restos)).

combinacionesEspa�ol(Restos,[g,d|W],griego(Restos)).

combinacionesEspa�ol(Restos,[c,n|W],griego(Restos)).

combinacionesEspa�ol(Restos,[p,s|W],griego_o_latin(Restos)).

combinacionesEspa�ol(Restos,[u,a,i|W],amerindio_o_catalan(Restos)).



combinacionesEspa�ol(Restos,[X,Y|W],Origen):-
                                       combinacionesEspa�ol(Restos,[Y|W],Origen).



espa�ol(X,Restos):-
               transcribir(X,W),
               !,
               lista(W,Y),
               comprobar_palabrasEspa�ol(Y,N),
               recuperar_descartesEspa�ol(N,Restos).

recuperar_descartesEspa�ol([],[]).

recuperar_descartesEspa�ol([N|M],[Restos|MasRestos]):-
               atom_codes(Restos,N),
               recuperar_descartesEspa�ol(M,MasRestos).


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







comprobar_palabrasEspa�ol([],[]).


comprobar_palabrasEspa�ol([Z|W],Sobras):-
                       silabearEspa�ol(Z,F),
                       comprobar_palabrasEspa�ol(W,Sobras),
                       !.

comprobar_palabrasEspa�ol([Z|W],[Z|Sobras]):-
                       not(silabearEspa�ol(Z,F)),
                       comprobar_palabrasEspa�ol(W,Sobras),
                       !.








% El proceso de silabeo se incia con la transformacion de lista de codigo ANSII a letras sueltas para poder silabear, y lo manda ya en forma de letras al proceso auxiliar de silabeo


silabearEspa�ol(X,Z):-
               atom_codes(Y,X),
               atom_chars(Y,W),
               olfatear_espa�ol(W),
                           contar_acentos(W,Numero),
               numero_acentos(Numero),
               silabear_auxEspa�ol(W,Z).
                           

numero_acentos(Numero):-
                           Numero=<1.
                                                   
numero_acentos(Numero):-
                          Numero>1,
                          !,
                          fail.
                                                  

                                                  
                                        
               

vocal_tildadaEspa�ol(�).
vocal_tildadaEspa�ol(�).
vocal_tildadaEspa�ol(�).
vocal_tildadaEspa�ol(�).
vocal_tildadaEspa�ol(�).

               
contar_acentos([X|T],Numero):-
                                                contar_acentos(T,NumeroNuevo),
                                                vocal_tildadaEspa�ol(X),
                                           Numero is NumeroNuevo+1,
                                           !.
                                           

                       
contar_acentos([X|T],Numero):-
                       contar_acentos(T,Numero).
                       

contar_acentos([],0).
               

olfatear_espa�ol([]).

olfatear_espa�ol([q,u,a|T]):-
                           !,
                           fail.
                           
olfatear_espa�ol([q,u,o|T]):-
                           !,
                           fail.
                           
olfatear_espa�ol([q,u,u|T]):-
                           !,
                           fail.
                           
olfatear_espa�ol([z,e|T]):-
                           !,
                           fail.
                           
olfatear_espa�ol([z,i|T]):-
                           !,
                           fail.
                           
olfatear_espa�ol([n,b|T]):-
                           !,
                           fail.
                           
olfatear_espa�ol([n,p|T]):-
                           !,
                           fail.
                           
olfatear_espa�ol([m,v|T]):-
                           !,
                           fail.
                           
olfatear_espa�ol([s,s|T]):-
                           !,
                           fail.
olfatear_espa�ol([d,d|T]):-
                           !,
                           fail.
                           

                           
olfatear_espa�ol([X|Y]):-
                 olfatear_espa�ol(Y).

silabeoEspa�ol(X,Z):-
               silabear_auxEspa�ol(X,Z).

% El proceso auxiliar de silabeo

silabear_auxEspa�ol([],[]).

silabear_auxEspa�ol([Z|T],[Silaba|Silaba_resto]):-
                 posibilidadesEspa�ol(Z,Silabeo,[Z|T],Resto),
                 atom_chars(Silaba,Silabeo),
                 silabear_auxEspa�ol(Resto,Silaba_resto).









% Letras en espa�ol




vocal_Espa�ol(i).
vocal_Espa�ol(u).
vocal_Espa�ol(a).
vocal_Espa�ol(e).
vocal_Espa�ol(o).
vocal_Espa�ol(�).

vocalEspa�ol(C):-
                 vocal_Espa�ol(C),
                 !.

vocalEspa�ol(C):-
                 letras_equivalentesEspa�ol(A,C),
                 vocal_Espa�ol(A),
                 !.

letras_equivalentesEspa�ol(a,�).
letras_equivalentesEspa�ol(e,�).
letras_equivalentesEspa�ol(i,�).
letras_equivalentesEspa�ol(o,�).
letras_equivalentesEspa�ol(u,�).

letras_equivalentesEspa�ol(A,A).

cons_fEspa�ol(r).
cons_fEspa�ol(s).
cons_fEspa�ol(l).
cons_fEspa�ol(n).
cons_fEspa�ol(d).
cons_fEspa�ol(z).

consFinalTotal(s).

bilabial(p).
bilabial(b).

liquida(r).
liquida(l).

%% Consontantes emparejadas

% b

consParejaEspa�ol(b,[b,Liquida|_]):-
                              liquida(Liquida).

% f

consParejaEspa�ol(f,[f,Liquida|_]):-
                              liquida(Liquida).

% g

consParejaEspa�ol(g,[g,Liquida|_]):-
                              liquida(Liquida).

% c

consParejaEspa�ol(c,[c,Liquida|_]):-
                              liquida(Liquida).

% g

consParejaEspa�ol(p,[p,Liquida|_]):-
                              liquida(Liquida).

% d

consParejaEspa�ol(d,[d,r|_]).

% t

consParejaEspa�ol(t,[t,r|_]).

% ch

consParejaEspa�ol(c,[c,h|_]).

% ll

consParejaEspa�ol(l,[l,l|_]).

% rr
% porque entonces acepta rrato. Mejor que lo considere car-ro.
% consParejaEspa�ol(r,[r,r|_]).

% qu

consParejaEspa�ol(q,[q,u|_]).

% gu

consParejaEspa�ol(g,[g,u|_]).



%% Vocales emparejadas: DIPTONGOS

diptongoEspa�ol(Vocal_a,[Vocal_a,i|_]):-
                                        letras_equivalentesEspa�ol(a,Vocal_a).
diptongoEspa�ol(Vocal_a,[Vocal_a,u|_]):-
                                        letras_equivalentesEspa�ol(a,Vocal_a).
diptongoEspa�ol(a,[a,y|_]).

diptongoEspa�ol(Vocal_e,[Vocal_e,i|_]):-
                                         letras_equivalentesEspa�ol(e,Vocal_e).
diptongoEspa�ol(Vocal_e,[Vocal_e,u|_]):-
                                         letras_equivalentesEspa�ol(e,Vocal_e).
diptongoEspa�ol(e,[e,y|_]).

diptongoEspa�ol(i,[i,Vocal_a|_]):-
                                   letras_equivalentesEspa�ol(a,Vocal_a).


diptongoEspa�ol(i,[i,Vocal_e|_]):-
                                  letras_equivalentesEspa�ol(e,Vocal_e).

diptongoEspa�ol(i,[i,Vocal_o|_]):-
                                  letras_equivalentesEspa�ol(o,Vocal_o).
                                  
diptongoEspa�ol(i,[i,Vocal_u|_]):-
                                  letras_equivalentesEspa�ol(u,Vocal_u).

diptongoEspa�ol(Vocal_o,[Vocal_o,i|_]):-
                                        letras_equivalentesEspa�ol(o,Vocal_o).

diptongoEspa�ol(o,[o,y|_]).

diptongoEspa�ol(u,[u,Vocal_a|_]):-
                                     letras_equivalentesEspa�ol(a,Vocal_a).

diptongoEspa�ol(u,[u,Vocal_e|_]):-
                                   letras_equivalentesEspa�ol(e,Vocal_e).
diptongoEspa�ol(u,[u,Vocal_i|_]):-
                                   letras_equivalentesEspa�ol(i,Vocal_i).
diptongoEspa�ol(u,[u,Vocal_o|_]):-
                                    letras_equivalentesEspa�ol(o,Vocal_o).
diptongoEspa�ol(�,[�,Vocal_e|_]):-
                                    letras_equivalentesEspa�ol(e,Vocal_e).
diptongoEspa�ol(�,[�,Vocal_i|_]):-
                                  letras_equivalentesEspa�ol(i,Vocal_i).

diptongoEspa�ol(u,[u,y|_]).

hiatoEspa�ol(Vocal_a,[Vocal_a,Vocal_e|_]):-
                               letras_equivalentesEspa�ol(a,Vocal_a),
                               letras_equivalentesEspa�ol(e,Vocal_e).

hiatoEspa�ol(a,[a,�|_]).

hiatoEspa�ol(Vocal_a,[Vocal_a,Vocal_o|_]):-
                               letras_equivalentesEspa�ol(a,Vocal_a),
                               letras_equivalentesEspa�ol(o,Vocal_o).

hiatoEspa�ol(a,[a,�|_]).

hiatoEspa�ol(Vocal_a,[Vocal_a,Vocal_a|_]):-
                               letras_equivalentesEspa�ol(a,Vocal_a),
                               letras_equivalentesEspa�ol(a,Vocal_a).

hiatoEspa�ol(Vocal_e,[Vocal_e,Vocal_a|_]):-
                               letras_equivalentesEspa�ol(e,Vocal_e),
                               letras_equivalentesEspa�ol(a,Vocal_a).

hiatoEspa�ol(Vocal_e,[Vocal_e,Vocal_e|_]):-
                               letras_equivalentesEspa�ol(e,Vocal_e),
                               letras_equivalentesEspa�ol(e,Vocal_e).

hiatoEspa�ol(e,[e,�|_]).

hiatoEspa�ol(Vocal_e,[Vocal_e,Vocal_o|_]):-
                               letras_equivalentesEspa�ol(e,Vocal_e),
                               letras_equivalentesEspa�ol(o,Vocal_o).

hiatoEspa�ol(e,[e,�|_]).

hiatoEspa�ol(�,[�,a|_]).

hiatoEspa�ol(�,[�,e|_]).

hiatoEspa�ol(�,[�,o|_]).

hiatoEspa�ol(Vocal_o,[Vocal_o,Vocal_a|_]):-
                               letras_equivalentesEspa�ol(o,Vocal_o),
                               letras_equivalentesEspa�ol(a,Vocal_a).

hiatoEspa�ol(Vocal_o,[Vocal_o,Vocal_e|_]):-
                               letras_equivalentesEspa�ol(o,Vocal_o),
                               letras_equivalentesEspa�ol(e,Vocal_e).

hiatoEspa�ol(Vocal_o,[Vocal_o,�|_]):-
                               letras_equivalentesEspa�ol(o,Vocal_o).

hiatoEspa�ol(Vocal_o,[Vocal_o,Vocal_o2|_]):-
                               letras_equivalentesEspa�ol(o,Vocal_o),
                               letras_equivalentesEspa�ol(o,Vocal_o2).

hiatoEspa�ol(�,[�,a|_]).

hiatoEspa�ol(�,[�,o|_]).



triptongoEspa�ol(i,[i,Vocal_a,i]):-
                                             letras_equivalentesEspa�ol(a,Vocal_a).

triptongoEspa�ol(i,[i,Vocal_e,i]):-
                                             letras_equivalentesEspa�ol(e,Vocal_e).

triptongoEspa�ol(u,[u,Vocal_a,y]):-
                                               letras_equivalentesEspa�ol(a,Vocal_a).

triptongoEspa�ol(u,[u,�,i]).

triptongoEspa�ol(u,[u,e,y]).
















%% Consontantes pseudofinales: contextos

% Letra m

pseudocons_fEspa�ol(m,[m,Bilabial|_]):-
                                  bilabial(Bilabial).


pseudocons_fEspa�ol(m,[m,n|_]).



% Letra g


pseudocons_fEspa�ol(g,[g,n|_]).


% Letra p

dentalEspa�ol(t).
dentalEspa�ol(c).

pseudocons_fEspa�ol(p,[p,Dentales|_]):-
                                  dentalEspa�ol(Dentales).




% Letra x

pseudocons_fEspa�ol(x,[x,t|_]).


% Letra c

pseudocons_fEspa�ol(c,[c,c|_]).



pseudocons_fEspa�ol(c,[c,t|_]).




consEspa�ol(b).
consEspa�ol(c).
consEspa�ol(d).
consEspa�ol(f).
consEspa�ol(g).
consEspa�ol(h).
consEspa�ol(j).
consEspa�ol(k).
consEspa�ol(l).
consEspa�ol(m).
consEspa�ol(n).
consEspa�ol(�).
consEspa�ol(p).
consEspa�ol(r).
consEspa�ol(s).
consEspa�ol(t).
consEspa�ol(v).
consEspa�ol(x).
consEspa�ol(y).
consEspa�ol(z).





% Silabas en espa�ol

% posibilidades/4: letra que encabeza la silaba/silaba/letras que siguen/resto de la palabra (sin la silaba que acabamos de aislar, pero si con el contexto.

%% PREFIJOS

% sub

posibilidadesEspa�ol(s,[s,u,b,s],[s,u,b,s,Cons|W],Resto):-
                                                                consEspa�ol(Cons),
                                                                append([s,u,b,s],Resto,[s,u,b,s,Cons|W]),
                                                                !.

posibilidadesEspa�ol(s,[s,u,b],[s,u,b,Cons|W],Resto):-
                                                                consEspa�ol(Cons),
                                                                append([s,u,b],Resto,[s,u,b,Cons|W]),
                                                              !.



% ob

posibilidadesEspa�ol(o,[o,b,s],[o,b,s,Cons|W],Resto):-
                                                                consEspa�ol(Cons),
                                                                append([o,b,s],Resto,[o,b,s,Cons|W]),
                                                                 !.

posibilidadesEspa�ol(o,[o,b],[o,b,Cons|W],Resto):-
                                                                consEspa�ol(Cons),
                                                                not(consParejaEspa�ol(b,[b,Cons])),
                                                                append([o,b],Resto,[o,b,Cons|W]),
                                                                 !.


%ab

posibilidadesEspa�ol(a,[a,b,s],[a,b,s,Cons|W],Resto):-
                                                                consEspa�ol(Cons),
                                                                append([a,b,s],Resto,[a,b,s,Cons|W]),
                                                                !.

posibilidadesEspa�ol(a,[a,b],[a,b,Cons|W],Resto):-
                                                               consEspa�ol(Cons),
                                                                not(consParejaEspa�ol(b,[b,Cons])),
                                                                 append([a,b],Resto,[a,b,Cons|W]),
                                                                !.

%ex

posibilidadesEspa�ol(e,[e,x],[e,x,Cons|W],Resto):-
                                                                consEspa�ol(Cons),
                                                                append([e,x],Resto,[e,x,Cons|W]),
                                                                 !.






%%% silabas que empiezan por vocal

%% OJO PARA TODO EL C�DIGO QUE SIGUE: OI, AU y AI son los unicos dipt que forman dipt tal cual. Los desmas son extranj: comprobar


% a

posibilidadesEspa�ol(Vocal,[Vocal],[Vocal],Resto):-
                                                                vocalEspa�ol(Vocal),
                                                                append([Vocal],Resto,[Vocal]),
                                                                !.

posibilidadesEspa�ol(y,[y],[y],Resto):-
                                                               append([y],Resto,[y]),
                                                                !.

% a-la

posibilidadesEspa�ol(Vocal,[Vocal],[Vocal,Cons,Vocal1|W],Resto):-
                                                                vocalEspa�ol(Vocal),
                                                                consEspa�ol(Cons),
                                                                vocalEspa�ol(Vocal1),
                                                                append([Vocal],Resto,[Vocal,Cons,Vocal1|W]),
                                                                !.

% a-pro-xi-mar-se; a-dri

posibilidadesEspa�ol(Vocal,[Vocal],[Vocal,ConsPareja,Cons_tr,Vocal2|W],Resto):-
                                                                 vocalEspa�ol(Vocal),
                                                                 consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                                               append([Vocal],Resto,[Vocal,ConsPareja,Cons_tr,Vocal2|W]),
                                                                !.



% a-e-re-o

posibilidadesEspa�ol(Vocal,[Vocal],[Vocal,Vocal2|W],Resto):-
                                                                hiatoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                                                 append([Vocal],Resto,[Vocal,Vocal2|W]),
                                                                 !.

% en

posibilidadesEspa�ol(Vocal,[Vocal,Cons_f],[Vocal,Cons_f],Resto):-
                                                                vocalEspa�ol(Vocal),
                                                                cons_fEspa�ol(Cons_f),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f]),
                                                                !.
% is-la

posibilidadesEspa�ol(Vocal,[Vocal,Cons_f],[Vocal,Cons_f,ConsBis,VocalBis|W],Resto):-
                                                                 vocalEspa�ol(Vocal),
                                                                (cons_fEspa�ol(Cons_f);pseudocons_fEspa�ol(Cons_f,[Cons_f,ConsBis|W])),
                                                                consEspa�ol(ConsBis),
                                                                vocalEspa�ol(VocalBis),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f,ConsBis,VocalBis|W]),
                                                                !.


% antro, an-cla

posibilidadesEspa�ol(Vocal,[Vocal,Cons_f],[Vocal,Cons_f,ConsPareja,Cons_tr|W],Resto):-
                                                                 vocalEspa�ol(Vocal),
                                                                (cons_fEspa�ol(Cons_f);pseudocons_fEspa�ol(Cons_f,[Cons_f,ConsPareja|_])),
                                                                consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f,ConsPareja,Cons_tr|W]),
                                                                !.

%ins-tau-rar

posibilidadesEspa�ol(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal,ConsBis|W],Resto):-
                                                                 vocalEspa�ol(Vocal),
                                                                cons_fEspa�ol(Cons_f),
                                                                 consFinalTotal(ConsFinalTotal),
                                                                consEspa�ol(ConsBis),
                                                                append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal,ConsBis|W]),
                                                                !.



% ay, uy: unicas palabras encontradas con dipt que solo son eso: el dipt, pero meto todos los casos pq nada lo prohibe. No meto el dipt ascendente ni el doble pq no parece probable

posibilidadesEspa�ol(Vocal,[Vocal,Vocal2],[Vocal,Vocal2],Resto):-
                                                                diptongoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                                                append([Vocal,Vocal2],Resto,[Vocal,Vocal2]),
                                                                !.

% ai-rar, au-lar, eusebio

posibilidadesEspa�ol(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,Cons,Vocal1|W],Resto):-
                                                                        diptongoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                                                        consEspa�ol(Cons),
                                                                        vocalEspa�ol(Vocal1),
                                                                        append([Vocal,Vocal2],Resto,[Vocal,Vocal2,Cons,Vocal1|W]),
                                                                         !.


% ai-trar

posibilidadesEspa�ol(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,ConsPareja,Cons_tr|W],Resto):-
                                                                        diptongoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                                                        consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr]),
                                                                        append([Vocal,Vocal2],Resto,[Vocal,Vocal2,ConsPareja,Cons_tr|W]),
                                                                         !.




% a-ma-ri-AIS

posibilidadesEspa�ol(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f],Resto):-
                                                                diptongoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                                                cons_fEspa�ol(Cons_f),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f]),
                                                                !.


% AIS-lar

posibilidadesEspa�ol(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f,Cons|W],Resto):-
                                                                diptongoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                                                (cons_fEspa�ol(Cons_f);pseudocons_fEspa�ol(Cons_f,[Cons_f,Cons|_])),
                                                                consEspa�ol(Cons),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f,Cons|W]),
                                                                !.

% aun-que

posibilidadesEspa�ol(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f,ConsPareja,Trabada|W],Resto):-
                                                                diptongoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                                                (cons_fEspa�ol(Cons_f);pseudocons_fEspa�ol(Cons_f,[Cons_f,ConsPareja|_])),
                                                                consParejaEspa�ol(ConsPareja,[ConsPareja,Trabada|_]),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f,ConsPareja,Trabada|W]),
                                                                !.

% ea: hiato aislado. Solo encontrado "ea", pero meto todo porque nada lo impide

posibilidadesEspa�ol(Vocal,[Vocal],[Vocal,Vocal2],Resto):-
                                                               hiatoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                                                append([Vocal],Resto,[Vocal,Vocal2]),
                                                                !.

% posibilidades para CONS+VOCAL AISLADO

posibilidadesEspa�ol(Cons,[Cons,Vocal],[Cons,Vocal],Resto):-
                                          consEspa�ol(Cons),
                                          vocalEspa�ol(Vocal),
                                          append([Cons,Vocal],Resto,[Cons,Vocal]),
                                          !.

% posibilidaddes para CONS+VOCAL EN PALABRA


posibilidadesEspa�ol(Cons,[Cons,Vocal],[Cons,Vocal,Cons1,Vocal1|W],Resto):-
                                          consEspa�ol(Cons),
                                          vocalEspa�ol(Vocal),
                                          consEspa�ol(Cons1),
                                          vocalEspa�ol(Vocal1),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Cons1,Vocal1|W]),
                                          !.

% posibilidaddes para CONS+VOCAL EN PALABRA: HIATO

posibilidadesEspa�ol(Cons,[Cons,Vocal],[Cons,Vocal,Vocal2|W],Resto):-
                                          consEspa�ol(Cons),
                                         hiatoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Vocal2|W]),
                                          !.


% posibilidades para CONS+VOCAL+VOCAL AISLADO: HIATO

posibilidadesEspa�ol(Cons,[Cons,Vocal],[Cons,Vocal,Vocal2],Resto):-
                                          consEspa�ol(Cons),
                                          hiatoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Vocal2]),
                                          !.

%% Caso particular: trabadas en mitad de palabra

% nu-blar  pa-dre

posibilidadesEspa�ol(Cons,[Cons,Vocal],[Cons,Vocal,ConsPareja,Cons_tr|W],Resto):-
                                          consEspa�ol(Cons),
                                          consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr]),
                                          vocalEspa�ol(Vocal),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,ConsPareja,Cons_tr|W]),
                                          !.


% CONS+VOCAL+CONS_FIN EN FINAL DE PALABRA: del-fin


posibilidadesEspa�ol(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f],Resto):-
                                          consEspa�ol(Cons),
                                          vocalEspa�ol(Vocal),
                                          cons_fEspa�ol(Cons_f),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f]),
                                          !.

% CONS+VOCAL+CONS_F: pen-sar


posibilidadesEspa�ol(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f,Cons1,Vocal1|W],Resto):-
                                          consEspa�ol(Cons),
                                          vocalEspa�ol(Vocal),
                                          (cons_fEspa�ol(Cons_f);pseudocons_fEspa�ol(Cons_f,[Cons_f,Cons1|_])),
                                          consEspa�ol(Cons1),
                                          vocalEspa�ol(Vocal1),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f,Cons1,Vocal1|W]),
                                          !.

% CONS+VOCAL+CONS_F: pen-trar


posibilidadesEspa�ol(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f,ConsPareja,Cons_tr|W],Resto):-
                                          consEspa�ol(Cons),
                                          vocalEspa�ol(Vocal),
                                          (cons_fEspa�ol(Cons_f);pseudocons_fEspa�ol(Cons_f,[Cons_f,ConsTipo|W])),
                                          consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_trEspanol]),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f,ConsPareja,Cons_tr|W]),
                                          !.

% CONS-truir


posibilidadesEspa�ol(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal],[Cons,Vocal,Cons_f,ConsFinalTotal,Cons1|W],Resto):-
                                          consEspa�ol(Cons),
                                          vocalEspa�ol(Vocal),
                                          cons_fEspa�ol(Cons_f),
                                          consFinalTotal(ConsFinalTotal),
                                          consEspa�ol(Cons1),
                                          append([Cons,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,Cons1|W]),
                                          !.


% posibilidades para CONS+VOCAL+VOCAL AISLADO:

posibilidadesEspa�ol(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2],Resto):-
                                          consEspa�ol(Cons),
                                          diptongoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2]),
                                          !.

% posibilidaddes para CONS+VOCAL+VOCAL EN PALABRA:

posibilidadesEspa�ol(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2,Cons1,Vocal1|W],Resto):-
                                          consEspa�ol(Cons),
                                          diptongoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                          consEspa�ol(Cons1),
                                          vocalEspa�ol(Vocal1),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2,Cons1,Vocal1|W]),
                                          !.
% nai-blar; nai-trar

posibilidadesEspa�ol(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2,ConsPareja,Cons_tr|W],Resto):-
                                          consEspa�ol(Cons),
                                          diptongoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                          consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2,ConsPareja,Cons_tr|W]),
                                          !.

% CONS+VOCAL+VOCAL+CONS_FIN con dipt descendente AISLADO: a-ma-bais

posibilidadesEspa�ol(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f],Resto):-
                                          consEspa�ol(Cons),
                                          diptongoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                          cons_fEspa�ol(Cons_f),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f]),
                                          !.


% CONS+VOCAL+VOCAL+CONS_F: DIPT DESCENDENTE mais-car



posibilidadesEspa�ol(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f,Cons1,Vocal1|W],Resto):-
                                          consEspa�ol(Cons),
                                          diptongoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                          (cons_fEspa�ol(Cons_f);pseudocons_fEspa�ol(Cons_f,[Cons_f,Cons1|_])),
                                          consEspa�ol(Cons1),
                                          vocalEspa�ol(Vocal1),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f,Cons1,Vocal1|W]),
                                          !.

% CONS+VOCAL+VOCAL+CONS_F: DIPT DESCENDENTE mais-bla



posibilidadesEspa�ol(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f,ConsTipo,Cons_tr|W],Resto):-
                                          consEspa�ol(Cons),
                                          diptongoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                          (cons_fEspa�ol(Cons_f);pseudocons_fEspa�ol(Cons_f,[Cons_f,ConsTipo|_])),
                                          consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsPareja,Cons_tr|W]),
                                          !.

% CONS+VOCAL+VOCAL+CONS_F*CONSFINALTOTAL: DIPT DESCENDENTE mains-bla



posibilidadesEspa�ol(Cons,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal,Cons1|W],Resto):-
                                          consEspa�ol(Cons),
                                          diptongoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                          cons_fEspa�ol(Cons_f),
                                          consEspa�ol(Cons1),
                                          append([Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal,Cons1|W]),
                                          !.

% bla

posibilidadesEspa�ol(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal],Resto):-
                                          consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalEspa�ol(Vocal),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal]),
                                          !.

% gra-pa

posibilidadesEspa�ol(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,Cons,Vocal1|W],Resto):-
                                          consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalEspa�ol(Vocal),
                                          vocalEspa�ol(Vocal1),
                                          consEspa�ol(Cons),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,Cons,Vocal1|W]),
                                          !.


% pro-e-mio

posibilidadesEspa�ol(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,Vocal2|W],Resto):-
                                          consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          hiatoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2|W]),
                                          !.



% bla-bla

posibilidadesEspa�ol(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,ConsPareja1,Cons_trEspanol1|W],Resto):-
                                          consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalEspa�ol(Vocal),
                                          consParejaEspa�ol(ConsPareja1,[ConsPareja1,Cons_trEspanol1|_]),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,ConsPareja1,Cons_trEspanol1|W]),
                                          !.

% blas

posibilidadesEspa�ol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f],Resto):-
                                          consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalEspa�ol(Vocal),
                                          cons_fEspa�ol(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f]),
                                          !.


% blan-ca

posibilidadesEspa�ol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f,Cons1,Vocal1|W],Resto):-
                                          consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalEspa�ol(Vocal),
                                          vocalEspa�ol(Vocal1),
                                          (cons_fEspa�ol(Cons_f);pseudocons_fEspa�ol(Cons_f,[Cons_f,Cons1|_])),
                                          consEspa�ol(Cons1),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,Cons1,Vocal1|W]),
                                          !.

% blan-bla

posibilidadesEspa�ol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsPareja1,Cons_trEspanol1|W],Resto):-
                                          consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalEspa�ol(Vocal),
                                          (cons_fEspa�ol(Cons_f);pseudocons_fEspa�ol(Cons_f,[Cons_f,ConsTipo|_])),
                                          consParejaEspa�ol(ConsPareja1,[ConsPareja1,Cons_trEspanol1|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsPareja1,Cons_trEspanol1|W]),
                                          !.




% blans-ca; blans-bla

posibilidadesEspa�ol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal,Cons1|W],Resto):-
                                          consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalEspa�ol(Vocal),
                                          cons_fEspa�ol(Cons_f),
                                          consFinalTotal(ConsFinalTotal),
                                          consEspa�ol(Cons1),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal,Cons1|W]),
                                          !.


% blau?

posibilidadesEspa�ol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2],Resto):-
                                          consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2]),
                                          !.

% brai-le

posibilidadesEspa�ol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons,Vocal1|W],Resto):-
                                          consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                          vocalEspa�ol(Vocal1),
                                          consEspa�ol(Cons),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons,Vocal1|W]),
                                          !.


% blau-bla

posibilidadesEspa�ol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2,ConsParejaBis,Cons_trEspanolBis|W],Resto):-
                                          consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                          consParejaEspa�ol(ConsParejaBis,[ConsParejaBis,Cons_trEspanolBis|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,ConsParejaBis,Cons_trEspanolBis|W]),
                                          !.

% blaun

posibilidadesEspa�ol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto):-
                                          consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                          cons_fEspa�ol(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f]),
                                       !.

% brains-le

posibilidadesEspa�ol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal,Cons|W],Resto):-
                                          consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                          consEspa�ol(Cons),
                                          consFinalTotal(ConsFinalTotal),
                                          (cons_fEspa�ol(Cons_f);pseudocons_fEspa�ol(Cons_f,[Cons_f,Cons|_])),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal,Cons|W]),
                                          !.

% brain-le

posibilidadesEspa�ol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,Cons|W],Resto):-
                                          consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoEspa�ol(Vocal,[Vocal,Vocal2|_]),
                                          consEspa�ol(Cons),
                                          (cons_fEspa�ol(Cons_f);pseudocons_fEspa�ol(Cons_f,[Cons_f,Cons|_])),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,Cons|W]),
                                          !.






% Triptongo

posibilidadesEspa�ol(Cons,[Cons,Vocal,Vocal2,Vocal3],[Cons,Vocal,Vocal2,Vocal3],Resto):-
                                          consEspa�ol(Cons),
                                          triptongoEspa�ol(Vocal,[Vocal,Vocal2,Vocal3]),
                                          append([Cons,Vocal,Vocal2,Vocal3],Resto,[Cons,Vocal,Vocal2,Vocal3]),
                                          !.

% Triptongo en cons_trEspa�ol

posibilidadesEspa�ol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],Resto):-
                                          consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                           triptongoEspa�ol(Vocal,[Vocal,Vocal2,Vocal3]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3]),
                                          !.

% Triptongo +cons final

posibilidadesEspa�ol(Cons,[Cons,Vocal,Vocal2,Vocal3,Cons_f],[Cons,Vocal,Vocal2,Vocal3,Cons_f],Resto):-
                                          consEspa�ol(Cons),
                                           triptongoEspa�ol(Vocal,[Vocal,Vocal2,Vocal3]),
                                          cons_fEspa�ol(Cons_f),
                                          append([Cons,Vocal,Vocal2,Vocal3,Cons_f],Resto,[Cons,Vocal,Vocal2,Vocal3,Cons_f]),
                                          !.

% Triptongo en cons_trEspa�ol + cons final

posibilidadesEspa�ol(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],Resto):-
                                          consParejaEspa�ol(ConsPareja,[ConsPareja,Cons_tr|_]),
                                           triptongoEspa�ol(Vocal,[Vocal,Vocal2,Vocal3]),
                                          cons_fEspa�ol(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f]),
                                          !.


