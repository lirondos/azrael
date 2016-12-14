% Autor:
% Datum: 29/03/2011

% Autor:
% Datum: 29/03/2011

latin(X,Restos,Origen):-
                           latin(X,Restos),
                           origen_etimologicoLatin_Azraelum(Restos,Origen).

origen_etimologicoLatin_Azraelum([],[]).

origen_etimologicoLatin_Azraelum([Restos|MasRestos],[Origen|MasOrigen]):-
                                                 atom_chars(Restos,Deletreo),
                                                 combinacionesLatin_Azraelum(Restos,Deletreo,Origen),
                                                 !,
                                                 origen_etimologicoLatin_Azraelum(MasRestos,MasOrigen).


combinacionesLatin_Azraelum(Restos,[J],desconocido(Restos)).

% combinacionesLatin_Azraelum(Restos,[k|W],griego(Restos)).
% excluida la K porque solo encuentro 8 helenismos que la contengan (y da falso - frente a eusk)
% combinacionesLatin_Azraelum(Restos,[z|W],griego(Restos)).

combinacionesLatin_Azraelum(Restos,[Cons,y|W],griego(Restos)):-
                                                      consLatin_Azraelum(Cons).

combinacionesLatin_Azraelum(Restos,[r,h,A|W],griego(Restos)).

combinacionesLatin_Azraelum(Restos,[c,h,C|W],griego(Restos)).

combinacionesLatin_Azraelum(Restos,[t,h,C|W],griego(Restos)).

combinacionesLatin_Azraelum(Restos,[g,d,C|W],griego(Restos)).

combinacionesLatin_Azraelum(Restos,[c,n,C|W],griego(Restos)).

combinacionesLatin_Azraelum(Restos,[t,h,C|W],griego(Restos)).

combinacionesLatin_Azraelum(Restos,[o,u|W],griego(Restos)).

combinacionesLatin_Azraelum(Restos,[p,h,C|W],griego(Restos)).

combinacionesLatin_Azraelum(Restos,[c,m,F|W],griego(Restos)).

combinacionesLatin_Azraelum(Restos,[p,n,F|W],griego(Restos)).

combinacionesLatin_Azraelum(Restos,[p,s,E|W],griego(Restos)).

combinacionesLatin_Azraelum(Restos,[p,t,T|W],griego(Restos)).

combinacionesLatin_Azraelum(Restos,[s,m,E|W],griego(Restos)).

combinacionesLatin_Azraelum(Restos,[Cons,y,A|W],griego(Restos)):-
                                                               consLatin_Azraelum(Cons).




combinacionesLatin_Azraelum(Restos,[X,Y|W],Origen):-
                                       combinacionesLatin_Azraelum(Restos,[Y|W],Origen).

latin(X,Restos):-
               transcribir_Azraelum(X,J),
                transcribir_latin_Azraelum(J,W),
               !,
               lista_Azraelum(W,Y),
               comprobar_palabrasLatin_Azraelum(Y,N),
               recuperar_descartesLatin_Azraelum(N,Restos).


recuperar_descartesLatin_Azraelum([],[]).

recuperar_descartesLatin_Azraelum([N|M],[Restos|MasRestos]):-
               atom_codes(Restos,N),
               recuperar_descartesLatin_Azraelum(M,MasRestos).




% Transcribir convierte las mayusculas en minusculas

transcribir_latin_Azraelum([],[]).

transcribir_latin_Azraelum([97,101|B],[230|Resto]):-
                                transcribir_latin_Azraelum(B,Resto),
                                !.

transcribir_latin_Azraelum([111,101|B],[339|Resto]):-
                                transcribir_latin_Azraelum(B,Resto),
                                !.

transcribir_latin_Azraelum([A|B],[A|Resto]):-
                                transcribir_latin_Azraelum(B,Resto),
                                !.

transcribir_Azraelum([],[]).


transcribir_Azraelum([W|Y],[W|Z]):-
                          W<65,
                          transcribir_Azraelum(Y,Z).

transcribir_Azraelum([W|Y],[A|Z]):-
                          W=<90,
                          W>=65,
                          A is W + 32,
                          transcribir_Azraelum(Y,Z).

transcribir_Azraelum([W|Y],[A|Z]):-
                          W=<220,
                          W>=192,
                          A is W + 32,
                          transcribir_Azraelum(Y,Z).

transcribir_Azraelum([W|Y],[W|Z]):-
                          W>90,
                          transcribir_Azraelum(Y,Z).




%corte(Inicial, Resfinal, Resprov, Sublista)

lista_Azraelum(X, Y):- corte_Azraelum(X, Y, _, []), !.


%%%%METODO AUXILIAR%%%%

%Reglas base
corte_Azraelum([], Resprov, Resfinal, []):-append(Resprov, [], Resfinal).
corte_Azraelum([], Resfinal, Resprov, Sublista):-Sublista\=[], append(Resprov, [Sublista], Resfinal).

%listas vacias cuando aparece dos veces consecutivas 1000 cambiar las dos reglas anteriores por corte([], Resfinal, Resprov, Sublista):-append(Resprov, [Sublista], Resfinal).


% Esta regla sirve para evitar meter listas vacias si te encuentras dos veces seguidas 1000

corte_Azraelum([SignPunt|Resto], Resfinal, Resprov, Sublista):-
                                                    signPunt_Azraelum(SignPunt),
                                                    Sublista=[],
                                                    corte_Azraelum(Resto, Resfinal, Resprov, []).

% Reglas recursivas
corte_Azraelum([SignPunt|Resto], Resfinal, Resprov, Sublista):-
                                                       signPunt_Azraelum(SignPunt),
                                                       append(Resprov, [Sublista], Res),
                                                       corte_Azraelum(Resto, Resfinal, Res, []).

corte_Azraelum([Cab|Resto], Resfinal, Resprov, Sublista):-
                                                   not(signPunt_Azraelum(Cab)),
                                                   append(Sublista, [Cab], Prov),
                                                   corte_Azraelum(Resto, Resfinal, Resprov, Prov).

% Signos de puntuacion considerados

signPunt_Azraelum(32).
signPunt_Azraelum(33).
signPunt_Azraelum(34).
signPunt_Azraelum(38).
signPunt_Azraelum(40).
signPunt_Azraelum(41).
signPunt_Azraelum(44).
signPunt_Azraelum(45).
signPunt_Azraelum(46).
signPunt_Azraelum(47).
signPunt_Azraelum(58).
signPunt_Azraelum(59).
signPunt_Azraelum(63).
signPunt_Azraelum(132).
signPunt_Azraelum(147).
signPunt_Azraelum(148).
signPunt_Azraelum(161).
signPunt_Azraelum(191).
signPunt_Azraelum(171).
signPunt_Azraelum(187).
signPunt_Azraelum(8220).
signPunt_Azraelum(8221).
signPunt_Azraelum(91).
signPunt_Azraelum(93).
signPunt_Azraelum(92).
signPunt_Azraelum(8212).
signPunt_Azraelum(60).
signPunt_Azraelum(62).

signPunt_Azraelum(X):-
             X>47,
             X<58.
             
comprobar_palabrasLatin_Azraelum([],[]).


comprobar_palabrasLatin_Azraelum([Z|W],Sobras):-
                       silabearLatin_Azraelum(Z,F),
                       comprobar_palabrasLatin_Azraelum(W,Sobras),
                       !.

comprobar_palabrasLatin_Azraelum([Z|W],[Z|Sobras]):-
                       not(silabearLatin_Azraelum(Z,F)),
                       comprobar_palabrasLatin_Azraelum(W,Sobras),
                       !.













% El proceso de silabeo se incia con la transformacion de lista de codigo ANSII
% a letras sueltas para poder silabear, y lo manda ya en forma de letras al
% proceso auxiliar de silabeo


% El proceso de silabeo se incia con la transformacion de lista de codigo ANSII a letras sueltas para poder silabear, y lo manda ya en forma de letras al proceso auxiliar de silabeo


silabearLatin_Azraelum(X,Z):-
               atom_codes(Y,X),
               atom_chars(Y,W),
               olfatear_latin(W),
               silabear_auxLatin_Azraelum(W,Z).

silabeoLatin_Azraelum(X,Z):-
               silabear_auxLatin_Azraelum(X,Z).
               
olfatear_latin([]).

olfatear_latin([l,h|T]):-
                         !,
                         fail.

olfatear_latin([H|T]):-
                                                olfatear_latin(T).







% El proceso auxiliar de silabeo

silabear_auxLatin_Azraelum([],[]).




silabear_auxLatin_Azraelum([Z|T],[Silaba|Silaba_resto]):-
                posibilidadesLatin_Azraelum(Z,Silabeo,[Z|T],Resto),
                 atom_chars(Silaba,Silabeo),
                 silabear_auxLatin_Azraelum(Resto,Silaba_resto).
                 




/*
silabear(X,Z):-
               atom_chars(X,W),
               silabear_aux(W,Z).

% El proceso auxiliar de silabeo

silabear_aux([],[]).

silabear_aux([Z|T],[Silaba|Silaba_resto]):-
                 posibilidadesLatin_Azraelum(Z,Silabeo,[Z|T],Resto),
                 atom_chars(Silaba,Silabeo),
                 silabear_aux(Resto,Silaba_resto).

*/











% Letras en español




vocal_Latin_Azraelum(œ).
vocal_Latin_Azraelum(æ).
vocal_Latin_Azraelum(i).
vocal_Latin_Azraelum(u).
vocal_Latin_Azraelum(a).
vocal_Latin_Azraelum(o).
vocal_Latin_Azraelum(e).

vocalLatin_Azraelum(X):-
               vocal_Latin_Azraelum(X),
               !.

vocalLatin_Azraelum(X):-
               letras_equivalentesLatin_Azraelum(Z,X),
               vocal_Latin_Azraelum(Z),
               !.



cons_fLatin_Azraelum(r).
cons_fLatin_Azraelum(s).
cons_fLatin_Azraelum(l).
cons_fLatin_Azraelum(n).
cons_fLatin_Azraelum(d).
cons_fLatin_Azraelum(x).
cons_fLatin_Azraelum(t).
cons_fLatin_Azraelum(m).
cons_fLatin_Azraelum(b).
cons_fLatin_Azraelum(c).

bilabial_Azraelum(p).
bilabial_Azraelum(b).

liquida_Azraelum(r).
liquida_Azraelum(l).

refinalTotalLatin_Azraelum(s,[m,p,s]).
refinalTotalLatin_Azraelum(s,[r,b,s]).
refinalTotalLatin_Azraelum(s,[r,p,s]).



consFinalTotalLatin_Azraelum(s,[p,s]).
consFinalTotalLatin_Azraelum(s,[n,s]).
consFinalTotalLatin_Azraelum(s,[r,s]).
consFinalTotalLatin_Azraelum(c,[n,c]).
consFinalTotalLatin_Azraelum(t,[n,t]).
consFinalTotalLatin_Azraelum(t,[s,t]).
consFinalTotalLatin_Azraelum(t,[r,t]).
consFinalTotalLatin_Azraelum(p,[m,p,t]).
consFinalTotalLatin_Azraelum(c,[n,c,t]).

consFinalTotalLatin_Azraelum(s,[n,s,Cons]):-
consLatin_Azraelum(Cons).


consFinalTotalLatin_Azraelum(t,[s,t,OclusivaSorda]):-
oclusiva_sorda_Azraelum(OclusivaSorda).


oclusiva_Azraelum(Cons):-
oclusiva_sorda_Azraelum(Cons);
oclusiva_sonora_Azraelum(Cons).

nasal_Azraelum(n).
nasal_Azraelum(m).

oclusiva_sorda_Azraelum(p).
oclusiva_sorda_Azraelum(t).
oclusiva_sorda_Azraelum(q).
oclusiva_sorda_Azraelum(c).

oclusiva_sonora_Azraelum(b).
oclusiva_sonora_Azraelum(d).
oclusiva_sonora_Azraelum(g).


equivalenteLatin_Azraelum(e,e).
% equivalenteLatin_Azraelum(e,è).
% equivalenteLatin_Azraelum(e,é).
% equivalenteLatin_Azraelum(a,á).
% equivalenteLatin_Azraelum(a,à).
equivalenteLatin_Azraelum(a,a).
% equivalenteLatin_Azraelum(i,í).
% equivalenteLatin_Azraelum(i,ì).
equivalenteLatin_Azraelum(i,i).
% equivalenteLatin_Azraelum(o,ó).
% equivalenteLatin_Azraelum(o,ò).
% equivalenteLatin_Azraelum(o,o).
equivalenteLatin_Azraelum(u,u).
% equivalenteLatin_Azraelum(u,ù).
% equivalenteLatin_Azraelum(u,ú).
equivalenteLatin_Azraelum(A,A).


letras_equivalentesLatin_Azraelum(A,B):-
                          equivalenteLatin_Azraelum(A,B).

letras_equivalentesLatin_Azraelum(A,B):-
                          equivalenteLatin_Azraelum(B,A).


letras_equivalentesLatin_Azraelum(X,Y):-
                          equivalenteLatin_Azraelum(X,Z),
                          equivalenteLatin_Azraelum(Z,Y).

letras_equivalentesLatin_Azraelum(X,Y):-
                          equivalenteLatin_Azraelum(X,Z),
                          equivalenteLatin_Azraelum(Y,Z).

letras_equivalentesLatin_Azraelum(X,Y):-
                          equivalenteLatin_Azraelum(Z,X),
                          equivalenteLatin_Azraelum(Z,Y).


















%% Consontantes emparejadas

% b

consParejaLatin_Azraelum(b,[b,Liquida|_]):-
                              liquida_Azraelum(Liquida).

% f

consParejaLatin_Azraelum(f,[f,Liquida|_]):-
                              liquida_Azraelum(Liquida).

% g

consParejaLatin_Azraelum(g,[g,Liquida|_]):-
                              liquida_Azraelum(Liquida).

% c

consParejaLatin_Azraelum(c,[c,Liquida|_]):-
                              liquida_Azraelum(Liquida).

% g

consParejaLatin_Azraelum(p,[p,Liquida|_]):-
                              liquida_Azraelum(Liquida).

% d

consParejaLatin_Azraelum(d,[d,r|_]).

% t

consParejaLatin_Azraelum(t,[t,r|_]).

% qu

consParejaLatin_Azraelum(q,[q,u|_]).

% gu

consParejaLatin_Azraelum(g,[g,u|_]).



% s liquida

consParejaLatin_Azraelum(s,[s,OclusivaSorda|_]):-
oclusiva_sorda_Azraelum(OclusivaSorda).

% s liquida

consParejaLatin_Azraelum(s,[s,s|_]).

% gu

consParejaLatin_Azraelum(g,[g,u|_]).

% ch

% consParejaLatin_Azraelum(c,[c,h|_]).

consTrioLatin_Azraelum(s,[s,Cons1,Cons2]):-
                             oclusiva_sorda_Azraelum(Cons1),
                             consParejaLatin_Azraelum(Cons1,[Cons1,Cons2]).
                             
% consTrioLatin_Azraelum(c,[c,h,r]).



%% Vocales emparejadas: DIPTONGOS

diptongoLatin_Azraelum(Vocal_a,[Vocal_a,Vocal_u|_]):-
                                            letras_equivalentesLatin_Azraelum(a,Vocal_a),
                                            letras_equivalentesLatin_Azraelum(u,Vocal_u).
                                            
diptongoLatin_Azraelum(æ,[æ,Vocal_a|_]):-
                                            letras_equivalentesLatin_Azraelum(a,Vocal_a).
                                            
diptongoLatin_Azraelum(æ,[æ,Vocal_e|_]):-
                                            letras_equivalentesLatin_Azraelum(e,Vocal_e).
                                            
diptongoLatin_Azraelum(æ,[æ,Vocal_i|_]):-
                                            letras_equivalentesLatin_Azraelum(i,Vocal_i).
                                            
diptongoLatin_Azraelum(æ,[æ,Vocal_o|_]):-
                                            letras_equivalentesLatin_Azraelum(o,Vocal_o).
                                            
diptongoLatin_Azraelum(æ,[æ,Vocal_u|_]):-
                                            letras_equivalentesLatin_Azraelum(u,Vocal_u).
                                            
diptongoLatin_Azraelum(Vocal_u,[Vocal_u,Vocal_i|_]):-
                                             letras_equivalentesLatin_Azraelum(i,Vocal_i),
                                             letras_equivalentesLatin_Azraelum(u,Vocal_u).
hiatoLatin_Azrealum(Vocal_a,[Vocal_a,Vocal_i|_]):-
                                            letras_equivalentesLatin_Azraelum(a,Vocal_a),
                                            letras_equivalentesLatin_Azraelum(i,Vocal_i).

hiatoLatin_Azrealum(Vocal_e,[Vocal_e,Vocal_i|_]):-
                                            letras_equivalentesLatin_Azraelum(e,Vocal_e),
                                             letras_equivalentesLatin_Azraelum(i,Vocal_i).

hiatoLatin_Azrealum(Vocal_e,[Vocal_e,Vocal_u|_]):-
                                            letras_equivalentesLatin_Azraelum(e,Vocal_e),
                                            letras_equivalentesLatin_Azraelum(u,Vocal_u).

hiatoLatin_Azrealum(Vocal_e,[Vocal_e,Vocal_u|_]):-
                                            letras_equivalentesLatin_Azraelum(e,Vocal_e),
                                            letras_equivalentesLatin_Azraelum(u,Vocal_u).

hiatoLatin_Azrealum(Vocal_i,[Vocal_i,Vocal_a|_]):-
                                            letras_equivalentesLatin_Azraelum(a,Vocal_a),
                                             letras_equivalentesLatin_Azraelum(i,Vocal_i).
                                             
hiatoLatin_Azrealum(Vocal_i,[Vocal_i,æ|_]):-
                                            letras_equivalentesLatin_Azraelum(i,Vocal_i).
                                            
hiatoLatin_Azrealum(Vocal_e,[Vocal_e,æ|_]):-
                                            letras_equivalentesLatin_Azraelum(e,Vocal_e).


hiatoLatin_Azrealum(Vocal_i,[Vocal_i,Vocal_e|_]):-
                                            letras_equivalentesLatin_Azraelum(e,Vocal_e),
                                             letras_equivalentesLatin_Azraelum(i,Vocal_i).

hiatoLatin_Azrealum(Vocal_i1,[Vocal_i1,Vocal_i2|_]):-
                                             letras_equivalentesLatin_Azraelum(i,Vocal_i1),
                                              letras_equivalentesLatin_Azraelum(i,Vocal_i2).
hiatoLatin_Azrealum(Vocal_i,[Vocal_i,Vocal_o|_]):-
                                             letras_equivalentesLatin_Azraelum(i,Vocal_i),
                                             letras_equivalentesLatin_Azraelum(o,Vocal_o).
hiatoLatin_Azrealum(Vocal_i,[Vocal_i,Vocal_u|_]):-
                                             letras_equivalentesLatin_Azraelum(i,Vocal_i),
                                             letras_equivalentesLatin_Azraelum(u,Vocal_u).
                                             
hiatoLatin_Azrealum(Vocal_o,[Vocal_o,Vocal_i|_]):-
                                         letras_equivalentesLatin_Azraelum(i,Vocal_i),
                                         letras_equivalentesLatin_Azraelum(o,Vocal_o).
                                         
hiatoLatin_Azrealum(Vocal_u,[Vocal_u,Vocal_a|_]):-
                                            letras_equivalentesLatin_Azraelum(a,Vocal_a),
                                            letras_equivalentesLatin_Azraelum(u,Vocal_u).
                                            
hiatoLatin_Azrealum(Vocal_u,[Vocal_u,æ|_]):-
                                            letras_equivalentesLatin_Azraelum(u,Vocal_u).

hiatoLatin_Azrealum(Vocal_u,[Vocal_u,Vocal_e|_]):-
                                            letras_equivalentesLatin_Azraelum(e,Vocal_e),
                                            letras_equivalentesLatin_Azraelum(u,Vocal_u).

hiatoLatin_Azrealum(Vocal_u,[Vocal_u,Vocal_o|_]):-
                                            letras_equivalentesLatin_Azraelum(o,Vocal_o),
                                            letras_equivalentesLatin_Azraelum(u,Vocal_u).
hiatoLatin_Azrealum(Vocal_u,[Vocal_u,Vocal_u2|_]):-
                                            letras_equivalentesLatin_Azraelum(u,Vocal_u),
                                            letras_equivalentesLatin_Azraelum(u,Vocal_u2).


hiatoLatin_Azrealum(Vocal_e,[Vocal_e,Vocal_a|_]):-
                                         letras_equivalentesLatin_Azraelum(a,Vocal_a),
                                         letras_equivalentesLatin_Azraelum(e,Vocal_e).


hiatoLatin_Azrealum(Vocal_e,[Vocal_e,Vocal_e2|_]):-
                                          letras_equivalentesLatin_Azraelum(e,Vocal_e),
                                          letras_equivalentesLatin_Azraelum(e,Vocal_e2).
hiatoLatin_Azrealum(Vocal_e,[Vocal_e,Vocal_o|_]):-
                                         letras_equivalentesLatin_Azraelum(e,Vocal_e),
                                         letras_equivalentesLatin_Azraelum(o,Vocal_o).


hiatoLatin_Azrealum(Vocal_o,[Vocal_o,Vocal_a|_]):-
                                         letras_equivalentesLatin_Azraelum(a,Vocal_a),
                                         letras_equivalentesLatin_Azraelum(o,Vocal_o).
                                         


triptongoLatin_Azraelum(Vocal_i,[Vocal_i,Vocal_e,Vocal_i2]):-
                                                    letras_equivalentesLatin_Azraelum(i,Vocal_i),
                                                    letras_equivalentesLatin_Azraelum(e,Vocal_e),
                                                    letras_equivalentesLatin_Azraelum(i,Vocal_i2).




%% Consontantes pseudofinales: contextos

% Letra m

pseudocons_fLatin_Azraelum(m,[m,Bilabial|_]):-
                                  bilabial_Azraelum(Bilabial).


pseudocons_fLatin_Azraelum(m,[m,n|_]).



% Letra g


pseudocons_fLatin_Azraelum(g,[g,n|_]).

pseudocons_fLatin_Azraelum(g,[g,g|_]).

pseudocons_fLatin_Azraelum(g,[g,m|_]).


% Letra p

dental_Azraelum(t).
dental(c).

pseudocons_fLatin_Azraelum(p,[p,Dentales|_]):-
                                  dental_Azraelum(Dentales);
                                   Dentales=s;
                                   Dentales=p.




% Letra x

pseudocons_fLatin_Azraelum(x,[x,t|_]).


% Letra c

pseudocons_fLatin_Azraelum(c,[c,c|_]).



pseudocons_fLatin_Azraelum(c,[c,t|_]).




consLatin_Azraelum(b).
consLatin_Azraelum(c).
consLatin_Azraelum(d).
consLatin_Azraelum(f).
consLatin_Azraelum(g).
consLatin_Azraelum(h).
consLatin_Azraelum(l).
consLatin_Azraelum(m).
consLatin_Azraelum(n).
consLatin_Azraelum(p).
consLatin_Azraelum(r).
consLatin_Azraelum(s).
consLatin_Azraelum(t).
consLatin_Azraelum(v).
consLatin_Azraelum(x).
consLatin_Azraelum(i).



% Silabas en español

% posibilidades/4: letra que encabeza la silaba/silaba/letras que siguen/resto de la palabra (sin la silaba que acabamos de aislar, pero si con el contexto.

%% PREFIJOS



posibilidadesLatin_Azraelum(s,[s,u,b,s],[s,u,b,s,Cons|W],Resto):-
                                                                consLatin_Azraelum(Cons),
                                                                Cons\=i,
                                                                append([s,u,b,s],Resto,[s,u,b,s,Cons|W]),
                                                                !.




posibilidadesLatin_Azraelum(s,[s,u,b],[s,u,b,Cons|W],Resto):-
                                                                consLatin_Azraelum(Cons),
                                                                Cons\=i,
                                                                append([s,u,b],Resto,[s,u,b,Cons|W]),
                                                              !.

posibilidadesLatin_Azraelum(s,[s,u,b],[s,u,b],Resto):-
                                                                append([s,u,b],Resto,[s,u,b]),
                                                                 !.




posibilidadesLatin_Azraelum(o,[o,b,s],[o,b,s,Cons|W],Resto):-
                                                                consLatin_Azraelum(Cons),
                                                                Cons\=i,
                                                                append([o,b,s],Resto,[o,b,s,Cons|W]),
                                                                 !.



posibilidadesLatin_Azraelum(o,[o,b],[o,b],Resto):-
                                                                append([o,b],Resto,[o,b]),
                                                                 !.


posibilidadesLatin_Azraelum(o,[o,b],[o,b,Cons|W],Resto):-
                                                                consLatin_Azraelum(Cons),
                                                                Cons\=i,
                                                                not(consParejaLatin_Azraelum(b,[b,Cons])),
                                                                append([o,b],Resto,[o,b,Cons|W]),
                                                                 !.



posibilidadesLatin_Azraelum(a,[a,b,s],[a,b,s,Cons|W],Resto):-
                                                                consLatin_Azraelum(Cons),
                                                                append([a,b,s],Resto,[a,b,s,Cons|W]),
                                                                !.




posibilidadesLatin_Azraelum(a,[a,b],[a,b],Resto):-
                                                                append([a,b],Resto,[a,b]),
                                                                 !.









posibilidadesLatin_Azraelum(a,[a,b],[a,b,Cons|W],Resto):-
                                                               consLatin_Azraelum(Cons),
                                                               Cons\=i,
                                                                not(consParejaLatin_Azraelum(b,[b,Cons])),
                                                                 append([a,b],Resto,[a,b,Cons|W]),
                                                                !.


posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,c],[Cons,Vocal,c],Resto):-
                                                                append([Cons,Vocal,c],Resto,[Cons,Vocal,c]),
                                                                vocalLatin_Azraelum(Vocal),
                                                                consLatin_Azraelum(Cons),
                                                                 !.

posibilidadesLatin_Azraelum(a,[a,c],[a,c],Resto):-
                                                                append([a,c],Resto,[a,c]),
                                                                 !.



%ex

posibilidadesLatin_Azraelum(e,[e,x],[e,x,Cons|W],Resto):-
                                                                consLatin_Azraelum(Cons),
                                                                Cons\=i,
                                                                append([e,x],Resto,[e,x,Cons|W]),
                                                                 !.

%off

posibilidadesLatin_Azraelum(o,[o,f],[o,f,f|W],Resto):-
                                                                 append([o,f],Resto,[o,f,f|W]),
                                                                 !.



%diff

posibilidadesLatin_Azraelum(d,[d,i,f],[d,i,f,f|W],Resto):-
                                                                 append([d,i,f],Resto,[d,i,f,f|W]),
                                                                 !.

%eff

posibilidadesLatin_Azraelum(e,[e,f],[e,f,f|W],Resto):-
                                                                 append([e,f],Resto,[e,f,f|W]),
                                                                 !.

%suff

posibilidadesLatin_Azraelum(s,[s,u,f],[s,u,f,f|W],Resto):-
                                                                 append([s,u,f],Resto,[s,u,f,f|W]),
                                                                 !.

/*
posibilidadesLatin_Azraelum(a,[a,b,s],[a,b,s,Cons|W],Resto):-
                                                                 consLatin_Azraelum(Cons),
                                                                 % not(consParejaLatin_Azraelum(s,[s,Cons])),
                                                                 append([a,b,s],Resto,[a,b,s,Cons|W]),
                                                                 !.

posibilidadesLatin_Azraelum(o,[o,b,s],[o,b,s,Cons|W],Resto):-
                                                                 consLatin_Azraelum(Cons),
                                                                 % not(consParejaLatin_Azraelum(s,[s,Cons])),
                                                                 append([o,b,s],Resto,[o,b,s,Cons|W]),
                                                                 !.


posibilidadesLatin_Azraelum(i,[i,n,s],[i,n,s,Cons|W],Resto):-
                                                                 consLatin_Azraelum(Cons),
                                                                 % not(consParejaLatin_Azraelum(s,[s,Cons])),
                                                                 append([i,n,s],Resto,[i,n,s,Cons|W]),
                                                                 !.

posibilidadesLatin_Azraelum(t,[t,r,a,n,s],[t,r,a,n,s,Cons|W],Resto):-
                                                                 consLatin_Azraelum(Cons),
                                                                 % not(consParejaLatin_Azraelum(s,[s,Cons])),
                                                                 append([t,r,a,n,s],Resto,[t,r,a,n,s,Cons|W]),
                                                                 !.

*/

%tirps

posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal,Refinal],[Cons,Vocal,Cons_f,ConsFinalTotal,Refinal],Resto):-
                                                                  consLatin_Azraelum(Cons),
                                                                 vocalLatin_Azraelum(Vocal),
                                                               refinalTotalLatin_Azraelum(Refinal,[Cons_f,ConsFinalTotal,Refinal]),
                                                                append([Cons,Vocal,Cons_f,ConsFinalTotal,Refinal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,Refinal]),
                                                                !.


% post-quam



posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal],[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,Cons_tr|W],Resto):-
                                          consLatin_Azraelum(Cons),
                                          vocalLatin_Azraelum(Vocal),
                                          consParejaLatin_Azraelum(ConsSiguiente,[ConsSiguiente,Cons_tr]),
                                          not(consTrioLatin_Azraelum(ConsFinalTotal,[ConsFinalTotal,ConsSiguiente,Cons_tr])),
                                          consFinalTotalLatin_Azraelum(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                          append([Cons,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,Cons_tr|W]),
                                          !.

% Au-un

posibilidadesLatin_Azraelum(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,Vocal3|W],Resto):-
                                                                diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2]),
                                                                hiatoLatin_Azrealum(Vocal2,[Vocal2,Vocal3]),
                                                                append([Vocal,Vocal2],Resto,[Vocal,Vocal2,Vocal3|W]),
                                                                !.


%nunc

posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal],[Cons,Vocal,Cons_f,ConsFinalTotal],Resto):-
                                                                  consLatin_Azraelum(Cons),
                                                                 vocalLatin_Azraelum(Vocal),
                                                                consFinalTotalLatin_Azraelum(ConsFinalTotal,[Cons_f,ConsFinalTotal]),
                                                                append([Cons,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal]),
                                                                !.
                                                                
%tirps

posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal,Refinal],[Cons,Vocal,Cons_f,ConsFinalTotal,Refinal],Resto):-
                                                                  consLatin_Azraelum(Cons),
                                                                 vocalLatin_Azraelum(Vocal),
                                                              refinalTotalLatin_Azraelum(Refinal,[Cons_f,ConsFinalTotal,Refinal]),
                                                                append([Cons,Vocal,Cons_f,ConsFinalTotal,Refinal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,Refinal]),
                                                                !.




% brai-le

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2,ConsBis,VocalBis|W],Resto):-
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                          consLatin_Azraelum(ConsBis),
                                          vocalLatin_Azraelum(VocalBis),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,ConsBis,VocalBis|W]),
                                          !.
                                          
% post-quam



posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal],[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,Cons_tr|W],Resto):-
                                          consLatin_Azraelum(Cons),
                                          vocalLatin_Azraelum(Vocal),
                                          consParejaLatin_Azraelum(ConsSiguiente,[ConsSiguiente,Cons_tr]),
                                          not(consTrioLatin_Azraelum(ConsFinalTotal,[ConsFinalTotal,ConsSiguiente,Cons_tr])),
                                          consFinalTotalLatin_Azraelum(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                          append([Cons,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,Cons_tr|W]),
                                          !.

% antro, an-cla


% antro, an-cla

posibilidadesLatin_Azraelum(Vocal,[Vocal,Cons_f],[Vocal,Cons_f,ConsPareja,Cons_tr,Vocal2|W],Resto):-
                                                                 vocalLatin_Azraelum(Vocal),
                                                                (cons_fLatin_Azraelum(Cons_f);pseudocons_fLatin_Azraelum(Cons_f,[Cons_f,ConsPareja|_])),
                                                                (not(consParejaLatin_Azraelum(Cons_f,[Cons_f,ConsPareja]));not(consTrioLatin_Azraelum(Cons_f,[Cons_f,ConsPareja,Cons_tr]))),
                                                                ((consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr]),vocalLatin_Azraelum(Vocal2));consTrioLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr,Vocal2])),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f,ConsPareja,Cons_tr,Vocal2|W]),
                                                                !.




% posibilidades para CONS+VOCAL AISLADO

posibilidadesLatin_Azraelum(Cons,[Cons,Vocal],[Cons,Vocal],Resto):-
                                          consLatin_Azraelum(Cons),
                                          vocalLatin_Azraelum(Vocal),
                                          append([Cons,Vocal],Resto,[Cons,Vocal]),
                                          !.

% posibilidaddes para CONS+VOCAL EN PALABRA


posibilidadesLatin_Azraelum(Cons,[Cons,Vocal],[Cons,Vocal,Cons1,Vocal1|W],Resto):-
                                          consLatin_Azraelum(Cons),
                                          vocalLatin_Azraelum(Vocal),
                                          consLatin_Azraelum(Cons1),
                                          vocalLatin_Azraelum(Vocal1),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Cons1,Vocal1|W]),
                                          !.

% CONS+VOCAL+CONS_F: pen-sar. OJO: para el latin es importante que esta regla
% aparezca aqui. Res-su-co; al ir la s tb como vocal, si cambiamos el orden, lo
% interpreta como re-s-su-co.


posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f,Cons1,Vocal1|W],Resto):-
                                          consLatin_Azraelum(Cons),
                                          vocalLatin_Azraelum(Vocal),
                                          (cons_fLatin_Azraelum(Cons_f);pseudocons_fLatin_Azraelum(Cons_f,[Cons_f,Cons1|_])),
                                          consLatin_Azraelum(Cons1),
                                          vocalLatin_Azraelum(Vocal1),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f,Cons1,Vocal1|W]),
                                          !.


% posibilidades para CONS+VOCAL+VOCAL AISLADO:

posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2],Resto):-
                                          consLatin_Azraelum(Cons),
                                          diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2]),
                                          !.



% posibilidaddes para CONS+VOCAL+VOCAL EN PALABRA:

posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2,Cons1,Vocal1|W],Resto):-
                                          consLatin_Azraelum(Cons),
                                          diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                          consLatin_Azraelum(Cons1),
                                          vocalLatin_Azraelum(Vocal1),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2,Cons1,Vocal1|W]),
                                          !.



% posibilidaddes para CONS+VOCAL EN PALABRA: HIATO

posibilidadesLatin_Azraelum(Cons,[Cons,Vocal],[Cons,Vocal,Vocal2|W],Resto):-
                                          consLatin_Azraelum(Cons),
                                          hiatoLatin_Azrealum(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Vocal2|W]),
                                          !.



% nai-blar; nai-trar

posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2,ConsPareja,Cons_tr|W],Resto):-
                                          consLatin_Azraelum(Cons),
                                          diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2,ConsPareja,Cons_tr|W]),
                                          !.

% CONS+VOCAL+VOCAL+CONS_FIN con dipt descendente AISLADO: a-ma-bais

posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f],Resto):-
                                          consLatin_Azraelum(Cons),
                                          diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                          cons_fLatin_Azraelum(Cons_f),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f]),
                                          !.


% CONS+VOCAL+VOCAL+CONS_F: DIPT DESCENDENTE mais-car



posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f,Cons1,Vocal1|W],Resto):-
                                          consLatin_Azraelum(Cons),
                                          diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                          (cons_fLatin_Azraelum(Cons_f);pseudocons_fLatin_Azraelum(Cons_f,[Cons_f,Cons1|_])),
                                          consLatin_Azraelum(Cons1),
                                          vocalLatin_Azraelum(Vocal1),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f,Cons1,Vocal1|W]),
                                          !.

% CONS+VOCAL+VOCAL+CONS_F: DIPT DESCENDENTE mais-bla



posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f,ConsTipo,Cons_tr|W],Resto):-
                                          consLatin_Azraelum(Cons),
                                          diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                          (cons_fLatin_Azraelum(Cons_f);pseudocons_fLatin_Azraelum(Cons_f,[Cons_f,ConsTipo|_])),
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsPareja,Cons_tr|W]),
                                          !.


% CONS+VOCAL+VOCAL+CONS_F*CONSFINALTOTAL: DIPT DESCENDENTE mains-bla



posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                          consLatin_Azraelum(Cons),
                                          diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                          consFinalTotalLatin_Azraelum(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                          append([Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal,ConsSiguiente|_]),
                                          !.
                                          

%panc-to

posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal],[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                                                  consLatin_Azraelum(Cons),
                                                                 vocalLatin_Azraelum(Vocal),
                                                                 consFinalTotalLatin_Azraelum(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                                                append([Cons,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W]),
                                                                !.



%neins

posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],Resto):-
                                          consLatin_Azraelum(Cons),
                                          diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                          consFinalTotalLatin_Azraelum(ConsFinalTotal,[Cons_f,ConsFinalTotal]),
                                          append([Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal]),
                                          !.
                                          
%hiemps

posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal,Refinal],[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal,Refinal],Resto):-
                                                                consLatin_Azraelum(Cons),
                                                                diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                                                refinalTotalLatin_Azraelum(Refinal,[Cons_f,ConsFinalTotal,Refinal]),
                                                                append([Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal,Refinal],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal,Refinal]),
                                                                !.
                                                                
% stirps

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal,Refinal],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal,Refinal],Resto):-
                                                                 vocalLatin_Azraelum(Vocal),
                                                                 consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr]),
                                                                refinalTotalLatin_Azraelum(Refinal,[Cons_f,ConsFinalTotal,Refinal]),
                                                               append([ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal,Refinal],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal,Refinal]),
                                                                !.


% CONS+VOCAL+CONS_F: pen-trar


posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f,ConsPareja,Cons_tr|W],Resto):-
                                          consLatin_Azraelum(Cons),
                                          vocalLatin_Azraelum(Vocal),
                                          (cons_fLatin_Azraelum(Cons_f);pseudocons_fLatin_Azraelum(Cons_f,[Cons_f,ConsPareja|W])),
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr]),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f,ConsPareja,Cons_tr|W]),
                                          !.





% nu-blar  pa-dre

posibilidadesLatin_Azraelum(Cons,[Cons,Vocal],[Cons,Vocal,ConsPareja,Cons_tr|W],Resto):-
                                          consLatin_Azraelum(Cons),
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr]),
                                          vocalLatin_Azraelum(Vocal),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,ConsPareja,Cons_tr|W]),
                                          !.


% CONS+VOCAL+CONS_FIN EN FINAL DE PALABRA: del-fin


posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f],Resto):-
                                          consLatin_Azraelum(Cons),
                                          vocalLatin_Azraelum(Vocal),
                                          cons_fLatin_Azraelum(Cons_f),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f]),
                                          !.






%%% silabas que empiezan por vocal

%% OJO PARA TODO EL CÓDIGO QUE SIGUE: OI, AU y AI son los unicos dipt que forman dipt tal cual. Los desmas son extranj: comprobar

% a


posibilidadesLatin_Azraelum(Vocal,[Vocal],[Vocal],Resto):-
                                                                vocalLatin_Azraelum(Vocal),
                                                                append([Vocal],Resto,[Vocal]),
                                                                !.
% a-la

posibilidadesLatin_Azraelum(Vocal,[Vocal],[Vocal,Cons,Vocal1|W],Resto):-
                                                                vocalLatin_Azraelum(Vocal),
                                                                consLatin_Azraelum(Cons),
                                                                vocalLatin_Azraelum(Vocal1),
                                                                append([Vocal],Resto,[Vocal,Cons,Vocal1|W]),
                                                                !.
%anc-to

posibilidadesLatin_Azraelum(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                                                 vocalLatin_Azraelum(Vocal),
                                                                 consFinalTotalLatin_Azraelum(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                                                append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W]),
                                                                !.








% a-pro-xi-mar-se; a-dri

posibilidadesLatin_Azraelum(Vocal,[Vocal],[Vocal,ConsPareja,Cons_tr,Vocal2|W],Resto):-
                                                                 vocalLatin_Azraelum(Vocal),
                                                                 consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                                               append([Vocal],Resto,[Vocal,ConsPareja,Cons_tr,Vocal2|W]),
                                                                !.

% ai-rar, au-lar, eusebio

posibilidadesLatin_Azraelum(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,Cons,Vocal1|W],Resto):-
                                                                        diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                                                        consLatin_Azraelum(Cons),
                                                                        vocalLatin_Azraelum(Vocal1),
                                                                        append([Vocal,Vocal2],Resto,[Vocal,Vocal2,Cons,Vocal1|W]),
                                                                         !.


% ai-trar

posibilidadesLatin_Azraelum(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,ConsPareja,Cons_tr|W],Resto):-
                                                                        diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                                                        consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr]),
                                                                        append([Vocal,Vocal2],Resto,[Vocal,Vocal2,ConsPareja,Cons_tr|W]),
                                                                         !.




% a-ma-ri-AIS

posibilidadesLatin_Azraelum(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f],Resto):-
                                                                diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                                                cons_fLatin_Azraelum(Cons_f),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f]),
                                                                !.


% AIS-lar

posibilidadesLatin_Azraelum(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f,Cons|W],Resto):-
                                                                diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                                                (cons_fLatin_Azraelum(Cons_f);pseudocons_fLatin_Azraelum(Cons_f,[Cons_f,Cons|_])),
                                                                consLatin_Azraelum(Cons),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f,Cons|W]),
                                                                !.
                                                                
% AIS-que

posibilidadesLatin_Azraelum(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f,Cons,Constr|W],Resto):-
                                                                diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                                                (cons_fLatin_Azraelum(Cons_f);pseudocons_fLatin_Azraelum(Cons_f,[Cons_f,Cons|_])),
                                                                consParejaLatin_Azraelum(Cons,[Cons,Constr]),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f,Cons,Constr|W]),
                                                                !.
                                                                

% Au-un

posibilidadesLatin_Azraelum(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,Vocal3|W],Resto):-
                                                                diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2]),
                                                                hiatoLatin_Azrealum(Vocal2,[Vocal2,Vocal3]),
                                                                append([Vocal,Vocal2],Resto,[Vocal,Vocal2,Vocal3|W]),
                                                                !.






% a-e-re-o

posibilidadesLatin_Azraelum(Vocal,[Vocal],[Vocal,Vocal2|W],Resto):-
                                                                 hiatoLatin_Azrealum(Vocal,[Vocal,Vocal2|_]),
                                                                 append([Vocal],Resto,[Vocal,Vocal2|W]),
                                                                 !.

% en

posibilidadesLatin_Azraelum(Vocal,[Vocal,Cons_f],[Vocal,Cons_f],Resto):-
                                                                vocalLatin_Azraelum(Vocal),
                                                                cons_fLatin_Azraelum(Cons_f),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f]),
                                                                !.


% is-la

posibilidadesLatin_Azraelum(Vocal,[Vocal,Cons_f],[Vocal,Cons_f,ConsBis,VocalBis|W],Resto):-
                                                                 vocalLatin_Azraelum(Vocal),
                                                                (cons_fLatin_Azraelum(Cons_f);pseudocons_fLatin_Azraelum(Cons_f,[Cons_f,ConsBis|W])),
                                                                consLatin_Azraelum(ConsBis),
                                                                vocalLatin_Azraelum(VocalBis),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f,ConsBis,VocalBis|W]),
                                                                !.




%ens-ta

posibilidadesLatin_Azraelum(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                                                 vocalLatin_Azraelum(Vocal),
                                                                  consFinalTotalLatin_Azraelum(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                                                append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W]),
                                                                !.

%ens

posibilidadesLatin_Azraelum(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal],Resto):-
                                                                 vocalLatin_Azraelum(Vocal),
                                                                consFinalTotalLatin_Azraelum(ConsFinalTotal,[Cons_f,ConsFinalTotal]),
                                                                append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal]),
                                                                !.
                                                                
%urbs

posibilidadesLatin_Azraelum(Vocal,[Vocal,Cons_f,ConsFinalTotal,Refinal],[Vocal,Cons_f,ConsFinalTotal,Refinal],Resto):-
                                                                vocalLatin_Azraelum(Vocal),
                                                               refinalTotalLatin_Azraelum(Refinal,[Cons_f,ConsFinalTotal,Refinal]),
                                                                append([Vocal,Cons_f,ConsFinalTotal,Refinal],Resto,[Vocal,Cons_f,ConsFinalTotal,Refinal]),
                                                                !.


/*


%anc-to

posibilidadesLatin_Azraelum(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                                                 vocalLatin_Azraelum(Vocal),
                                                                cons_fLatin_Azraelum(Cons_f),
                                                                 consFinalTotalLatin_Azraelum(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                                                append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W]),
                                                                !.

*/






% ay, uy: unicas palabras encontradas con dipt que solo son eso: el dipt, pero meto todos los casos pq nada lo prohibe. No meto el dipt ascendente ni el doble pq no parece probable

posibilidadesLatin_Azraelum(Vocal,[Vocal,Vocal2],[Vocal,Vocal2],Resto):-
                                                                diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                                                append([Vocal,Vocal2],Resto,[Vocal,Vocal2]),
                                                                !.


% ea: hiato aislado. Solo encontrado "ea", pero meto todo porque nada lo impide

posibilidadesLatin_Azraelum(Vocal,[Vocal],[Vocal,Vocal2],Resto):-
                                                                hiatoLatin_Azrealum(Vocal,[Vocal,Vocal2|_]),
                                                                append([Vocal],Resto,[Vocal,Vocal2]),
                                                                !.


% posibilidades para CONS+VOCAL+VOCAL AISLADO: HIATO

posibilidadesLatin_Azraelum(Cons,[Cons,Vocal],[Cons,Vocal,Vocal2],Resto):-
                                          consLatin_Azraelum(Cons),
                                          hiatoLatin_Azrealum(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Vocal2]),
                                          !.

% tue-or

posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2,Vocal3|W],Resto):-
                                          consLatin_Azraelum(Cons),
                                          diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                          hiatoLatin_Azrealum(Vocal2,[Vocal2,Vocal3|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2,Vocal3|W]),
                                          !.

%% Caso particular: trabadas en mitad de palabra






% bla

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal],Resto):-
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalLatin_Azraelum(Vocal),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal]),
                                          !.



% spra

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal],[ConsPareja,ConsPareja2,Cons_tr,Vocal],Resto):-
                                                                 vocalLatin_Azraelum(Vocal),
                                                                 consTrioLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                                               append([ConsPareja,ConsPareja2,Cons_tr,Vocal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal]),
                                                                !.



% gra-pa

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,Cons,Vocal1|W],Resto):-
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalLatin_Azraelum(Vocal),
                                          vocalLatin_Azraelum(Vocal1),
                                          consLatin_Azraelum(Cons),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,Cons,Vocal1|W]),
                                          !.

% stru-to

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal],[ConsPareja,ConsPareja2,Cons_tr,Vocal,Cons,Vocal1|W],Resto):-
                                                                 vocalLatin_Azraelum(Vocal),
                                                                 consLatin_Azraelum(Cons),
                                                                 vocalLatin_Azraelum(Vocal1),
                                                                 consTrioLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                                               append([ConsPareja,ConsPareja2,Cons_tr,Vocal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Cons,Vocal1|W]),
                                                                !.

% blau?

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2],Resto):-
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2]),
                                          !.
%strae

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2],[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2],Resto):-
                                          consTrioLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                          diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                          append([ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2]),
                                          !.


%strae-le

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2],[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsBis,VocalBis],Resto):-
                                          consTrioLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                          diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                          consLatin_Azraelum(ConsBis),
                                          vocalLatin_Azraelum(VocalBis),
                                          append([ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsBis,VocalBis]),
                                          !.


% blau-bla

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2,ConsParejaBis,Cons_trBis|W],Resto):-
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                          consParejaLatin_Azraelum(ConsParejaBis,[ConsParejaBis,Cons_trBis|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,ConsParejaBis,Cons_trBis|W]),
                                          !.

%strae-bla

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2],[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsParejaBis,Cons_trBis],Resto):-
                                          consTrioLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                          diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                          consParejaLatin_Azraelum(ConsParejaBis,[ConsPareja2,Cons_trBis|_]),
                                          append([ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsParejaBis,Cons_trBis]),
                                          !.

% blaun

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto):-
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                          cons_fLatin_Azraelum(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f]),
                                       !.

%straen

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsFinal],[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsFinal],Resto):-
                                          consTrioLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                          diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                          cons_fLatin_Azraelum(ConsFinal),
                                          append([ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsFinal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsFinal]),
                                          !.

% brains

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal],Resto):-
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                          consLatin_Azraelum(Cons),
                                          consFinalTotalLatin_Azraelum(ConsFinalTotal,[Cons_f,ConsFinalTotal]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal]),
                                          !.

% brain-le

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,Cons|W],Resto):-
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                          consLatin_Azraelum(Cons),
                                          (cons_fLatin_Azraelum(Cons_f);pseudocons_fLatin_Azraelum(Cons_f,[Cons_f,Cons|_])),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,Cons|W]),
                                          !.

%straen-le

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsFinal],[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsFinal,Cons,Vocal],Resto):-
                                          consTrioLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                          diptongoLatin_Azraelum(Vocal,[Vocal,Vocal2|_]),
                                          cons_fLatin_Azraelum(ConsFinal),
                                          consLatin_Azraelum(Cons),
                                          vocalLatin_Azraelum(Vocal),
                                          append([ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsFinal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsFinal,Cons,Vocal]),
                                          !.





% pro-e-mio

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,Vocal2|W],Resto):-
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          hiatoLatin_Azrealum(Vocal,[Vocal,Vocal2|_]),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2|W]),
                                          !.

% stra-o-mio

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal],[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2|W],Resto):-
                                                                 hiatoLatin_Azrealum(Vocal,[Vocal,Vocal2|_]),
                                                                 consTrioLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                                               append([ConsPareja,ConsPareja2,Cons_tr,Vocal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2|W]),
                                                                !.





% bla-bla

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,ConsPareja1,Cons_tr1|W],Resto):-
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalLatin_Azraelum(Vocal),
                                          consParejaLatin_Azraelum(ConsPareja1,[ConsPareja1,Cons_tr1|_]),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,ConsPareja1,Cons_tr1|W]),

%stra-bla
                                          !.

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal],[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsParejaBis,Cons_trBis|W],Resto):-
                                                                 vocalLatin_Azraelum(Vocal),
                                                                 consTrioLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                                                 consParejaLatin_Azraelum(ConsParejaBis,[ConsPareja1,Cons_trBis|_]),
                                                               append([ConsPareja,ConsPareja2,Cons_tr,Vocal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsParejaBis,Cons_trBis|W]),
                                                                !.

% blas

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f],Resto):-
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalLatin_Azraelum(Vocal),
                                          cons_fLatin_Azraelum(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f]),
                                          !.

%blans

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],Resto):-
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalLatin_Azraelum(Vocal),
                                          consFinalTotalLatin_Azraelum(ConsFinalTotal,[Cons_f,ConsFinalTotal]),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal]),
                                          !.

%stras

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal],[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal],Resto):-
                                                                 vocalLatin_Azraelum(Vocal),
                                                                 consTrioLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                                                 cons_fLatin_Azraelum(ConsFinal),
                                                               append([ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal]),
                                                                !.

%strans

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,ConsFinalTotal],[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,ConsFinalTotal],Resto):-
                                                                 vocalLatin_Azraelum(Vocal),
                                                                 consTrioLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                                                 consFinalTotalLatin_Azraelum(ConsFinalTotal,[ConsFinal,ConsFinalTotal]),
                                                               append([ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,ConsFinalTotal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,ConsFinalTotal]),
                                                                !.





% blan-ca

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f,Cons1,Vocal1|W],Resto):-
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalLatin_Azraelum(Vocal),
                                          vocalLatin_Azraelum(Vocal1),
                                          (cons_fLatin_Azraelum(Cons_f);pseudocons_fLatin_Azraelum(Cons_f,[Cons_f,Cons1|_])),
                                          consLatin_Azraelum(Cons1),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,Cons1,Vocal1|W]),
                                          !.

%stras-ta

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal],[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,Cons,Vocal2|W],Resto):-
                                                                 vocalLatin_Azraelum(Vocal),
                                                                 vocalLatin_Azraelum(Vocal2),
                                                                 consTrioLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                                                 (cons_fLatin_Azraelum(ConsFinal);pseudocons_fLatin_Azraelum(ConsFinal,[ConsFinal,Cons|_])),
                                                                 consLatin_Azraelum(Cons),
                                                               append([ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,Cons,Vocal2|W]),
                                                                !.

% blan-bla

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsPareja1,Cons_tr1|W],Resto):-
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalLatin_Azraelum(Vocal),
                                          (cons_fLatin_Azraelum(Cons_f);pseudocons_fLatin_Azraelum(Cons_f,[Cons_f,ConsPareja1|_])),
                                          consParejaLatin_Azraelum(ConsPareja1,[ConsPareja1,Cons_tr1|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsPareja1,Cons_tr1|W]),
                                          !.

% stram-bla

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal],[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,ConsPareja1,Cons_tr1|W],Resto):-
                                                                 vocalLatin_Azraelum(Vocal),
                                                                 consTrioLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                                                 consParejaLatin_Azraelum(ConsParejaBis,[ConsPareja1,Cons_trBis|_]),
                                                                 cons_fLatin_Azraelum(ConsFinal),
                                                                 (cons_fLatin_Azraelum(ConsFinal);pseudocons_fLatin_Azraelum(ConsFinal,[ConsFinal,ConsPareja1|_])),
                                                                 consParejaLatin_Azraelum(ConsPareja1,[ConsPareja1,Cons_tr1|_]),
                                                               append([ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,Cons,Vocal2|W]),
                                                                !.



% blans-ca

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalLatin_Azraelum(Vocal),
                                          consFinalTotalLatin_Azraelum(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                          consLatin_Azraelum(Cons1),
                                          not(consParejaLatin_Azraelum(ConsFinalTotal,[ConsFinalTotal,ConsSiguiente])),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W]),
                                          !.

% strans

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,ConsFinalTotal],[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,ConsFinalTotal],Resto):-
                                                                 vocalLatin_Azraelum(Vocal),
                                                                 consTrioLatin_Azraelum(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                                                 consFinalTotalLatin_Azraelum(ConsFinalTotal,[ConsFinal,ConsFinalTotal,ConsSiguiente]),
                                                                 append([ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,ConsFinalTotal]),
                                                                !.




% Triptongo



posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Vocal2,Vocal3],[Cons,Vocal,Vocal2,Vocal3,ConsBis,VocalBis],Resto):-
                                          consLatin_Azraelum(Cons),
                                          consLatin_Azraelum(ConsBis),
                                          triptongoLatin_Azraelum(Vocal,[Vocal,Vocal2,Vocal3]),
                                          vocalLatin_Azraelum(VocalBis),
                                          append([Cons,Vocal,Vocal2,Vocal3],Resto,[Cons,Vocal,Vocal2,Vocal3,ConsBis,VocalBis]),
                                          !.

posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Vocal2,Vocal3],[Cons,Vocal,Vocal2,Vocal3,ConsPareja,Cons_tr|W],Resto):-
                                          consLatin_Azraelum(Cons),
                                          triptongoLatin_Azraelum(Vocal,[Vocal,Vocal2,Vocal3]),
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          append([Cons,Vocal,Vocal2,Vocal3],Resto,[Cons,Vocal,Vocal2,Vocal3,ConsPareja,Cons_tr|W]),
                                          !.

posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Vocal2,Vocal3],[Cons,Vocal,Vocal2,Vocal3],Resto):-
                                          consLatin_Azraelum(Cons),
                                          consLatin_Azraelum(ConsBis),
                                         triptongoLatin_Azraelum(Vocal,[Vocal,Vocal2,Vocal3]),
                                          vocalLatin_Azraelum(VocalBis),
                                          append([Cons,Vocal,Vocal2,Vocal3],Resto,[Cons,Vocal,Vocal2,Vocal3]),
                                          !.

% Triptongo en cons_tr

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],Resto):-
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          triptongoLatin_Azraelum(Vocal,[Vocal,Vocal2,Vocal3]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3]),
                                          !.

% Triptongo +cons final

posibilidadesLatin_Azraelum(Cons,[Cons,Vocal,Vocal2,Vocal3,Cons_f],[Cons,Vocal,Vocal2,Vocal3,Cons_f],Resto):-
                                          consLatin_Azraelum(Cons),
                                          triptongoLatin_Azraelum(Vocal,[Vocal,Vocal2,Vocal3]),
                                          cons_fLatin_Azraelum(Cons_f),
                                          append([Cons,Vocal,Vocal2,Vocal3,Cons_f],Resto,[Cons,Vocal,Vocal2,Vocal3,Cons_f]),
                                          !.

% Triptongo en cons_tr + cons final

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],Resto):-
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          triptongoLatin_Azraelum(Vocal,[Vocal,Vocal2,Vocal3]),
                                          cons_fLatin_Azraelum(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f]),
                                          !.

/*

% Cuadriptongos

posibilidadesLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Vocal4],[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Vocal4],Resto):-
                                          consParejaLatin_Azraelum(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocal_fuerteLatin(Vocal),
                                          vocal_fuerteLatin(Vocal2),
                                          vocal_debilLatin(Vocal3),
                                          cons_fLatin_Azraelum(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f]),
                                          !.


*/


