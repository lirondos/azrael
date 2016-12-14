

catalan(X,Restos,Origen):-
                           catalan(X,Restos),
                           origen_etimologico_catalan(Restos,Origen).

origen_etimologico_catalan([],[]).

origen_etimologico_catalan([Restos|MasRestos],[Origen|MasOrigen]):-
                                                 atom_chars(Restos,Deletreo),
                                                 combinaciones_catalan(Restos,Deletreo,Origen),
                                                 !,
                                                 origen_etimologico_catalan(MasRestos,MasOrigen).


combinaciones_catalan(Restos,[J],desconocido(Restos)).

combinaciones_catalan(Restos,[c,h],antigua(Restos)).

combinaciones_catalan(Restos,[o,o,C|W],griego_o_prefijada(Restos)).

combinaciones_catalan(Restos,[o,e,C|W],griego(Restos)).







combinaciones_catalan(Restos,[X,Y|W],Origen):-
                                       combinaciones_catalan(Restos,[Y|W],Origen).

catalan(X,Restos):-
               transcribir_catalan(X,J),
               !,
               lista_catalan(J,Y),
               comprobar_palabras_catalan(Y,N),
               recuperar_descartes_catalan(N,Restos).


recuperar_descartes_catalan([],[]).

recuperar_descartes_catalan([N|M],[Restos|MasRestos]):-
               atom_codes(Restos,N),
               recuperar_descartes_catalan(M,MasRestos).




% Transcribir convierte las mayusculas en minusculas


transcribir_catalan([],[]).


transcribir_catalan([W|Y],[W|Z]):-
                          W<65,
                          transcribir_catalan(Y,Z).

transcribir_catalan([W|Y],[A|Z]):-
                          W=<90,
                          W>=65,
                          A is W + 32,
                          transcribir_catalan(Y,Z).

transcribir_catalan([W|Y],[A|Z]):-
                          W=<220,
                          W>=192,
                          A is W + 32,
                          transcribir_catalan(Y,Z).

transcribir_catalan([W|Y],[W|Z]):-
                          W>90,
                          transcribir_catalan(Y,Z).




%corte(Inicial, Resfinal, Resprov, Sublista)

lista_catalan(X, Y):- corte_catalan(X, Y, _, []), !.


%%%%METODO AUXILIAR%%%%

%Reglas base
corte_catalan([], Resprov, Resfinal, []):-append(Resprov, [], Resfinal).
corte_catalan([], Resfinal, Resprov, Sublista):-Sublista\=[], append(Resprov, [Sublista], Resfinal).

%listas vacias cuando aparece dos veces consecutivas 1000 cambiar las dos reglas anteriores por corte([], Resfinal, Resprov, Sublista):-append(Resprov, [Sublista], Resfinal).


% Esta regla sirve para evitar meter listas vacias si te encuentras dos veces seguidas 1000

corte_catalan([SignPunt|Resto], Resfinal, Resprov, Sublista):-
                                                    signPunt_catalan(SignPunt),
                                                    Sublista=[],
                                                    corte_catalan(Resto, Resfinal, Resprov, []).

% Reglas recursivas
corte_catalan([SignPunt|Resto], Resfinal, Resprov, Sublista):-
                                                       signPunt_catalan(SignPunt),
                                                       append(Resprov, [Sublista], Res),
                                                       corte_catalan(Resto, Resfinal, Res, []).

corte_catalan([Cab|Resto], Resfinal, Resprov, Sublista):-
                                                   not(signPunt_catalan(Cab)),
                                                   append(Sublista, [Cab], Prov),
                                                   corte_catalan(Resto, Resfinal, Resprov, Prov).



% Signos de puntuacion considerados

signPunt_catalan(32).
signPunt_catalan(33).
signPunt_catalan(34).
signPunt_catalan(38).
signPunt_catalan(40).
signPunt_catalan(41).
signPunt_catalan(44).
signPunt_catalan(45).
signPunt_catalan(46).
signPunt_catalan(47).
signPunt_catalan(58).
signPunt_catalan(59).
signPunt_catalan(63).
signPunt_catalan(132).
signPunt_catalan(147).
signPunt_catalan(148).
signPunt_catalan(161).
signPunt_catalan(191).
signPunt_catalan(171).
signPunt_catalan(187).
signPunt_catalan(8220).
signPunt_catalan(8221).
signPunt_catalan(8211).
signPunt_catalan(91).
signPunt_catalan(93).
signPunt_catalan(92).
signPunt_catalan(8212).
signPunt_catalan(60).
signPunt_catalan(62).
signPunt_catalan(8230).

signPunt_catalan(X):-
             X>47,
             X<58.

comprobar_palabras_catalan([],[]).


comprobar_palabras_catalan([Z|W],Sobras):-
                       silabear_catalan(Z,F),
                       comprobar_palabras_catalan(W,Sobras),
                       !.

comprobar_palabras_catalan([Z|W],[Z|Sobras]):-
                       not(silabear_catalan(Z,F)),
                       comprobar_palabras_catalan(W,Sobras),
                       !.













% El proceso de silabeo se incia con la transformacion de lista de codigo ANSII
% a letras sueltas para poder silabear, y lo manda ya en forma de letras al
% proceso auxiliar de silabeo


% El proceso de silabeo se incia con la transformacion de lista de codigo ANSII a letras sueltas para poder silabear, y lo manda ya en forma de letras al proceso auxiliar de silabeo

silabear_catalan("nosaltres",[nos,al,tres]):-
                       !.
                       
silabear_catalan("vosaltres",[vos,al,tres]):-
                       !.
                       

silabear_catalan(X,Z):-
               atom_codes(Y,X),
               atom_chars(Y,W),
               olfatear_catalan(W),
               silabear_aux_catalan(W,Z).
               


silabeo_catalan(X,Z):-
               silabear_aux_catalan(X,Z).
               

olfatear_catalan([]).

olfatear_catalan([Vocal,z,Vocal2|T]):-
                                                        vocal_catalan(Vocal),
                                                        vocal_catalan(Vocal2),
                           !,
                           fail.

olfatear_catalan([a,e]):-
                           !,
                           fail.

olfatear_catalan([f,f|T]):-
                           !,
                           fail.
                                                   
olfatear_catalan([q,u,u|T]):-
                           !,
                           fail.
                           
olfatear_catalan([t,t|T]):-
                           !,
                           fail.
                           
olfatear_catalan([x,x|T]):-
                           !,
                           fail.

olfatear_catalan([ç,ç|T]):-
                           !,
                           fail.
                           
olfatear_catalan([p,p|T]):-
                           !,
                           fail.
% la P puede ser final de sílaba en cat, así que admite las combinaciones PH en mitad de palabras.
% La saco pq PH es distintivo de latín/francés y ensucia. La única palabra cat que lo contiene es "caphuitada", fusión de "cap" + "h-"
% no tanto pq PH sean consonantes emparejadas
olfatear_catalan([p,h|T]):-
                           !,
                           fail.
                           


olfatear_catalan([X|Y]):-
                 olfatear_catalan(Y).







% El proceso auxiliar de silabeo

silabear_aux_catalan([],[]).




silabear_aux_catalan([Z|T],[Silaba|Silaba_resto]):-
                 posibilidades_catalan(Z,Silabeo,[Z|T],Resto),
                 atom_chars(Silaba,Silabeo),
                 silabear_aux_catalan(Resto,Silaba_resto).



/*
silabear(X,Z):-
               atom_chars(X,W),
               silabear_aux(W,Z).

% El proceso auxiliar de silabeo

silabear_aux([],[]).

silabear_aux([Z|T],[Silaba|Silaba_resto]):-
                 posibilidades_catalan(Z,Silabeo,[Z|T],Resto),
                 atom_chars(Silaba,Silabeo),
                 silabear_aux(Resto,Silaba_resto).

*/











% Letras en español





vocal_catalan(i).
vocal_catalan(u).
vocal_catalan(a).
vocal_catalan(o).
vocal_catalan(e).

vocal_catalan(í).
vocal_catalan(ú).
vocal_catalan(á).
vocal_catalan(ó).
vocal_catalan(é).

vocal_catalan(ì).
vocal_catalan(ù).
vocal_catalan(à).
vocal_catalan(ò).
vocal_catalan(è).

vocal_catalan(ï).
vocal_catalan(ü).

vocal_dieresis(ï).
vocal_dieresis(ü).

/*

vocal_catalan(X):-
               letras_equivalentes_catalan(Z,X),
               vocal_catalan(Z),
               !.
               
*/


cons_f_catalan(m).
cons_f_catalan(s).
cons_f_catalan(t).
cons_f_catalan(p).
cons_f_catalan(t).
cons_f_catalan(c).
cons_f_catalan(l).
cons_f_catalan(g).
cons_f_catalan(r).
cons_f_catalan(n).
cons_f_catalan(d).
cons_f_catalan(ç).
cons_f_catalan(x).
cons_f_catalan(f).



bilabial_catalan(p).
bilabial_catalan(b).

liquida_catalan(r).
liquida_catalan(l).

consFinalTotal_catalan(·,[l,·]).
consFinalTotal_catalan(·,[l,·]).
consFinalTotal_catalan(s,[n,s]).
consFinalTotal_catalan(t,[n,t]).
consFinalTotal_catalan(t,[r,t]).
consFinalTotal_catalan(t,[s,t]).
consFinalTotal_catalan(s,[m,s]).
consFinalTotal_catalan(p,[m,p]).



refinalTotal_catalan(l,[l,l]).
refinalTotal_catalan(b,[m,b]).
refinalTotal_catalan(p,[m,p]).
refinalTotal_catalan(t,[n,t]).
refinalTotal_catalan(t,[r,t]).
refinalTotal_catalan(n,[r,n]).
refinalTotal_catalan(g,[r,g]).
refinalTotal_catalan(y,[n,y]).
refinalTotal_catalan(d,[r,d]).
refinalTotal_catalan(t,[s,t]).
refinalTotal_catalan(c,[n,c]).
refinalTotal_catalan(c,[r,c]).
refinalTotal_catalan(t,[l,t]).
refinalTotal_catalan(ç,[l,ç]).
refinalTotal_catalan(ç,[r,ç]).
refinalTotal_catalan(c,[s,c]).
refinalTotal_catalan(x,[t,x]).

refinalTotal_catalan(s,[Cons1,Cons2,s]):-
                                           refinalTotal_catalan(Cons2,[Cons1,Cons2]).
                                           
refinalTotal_catalan(s,[Cons1,s]):-
                                          cons_f_catalan(Cons1),
                                          Cons1\=s,
                                          Cons1\=g,
                                          Cons1\=ç.






oclusiva_catalan(Cons):-
oclusiva_sorda_catalan(Cons);
oclusiva_sonora_catalan(Cons).

nasal_catalan(n).
nasal_catalan(m).

oclusiva_sorda_catalan(p).
oclusiva_sorda_catalan(t).
oclusiva_sorda_catalan(q).
oclusiva_sorda_catalan(c).

oclusiva_sonora_catalan(b).
oclusiva_sonora_catalan(d).
oclusiva_sonora_catalan(g).


equivalente_catalan(e,e).
equivalente_catalan(e,è).
equivalente_catalan(e,é).
equivalente_catalan(a,á).
equivalente_catalan(a,à).
equivalente_catalan(a,a).
equivalente_catalan(i,í).
equivalente_catalan(i,ì).
equivalente_catalan(i,i).
equivalente_catalan(o,ó).
equivalente_catalan(o,ò).
equivalente_catalan(o,o).
equivalente_catalan(u,u).
equivalente_catalan(u,ù).
equivalente_catalan(u,ú).
equivalente_catalan(A,A).

letras_equivalentes_catalan(A,B):-
                          equivalente_catalan(A,B).

letras_equivalentes_catalan(A,B):-
                          equivalente_catalan(B,A).


letras_equivalentes_catalan(X,Y):-
                          equivalente_catalan(X,Z),
                          equivalente_catalan(Z,Y).

letras_equivalentes_catalan(X,Y):-
                          equivalente_catalan(X,Z),
                          equivalente_catalan(Y,Z).

letras_equivalentes_catalan(X,Y):-
                          equivalente_catalan(Z,X),
                          equivalente_catalan(Z,Y).


















%% Consontantes emparejadas

% b

consPareja_catalan(b,[b,Liquida|_]):-
                              liquida_catalan(Liquida).

% f

consPareja_catalan(f,[f,Liquida|_]):-
                              liquida_catalan(Liquida).

% g

consPareja_catalan(g,[g,Liquida|_]):-
                              liquida_catalan(Liquida).

% c

consPareja_catalan(c,[c,Liquida|_]):-
                              liquida_catalan(Liquida).

% g

consPareja_catalan(p,[p,Liquida|_]):-
                              liquida_catalan(Liquida).

% d

consPareja_catalan(d,[d,r|_]).

% t

consPareja_catalan(t,[t,r|_]).

% ll

consPareja_catalan(l,[l,l|_]).

% qu

consPareja_catalan(q,[q,u|_]).

% qü

consPareja_catalan(q,[q,ü|_]).

% gu

consPareja_catalan(g,[g,u|_]).

% gu

consPareja_catalan(g,[g,ü|_]).

% ny

consPareja_catalan(n,[n,y|_]).





%% Vocales emparejadas: DIPTONGOS

diptongo_catalan(Vocal_a,[Vocal_a,Vocal_i|_]):-
                                            letras_equivalentes_catalan(a,Vocal_a),
                                            letras_equivalentes_catalan(i,Vocal_i).

                                            
diptongo_catalan(Vocal_e,[Vocal_e,Vocal_i|_]):-
                                            letras_equivalentes_catalan(e,Vocal_e),
                                             letras_equivalentes_catalan(i,Vocal_i).
                                             
diptongo_catalan(Vocal_o,[Vocal_o,Vocal_i|_]):-
                                         letras_equivalentes_catalan(i,Vocal_i),
                                         letras_equivalentes_catalan(o,Vocal_o).


diptongo_catalan(Vocal_u,[Vocal_u,Vocal_i|_]):-
                                             letras_equivalentes_catalan(i,Vocal_i),
                                             letras_equivalentes_catalan(u,Vocal_u).
                                            

diptongo_catalan(Vocal_a,[Vocal_a,Vocal_u|_]):-
                                            letras_equivalentes_catalan(a,Vocal_a),
                                            letras_equivalentes_catalan(u,Vocal_u).



diptongo_catalan(Vocal_e,[Vocal_e,Vocal_u|_]):-
                                            letras_equivalentes_catalan(e,Vocal_e),
                                            letras_equivalentes_catalan(u,Vocal_u).
                                             
                                             
                                             
diptongo_catalan(Vocal_i,[Vocal_i,Vocal_u|_]):-
                                             letras_equivalentes_catalan(i,Vocal_i),
                                             letras_equivalentes_catalan(u,Vocal_u).
                                             
diptongo_catalan(Vocal_o,[Vocal_o,Vocal_u|_]):-
                                         letras_equivalentes_catalan(u,Vocal_u),
                                         letras_equivalentes_catalan(o,Vocal_o).
                                             
diptongo_catalan(Vocal_i,[Vocal_i,Vocal_i2|_]):-
                                             letras_equivalentes_catalan(i,Vocal_i),
                                             letras_equivalentes_catalan(i,Vocal_i2).
                                             
diptongo_catalan(Vocal_u,[Vocal_u,Vocal_u2|_]):-
                                             letras_equivalentes_catalan(u,Vocal_u),
                                             letras_equivalentes_catalan(u,Vocal_u2).
                                             
                                             

/*
                                             
diptongo_catalan(Vocal_i,[Vocal_i,Vocal_a|_]):-
                                         letras_equivalentes_catalan(i,Vocal_i),
                                         letras_equivalentes_catalan(a,Vocal_a).
*/
                                         
/*
                                         
diptongo_catalan(Vocal_i,[Vocal_i,Vocal_e|_]):-
                                         letras_equivalentes_catalan(i,Vocal_i),
                                         letras_equivalentes_catalan(e,Vocal_e).


                                         

                                         
diptongo_catalan(Vocal_i,[Vocal_i,Vocal_o|_]):-
                                         letras_equivalentes_catalan(i,Vocal_i),
                                         letras_equivalentes_catalan(o,Vocal_o).



diptongo_catalan(Vocal_u,[Vocal_u,Vocal_a|_]):-
                                         letras_equivalentes_catalan(u,Vocal_u),
                                         letras_equivalentes_catalan(a,Vocal_a).


diptongo_catalan(u,[u,e]).


*/



hiato_catalan(Vocal_dieresis,[Vocal_dieresis,Vocal|_]):-
                                          vocal_catalan(Vocal),
                                          vocal_dieresis(Vocal_dieresis).
                                          
hiato_catalan(Vocal,[Vocal,Vocal_dieresis|_]):-
                                           vocal_catalan(Vocal),
                                          vocal_dieresis(Vocal_dieresis).
                                          
hiato_catalan(Vocal,[Vocal,Vocal2]):-
                                          vocal_catalan(Vocal),
                                          vocal_catalan(Vocal2),
                                          not(diptongo_catalan(Vocal,[Vocal,Vocal2])).
                                          
                                          
/*
hiato_catalan(Vocal_a,[Vocal_a,Vocal_e2|_]):-
                                          letras_equivalentes_catalan(a,Vocal_a),
                                          letras_equivalentes_catalan(e,Vocal_e2).
hiato_catalan(Vocal_a,[Vocal_a,Vocal_o|_]):-
                                         letras_equivalentes_catalan(a,Vocal_a),
                                         letras_equivalentes_catalan(o,Vocal_o).
                                         




hiato_catalan(Vocal_e,[Vocal_e,Vocal_a|_]):-
                                         letras_equivalentes_catalan(a,Vocal_a),
                                         letras_equivalentes_catalan(e,Vocal_e).


hiato_catalan(Vocal_e,[Vocal_e,Vocal_e2|_]):-
                                          letras_equivalentes_catalan(e,Vocal_e),
                                          letras_equivalentes_catalan(e,Vocal_e2).
hiato_catalan(Vocal_e,[Vocal_e,Vocal_o|_]):-
                                         letras_equivalentes_catalan(e,Vocal_e),
                                         letras_equivalentes_catalan(o,Vocal_o).


hiato_catalan(Vocal_o,[Vocal_o,Vocal_a|_]):-
                                         letras_equivalentes_catalan(a,Vocal_a),
                                         letras_equivalentes_catalan(o,Vocal_o).

hiato_catalan(Vocal_o,[Vocal_o,Vocal_e|_]):-
                                         letras_equivalentes_catalan(e,Vocal_e),
                                         letras_equivalentes_catalan(o,Vocal_o).

hiato_catalan(Vocal_u,[Vocal_u,ï|_]):-
                                         letras_equivalentes_catalan(u,Vocal_u).

hiato_catalan(Vocal_a,[Vocal_a,ï|_]):-
                                         letras_equivalentes_catalan(a,Vocal_a).


*/



triptongo_catalan(i,[i,Vocal_a,i]):-
                                             letras_equivalentes_catalan(a,Vocal_a).

triptongo_catalan(i,[i,Vocal_e,u]):-
                                             letras_equivalentes_catalan(e,Vocal_e).

triptongo_catalan(u,[u,Vocal_a,i]):-
                                             letras_equivalentes_catalan(a,Vocal_a).

triptongo_catalan(u,[u,Vocal_e,u]):-
                                             letras_equivalentes_catalan(e,Vocal_e).

triptongo_catalan(u,[u,Vocal_e,i]):-
                                             letras_equivalentes_catalan(e,Vocal_e).
                                             
triptongo_catalan(h,[h,i,e]).

triptongo_catalan(h,[h,i,a]).


                                         

                                         
                                         



triptongo_catalan(Vocal_i,[Vocal_i,Vocal_e,Vocal_i2]):-
                                                    letras_equivalentes_catalan(i,Vocal_i),
                                                    letras_equivalentes_catalan(e,Vocal_e),
                                                    letras_equivalentes_catalan(i,Vocal_i2).




%% Consontantes pseudofinales: contextos

% Letra m

pseudocons_f_catalan(m,[m,Bilabial|_]):-
                                  bilabial_catalan(Bilabial).


pseudocons_f_catalan(m,[m,n|_]).



% Letra g


pseudocons_f_catalan(g,[g,n|_]).

pseudocons_f_catalan(g,[g,m|_]).


% Letra p

dental_catalan(t).
dental(c).

pseudocons_f_catalan(p,[p,Dentales|_]):-
                                  dental_catalan(Dentales);
                                   Dentales=s;
                                   Dentales=p.




% Letra x

pseudocons_f_catalan(x,[x,t|_]).


% Letra c

pseudocons_f_catalan(c,[c,c|_]).



pseudocons_f_catalan(c,[c,t|_]).

% Letra b


pseudocons_f_catalan(b,[b,t|_]).




cons_catalan(b).
cons_catalan(c).
cons_catalan(d).
cons_catalan(f).
cons_catalan(g).
cons_catalan(h).
cons_catalan(l).
cons_catalan(m).
cons_catalan(n).
cons_catalan(p).
cons_catalan(r).
cons_catalan(s).
cons_catalan(t).
cons_catalan(v).
cons_catalan(x).
cons_catalan(j).
cons_catalan(w).
cons_catalan(z).
cons_catalan(i).
cons_catalan(ç).
cons_catalan(u).

% Silabas en español

% posibilidades/4: letra que encabeza la silaba/silaba/letras que siguen/resto de la palabra (sin la silaba que acabamos de aislar, pero si con el contexto.

%% PREFIJOS



posibilidades_catalan(s,[s,u,b,s],[s,u,b,s,Cons|W],Resto):-
                                                                not(vocal_catalan(Cons)),
                                                                append([s,u,b,s],Resto,[s,u,b,s,Cons|W]),
                                                                !.




posibilidades_catalan(s,[s,u,b],[s,u,b,Cons|W],Resto):-
                                                                not(vocal_catalan(Cons)),
                                                                append([s,u,b],Resto,[s,u,b,Cons|W]),
                                                              !.




posibilidades_catalan(o,[o,b,s],[o,b,s,Cons|W],Resto):-
                                                                not(vocal_catalan(Cons)),
                                                                append([o,b,s],Resto,[o,b,s,Cons|W]),
                                                                 !.





posibilidades_catalan(o,[o,b],[o,b,Cons|W],Resto):-
                                                                not(vocal_catalan(Cons)),
                                                                not(consPareja_catalan(b,[b,Cons])),
                                                                append([o,b],Resto,[o,b,Cons|W]),
                                                                 !.
                                                                                                                                 
                                                                                                                                 
posibilidades_catalan(a,[a,b],[a,b,Cons|W],Resto):-
                                                                not(vocal_catalan(Cons)),
                                                                not(consPareja_catalan(b,[b,Cons])),
                                                                append([a,b],Resto,[a,b,Cons|W]),
                                                                 !.
                                                                 
posibilidades_catalan(a,[a,n],[a,n,Cons,Cons2|W],Resto):-
                                                                not(consPareja_catalan(n,[n,Cons])),
                                                                append([a,n],Resto,[a,n,Cons,Cons2|W]),
                                                                 !.
                                                                 
posibilidades_catalan(c,[c,o,n,s],[c,o,n,s,Cons,Cons2|W],Resto):-
                                                                append([c,o,n,s],Resto,[c,o,n,s,Cons,Cons2|W]),
                                                                 !.
                                                                 
posibilidades_catalan(c,[c,o,n],[c,o,n,Cons,Cons2|W],Resto):-
                                                                append([c,o,n],Resto,[c,o,n,Cons,Cons2|W]),
                                                                 !.
                                                                 
posibilidades_catalan(e,[e,n],[e,n,Cons,Cons2|W],Resto):-
                                                                append([e,n],Resto,[e,n,Cons,Cons2|W]),
                                                                 !.
posibilidades_catalan(i,[i,n],[i,n,Cons,Cons2|W],Resto):-
                                                                not(consPareja_catalan(n,[n,Cons])),
                                                                append([i,n],Resto,[i,n,Cons,Cons2|W]),
                                                                 !.

posibilidades_catalan(i,[i,n,s],[i,n,s,Cons,Cons2|W],Resto):-
                                                                append([i,n,s],Resto,[i,n,s,Cons,Cons2|W]),
                                                                 !.
                                                                 
posibilidades_catalan(d,[d,e,s],[d,e,s,Cons,Cons2|W],Resto):-
                                                                append([d,e,s],Resto,[d,e,s,Cons,Cons2|W]),
                                                                 !.
                                                                 
posibilidades_catalan(e,[e,x],[e,x,Cons,Cons2|W],Resto):-
                                                                append([e,x],Resto,[e,x,Cons,Cons2|W]),
                                                                !.
                                                                
posibilidades_catalan(t,[t,r,a,n,s],[t,r,a,n,s,Cons,Cons2|W],Resto):-
                                                                append([t,r,a,n,s],Resto,[t,r,a,n,s,Cons,Cons2|W]),
                                                                !.

posibilidades_catalan(a,[a,d],[a,d,h|W],Resto):-
                                                                append([a,d],Resto,[a,d,h|W]),
                                                                !.


%ens-ta

posibilidades_catalan(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                                                 vocal_catalan(Vocal),
                                                                 cons_catalan(ConsSiguiente),
                                                                  consFinalTotal_catalan(ConsFinalTotal,[Cons_f,ConsFinalTotal]),
                                                                append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal|W]),
                                                                !.






%ex
/*
posibilidades_catalan(e,[e,x],[e,x,Cons|W],Resto):-
                                                                cons_catalan(Cons),
                                                                Cons\=i,
                                                                append([e,x],Resto,[e,x,Cons|W]),
                                                                 !.
                                                                 
*/

posibilidades_catalan(Vocal,[Vocal,Vocal2,Vocal3],[Vocal,Vocal2,Vocal3|W],Resto):-
                                         triptongo_catalan(Vocal,[Vocal,Vocal2,Vocal3]),
                                          append([Vocal,Vocal2,Vocal3],Resto,[Vocal,Vocal2,Vocal3|W]),
                                          !.

%cients

posibilidades_catalan(Cons,[Cons,Vocal,Vocal2,Cons_f,Refinal,Final],[Cons,Vocal,Vocal2,Cons_f,Refinal,Final],Resto):-
                                          cons_catalan(Cons),
                                          diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                          refinalTotal_catalan(Final,[Cons_f,Refinal,Final]),
                                          append([Cons,Vocal,Vocal2,Cons_f,Refinal,Final],Resto,[Cons,Vocal,Vocal2,Cons_f,Refinal,Final]),
                                          !.
% rioll

posibilidades_catalan(Cons,[Cons,Vocal,Vocal2,Cons_f,ConsRefinal],[Cons,Vocal,Vocal2,Cons_f,ConsRefinal],Resto):-
                                          cons_catalan(Cons),
                                          diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                          refinalTotal_catalan(ConsRefinal,[Cons_f,ConsRefinal]),
                                          append([Cons,Vocal,Vocal2,Cons_f,ConsRefinal],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsRefinal]),
                                          !.

% riolls

posibilidades_catalan(Cons,[Cons,Vocal,Vocal2,Cons_f,ConsRefinal,ConsF],[Cons,Vocal,Vocal2,Cons_f,ConsRefinal,ConsF],Resto):-
                                          cons_catalan(Cons),
                                          diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                          refinalTotal_catalan(ConsF,[Cons_f,ConsRefinal,ConsF]),
                                          append([Cons,Vocal,Vocal2,Cons_f,ConsRefinal,ConsF],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsRefinal,ConsF]),
                                          !.


posibilidades_catalan(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2],Resto):-
                                          cons_catalan(Cons),
                                          diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2]),
                                          !.
                                          
%panc-to

posibilidades_catalan(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal],[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                                                  cons_catalan(Cons),
                                                                 vocal_catalan(Vocal),
                                                                 consFinalTotal_catalan(ConsFinalTotal,[Cons_f,ConsFinalTotal]),
                                                                append([Cons,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W]),
                                                                !.
                                                                


% antro, an-cla

posibilidades_catalan(Vocal,[Vocal,Cons_f],[Vocal,Cons_f,ConsPareja,Cons_tr,Vocal2|W],Resto):-
                                                                 vocal_catalan(Vocal),
                                                                (cons_f_catalan(Cons_f);pseudocons_f_catalan(Cons_f,[Cons_f,ConsPareja|_])),
                                                                not(consPareja_catalan(Cons_f,[Cons_f,ConsPareja])),
                                                                consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr]),
                                                                vocal_catalan(Vocal2),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f,ConsPareja,Cons_tr,Vocal2|W]),
                                                                !.

% posibilidaddes para CONS+VOCAL EN PALABRA


posibilidades_catalan(Cons,[Cons,Vocal],[Cons,Vocal,Cons1,Vocal1|W],Resto):-
                                          cons_catalan(Cons),
                                          vocal_catalan(Vocal),
                                          cons_catalan(Cons1),
                                          vocal_catalan(Vocal1),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Cons1,Vocal1|W]),
                                          !.

%tirps

posibilidades_catalan(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal,Refinal],[Cons,Vocal,Cons_f,ConsFinalTotal,Refinal],Resto):-
                                                                  cons_catalan(Cons),
                                                                 vocal_catalan(Vocal),
                                                               refinalTotal_catalan(Refinal,[Cons_f,ConsFinalTotal,Refinal]),
                                                                append([Cons,Vocal,Cons_f,ConsFinalTotal,Refinal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,Refinal]),
                                                                !.

%els

posibilidades_catalan(Vocal,[Vocal,Cons_f,Refinal],[Vocal,Cons_f,Refinal],Resto):-
                                                                   vocal_catalan(Vocal),
                                                               refinalTotal_catalan(Refinal,[Cons_f,Refinal]),
                                                                append([Vocal,Cons_f,Refinal],Resto,[Vocal,Cons_f,Refinal]),
                                                                !.


%anys

posibilidades_catalan(Vocal,[Vocal,Cons_f,Refinal,ConsF],[Vocal,Cons_f,Refinal,ConsF],Resto):-
                                                                   vocal_catalan(Vocal),
                                                               refinalTotal_catalan(ConsF,[Cons_f,Refinal,ConsF]),
                                                                append([Vocal,Cons_f,Refinal,ConsF],Resto,[Vocal,Cons_f,Refinal,ConsF]),
                                                                !.
                                                                
%mels

posibilidades_catalan(Cons,[Cons,Vocal,Cons_f,Refinal],[Cons,Vocal,Cons_f,Refinal],Resto):-
                                                                   vocal_catalan(Vocal),
                                                               refinalTotal_catalan(Refinal,[Cons_f,Refinal]),
                                                                append([Cons,Vocal,Cons_f,Refinal],Resto,[Cons,Vocal,Cons_f,Refinal]),
                                                                !.

%ments

posibilidades_catalan(Cons,[Cons,Vocal,Cons_f,Refinal,ConsF],[Cons,Vocal,Cons_f,Refinal,ConsF],Resto):-
                                                                   vocal_catalan(Vocal),
                                                               refinalTotal_catalan(Refinal,[Cons_f,Refinal,ConsF]),
                                                                append([Cons,Vocal,Cons_f,Refinal,ConsF],Resto,[Cons,Vocal,Cons_f,Refinal,ConsF]),
                                                                !.


% post-quam



posibilidades_catalan(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal],[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,Cons_tr|W],Resto):-
                                          cons_catalan(Cons),
                                          vocal_catalan(Vocal),
                                          consPareja_catalan(ConsSiguiente,[ConsSiguiente,Cons_tr]),
                                          consFinalTotal_catalan(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                          append([Cons,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,Cons_tr|W]),
                                          !.



% ins-tru



posibilidades_catalan(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,Cons_tr|W],Resto):-
                                          vocal_catalan(Vocal),
                                          consPareja_catalan(ConsSiguiente,[ConsSiguiente,Cons_tr]),
                                          consFinalTotal_catalan(ConsFinalTotal,[Cons_f,ConsFinalTotal]),
                                          append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,Cons_tr|W]),
                                          !.

% Au-un

posibilidades_catalan(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,Vocal3|W],Resto):-
                                                                diptongo_catalan(Vocal,[Vocal,Vocal2]),
                                                                hiato_catalan(Vocal2,[Vocal2,Vocal3]),
                                                                append([Vocal,Vocal2],Resto,[Vocal,Vocal2,Vocal3|W]),
                                                                !.


%nunc

posibilidades_catalan(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal],[Cons,Vocal,Cons_f,ConsFinalTotal],Resto):-
                                                                  cons_catalan(Cons),
                                                                 vocal_catalan(Vocal),
                                                                consFinalTotal_catalan(ConsFinalTotal,[Cons_f,ConsFinalTotal]),
                                                                append([Cons,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal]),
                                                                !.

%tirps

posibilidades_catalan(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal,Refinal],[Cons,Vocal,Cons_f,ConsFinalTotal,Refinal],Resto):-
                                                                  cons_catalan(Cons),
                                                                 vocal_catalan(Vocal),
                                                              refinalTotal_catalan(Refinal,[Cons_f,ConsFinalTotal,Refinal]),
                                                                append([Cons,Vocal,Cons_f,ConsFinalTotal,Refinal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,Refinal]),
                                                                !.




% brai-le

posibilidades_catalan(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2,ConsBis,VocalBis|W],Resto):-
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                          cons_catalan(ConsBis),
                                          vocal_catalan(VocalBis),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,ConsBis,VocalBis|W]),
                                          !.

% post-quam



posibilidades_catalan(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal],[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,Cons_tr|W],Resto):-
                                          cons_catalan(Cons),
                                          vocal_catalan(Vocal),
                                          consPareja_catalan(ConsSiguiente,[ConsSiguiente,Cons_tr]),
                                          consFinalTotal_catalan(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                          append([Cons,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,Cons_tr|W]),
                                          !.



% antro, an-cla

posibilidades_catalan(Vocal,[Vocal,Cons_f],[Vocal,Cons_f,ConsPareja,Cons_tr,Vocal2|W],Resto):-
                                                                 vocal_catalan(Vocal),
                                                                (cons_f_catalan(Cons_f);pseudocons_f_catalan(Cons_f,[Cons_f,ConsPareja|_])),
                                                                not(consPareja_catalan(Cons_f,[Cons_f,ConsPareja])),
                                                                consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr]),
                                                                vocal_catalan(Vocal2),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f,ConsPareja,Cons_tr,Vocal2|W]),
                                                                !.




% posibilidades para CONS+VOCAL AISLADO

posibilidades_catalan(Cons,[Cons,Vocal],[Cons,Vocal],Resto):-
                                          cons_catalan(Cons),
                                          vocal_catalan(Vocal),
                                          append([Cons,Vocal],Resto,[Cons,Vocal]),
                                          !.

% posibilidaddes para CONS+VOCAL EN PALABRA


posibilidades_catalan(Cons,[Cons,Vocal],[Cons,Vocal,Cons1,Vocal1|W],Resto):-
                                          cons_catalan(Cons),
                                          vocal_catalan(Vocal),
                                          cons_catalan(Cons1),
                                          vocal_catalan(Vocal1),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Cons1,Vocal1|W]),
                                          !.
                                          


% nu-blar  pa-dre

posibilidades_catalan(Cons,[Cons,Vocal],[Cons,Vocal,ConsPareja,Cons_tr|W],Resto):-
                                          cons_catalan(Cons),
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr]),
                                          vocal_catalan(Vocal),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,ConsPareja,Cons_tr|W]),
                                          !.

% CONS+VOCAL+CONS_F: pen-sar. OJO: para el latin es importante que esta regla
% aparezca aqui. Res-su-co; al ir la s tb como vocal, si cambiamos el orden, lo
% interpreta como re-s-su-co.


posibilidades_catalan(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f,Cons1,Vocal1|W],Resto):-
                                          cons_catalan(Cons),
                                          vocal_catalan(Vocal),
                                          (cons_f_catalan(Cons_f);pseudocons_f_catalan(Cons_f,[Cons_f,Cons1|_])),
                                          cons_catalan(Cons1),
                                          vocal_catalan(Vocal1),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f,Cons1,Vocal1|W]),
                                          !.


% posibilidades para CONS+VOCAL+VOCAL AISLADO:

posibilidades_catalan(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2],Resto):-
                                          cons_catalan(Cons),
                                          diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2]),
                                          !.



% posibilidaddes para CONS+VOCAL+VOCAL EN PALABRA:

posibilidades_catalan(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2,Cons1,Vocal1|W],Resto):-
                                          cons_catalan(Cons),
                                          diptongo_catalan(Vocal,[Vocal,Vocal2]),
                                          cons_catalan(Cons1),
                                          vocal_catalan(Vocal1),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2,Cons1,Vocal1|W]),
                                          !.






% nai-blar; nai-trar

posibilidades_catalan(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2,ConsPareja,Cons_tr|W],Resto):-
                                          cons_catalan(Cons),
                                          diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2,ConsPareja,Cons_tr|W]),
                                          !.

% CONS+VOCAL+VOCAL+CONS_FIN con dipt descendente AISLADO: a-ma-bais

posibilidades_catalan(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f],Resto):-
                                          cons_catalan(Cons),
                                          diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                          cons_f_catalan(Cons_f),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f]),
                                          !.



% CONS+VOCAL+VOCAL+CONS_F: DIPT DESCENDENTE mais-car



posibilidades_catalan(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f,Cons1,Vocal1|W],Resto):-
                                          cons_catalan(Cons),
                                          diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                          (cons_f_catalan(Cons_f);pseudocons_f_catalan(Cons_f,[Cons_f,Cons1|_])),
                                          cons_catalan(Cons1),
                                          vocal_catalan(Vocal1),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f,Cons1,Vocal1|W]),
                                          !.

% CONS+VOCAL+VOCAL+CONS_F: DIPT DESCENDENTE mais-bla



posibilidades_catalan(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f,ConsTipo,Cons_tr|W],Resto):-
                                          cons_catalan(Cons),
                                          diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                          (cons_f_catalan(Cons_f);pseudocons_f_catalan(Cons_f,[Cons_f,ConsTipo|_])),
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsPareja,Cons_tr|W]),
                                          !.


% CONS+VOCAL+VOCAL+CONS_F*CONSFINALTOTAL: DIPT DESCENDENTE mains-bla



posibilidades_catalan(Cons,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                          cons_catalan(Cons),
                                          diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                          consFinalTotal_catalan(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                          append([Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal,ConsSiguiente|_]),
                                          !.






%neins

posibilidades_catalan(Cons,[Cons,Vocal,Vocal2,Cons_f,Refinal],[Cons,Vocal,Vocal2,Cons_f,Refinal],Resto):-
                                          cons_catalan(Cons),
                                          diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                          refinalTotal_catalan(Refinal,[Cons_f,Refinal]),
                                          append([Cons,Vocal,Vocal2,Cons_f,Refinal],Resto,[Cons,Vocal,Vocal2,Cons_f,Refinal]),
                                          !.

%cients

posibilidades_catalan(Cons,[Cons,Vocal,Vocal2,Cons_f,Refinal,Final],[Cons,Vocal,Vocal2,Cons_f,Refinal,Final],Resto):-
                                          cons_catalan(Cons),
                                          diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                          refinalTotal_catalan(Refinal,[Cons_f,Refinal,Final]),
                                          append([Cons,Vocal,Vocal2,Cons_f,Refinal,Final],Resto,[Cons,Vocal,Vocal2,Cons_f,Refinal,Final]),
                                          !.

% CONS+VOCAL+CONS_F: pen-trar


posibilidades_catalan(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f,ConsPareja,Cons_tr|W],Resto):-
                                          cons_catalan(Cons),
                                          vocal_catalan(Vocal),
                                          (cons_f_catalan(Cons_f);pseudocons_f_catalan(Cons_f,[Cons_f,ConsPareja|W])),
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr]),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f,ConsPareja,Cons_tr|W]),
                                          !.

% CONS+VOCAL+CONS_F: pen-trar


posibilidades_catalan(Cons,[Cons,Vocal,'\'',ConsBis],[Cons,Vocal,'\'',ConsBis],Resto):-
                                          cons_catalan(Cons),
                                          vocal_catalan(Vocal),
                                          cons_catalan(ConsBis),
                                          append([Cons,Vocal,'\'',ConsBis],Resto,[Cons,Vocal,'\'',ConsBis]),
                                          !.






% CONS+VOCAL+CONS_FIN EN FINAL DE PALABRA: del-fin


posibilidades_catalan(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f],Resto):-
                                          cons_catalan(Cons),
                                          vocal_catalan(Vocal),
                                          cons_f_catalan(Cons_f),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f]),
                                          !.

                                                                           





%%% silabas que empiezan por vocal

%% OJO PARA TODO EL CÓDIGO QUE SIGUE: OI, AU y AI son los unicos dipt que forman dipt tal cual. Los desmas son extranj: comprobar

% a


posibilidades_catalan(Vocal,[Vocal],[Vocal],Resto):-
                                                                vocal_catalan(Vocal),
                                                                append([Vocal],Resto,[Vocal]),
                                                                !.
% a-la

posibilidades_catalan(Vocal,[Vocal],[Vocal,Cons,Vocal1|W],Resto):-
                                                                vocal_catalan(Vocal),
                                                                cons_catalan(Cons),
                                                                vocal_catalan(Vocal1),
                                                                append([Vocal],Resto,[Vocal,Cons,Vocal1|W]),
                                                                !.
%anc-to

posibilidades_catalan(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                                                 vocal_catalan(Vocal),
                                                                 consFinalTotal_catalan(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                                                append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W]),
                                                                !.








% a-pro-xi-mar-se; a-dri

posibilidades_catalan(Vocal,[Vocal],[Vocal,ConsPareja,Cons_tr,Vocal2|W],Resto):-
                                                                 vocal_catalan(Vocal),
                                                                 consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                                               append([Vocal],Resto,[Vocal,ConsPareja,Cons_tr,Vocal2|W]),
                                                                !.





% ai-rar, au-lar, eusebio

posibilidades_catalan(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,Cons,Vocal1|W],Resto):-
                                                                        diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                                                        cons_catalan(Cons),
                                                                        vocal_catalan(Vocal1),
                                                                        append([Vocal,Vocal2],Resto,[Vocal,Vocal2,Cons,Vocal1|W]),
                                                                         !.


% ai-trar

posibilidades_catalan(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,ConsPareja,Cons_tr|W],Resto):-
                                                                        diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                                                        consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr]),
                                                                        append([Vocal,Vocal2],Resto,[Vocal,Vocal2,ConsPareja,Cons_tr|W]),
                                                                         !.




% a-ma-ri-AIS

posibilidades_catalan(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f],Resto):-
                                                                diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                                                cons_f_catalan(Cons_f),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f]),
                                                                !.


% AIS-lar

posibilidades_catalan(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f,Cons|W],Resto):-
                                                                diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                                                (cons_f_catalan(Cons_f);pseudocons_f_catalan(Cons_f,[Cons_f,Cons|_])),
                                                                cons_catalan(Cons),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f,Cons|W]),
                                                                !.

% AIS-que

posibilidades_catalan(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f,Cons,Constr|W],Resto):-
                                                                diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                                                (cons_f_catalan(Cons_f);pseudocons_f_catalan(Cons_f,[Cons_f,Cons|_])),
                                                                consPareja_catalan(Cons,[Cons,Constr]),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f,Cons,Constr|W]),
                                                                !.


% Au-un

posibilidades_catalan(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,Vocal3|W],Resto):-
                                                                diptongo_catalan(Vocal,[Vocal,Vocal2]),
                                                                hiato_catalan(Vocal2,[Vocal2,Vocal3]),
                                                                append([Vocal,Vocal2],Resto,[Vocal,Vocal2,Vocal3|W]),
                                                                !.






% a-e-re-o

posibilidades_catalan(Vocal,[Vocal],[Vocal,Vocal2|W],Resto):-
                                                                 hiato_catalan(Vocal,[Vocal,Vocal2|_]),
                                                                 append([Vocal],Resto,[Vocal,Vocal2|W]),
                                                                 !.

% en

posibilidades_catalan(Vocal,[Vocal,Cons_f],[Vocal,Cons_f],Resto):-
                                                                vocal_catalan(Vocal),
                                                                cons_f_catalan(Cons_f),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f]),
                                                                !.


% is-la

posibilidades_catalan(Vocal,[Vocal,Cons_f],[Vocal,Cons_f,ConsBis,VocalBis|W],Resto):-
                                                                 vocal_catalan(Vocal),
                                                                (cons_f_catalan(Cons_f);pseudocons_f_catalan(Cons_f,[Cons_f,ConsBis|W])),
                                                                cons_catalan(ConsBis),
                                                                vocal_catalan(VocalBis),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f,ConsBis,VocalBis|W]),
                                                                !.






%ens

posibilidades_catalan(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal],Resto):-
                                                                 vocal_catalan(Vocal),
                                                                refinalTotal_catalan(ConsFinalTotal,[Cons_f,ConsFinalTotal]),
                                                                append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal]),
                                                                !.

%urbs

posibilidades_catalan(Vocal,[Vocal,Cons_f,ConsFinalTotal,Refinal],[Vocal,Cons_f,ConsFinalTotal,Refinal],Resto):-
                                                                vocal_catalan(Vocal),
                                                               refinalTotal_catalan(Refinal,[Cons_f,ConsFinalTotal,Refinal]),
                                                                append([Vocal,Cons_f,ConsFinalTotal,Refinal],Resto,[Vocal,Cons_f,ConsFinalTotal,Refinal]),
                                                                !.





% ay, uy: unicas palabras encontradas con dipt que solo son eso: el dipt, pero meto todos los casos pq nada lo prohibe. No meto el dipt ascendente ni el doble pq no parece probable

posibilidades_catalan(Vocal,[Vocal,Vocal2],[Vocal,Vocal2],Resto):-
                                                                diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                                                append([Vocal,Vocal2],Resto,[Vocal,Vocal2]),
                                                                !.


% ea: hiato aislado. Solo encontrado "ea", pero meto todo porque nada lo impide

posibilidades_catalan(Vocal,[Vocal],[Vocal,Vocal2],Resto):-
                                                                hiato_catalan(Vocal,[Vocal,Vocal2|_]),
                                                                append([Vocal],Resto,[Vocal,Vocal2]),
                                                                !.




% tue-or

posibilidades_catalan(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2,Vocal3|W],Resto):-
                                          cons_catalan(Cons),
                                          diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                          hiato_catalan(Vocal2,[Vocal2,Vocal3|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2,Vocal3|W]),
                                          !.

%% Caso particular: trabadas en mitad de palabra






% bla

posibilidades_catalan(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal],Resto):-
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocal_catalan(Vocal),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal]),
                                          !.






% gra-pa

posibilidades_catalan(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,Cons,Vocal1|W],Resto):-
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocal_catalan(Vocal),
                                          vocal_catalan(Vocal1),
                                          cons_catalan(Cons),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,Cons,Vocal1|W]),
                                          !.



% blau?

posibilidades_catalan(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2],Resto):-
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2]),
                                          !.


% blau-bla

posibilidades_catalan(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2,ConsParejaBis,Cons_trBis|W],Resto):-
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                          consPareja_catalan(ConsParejaBis,[ConsParejaBis,Cons_trBis|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,ConsParejaBis,Cons_trBis|W]),
                                          !.



% blaun

posibilidades_catalan(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto):-
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                          cons_f_catalan(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f]),
                                       !.
                                                                           
% grills

posibilidades_catalan(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsRefinal,Total],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsRefinal,Total],Resto):-
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          cons_catalan(Cons),
                                           refinalTotal_catalan(Total,[Cons_f,ConsRefinal,Total]),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f,ConsRefinal,Total],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsRefinal,Total]),
                                          !.



% brains

posibilidades_catalan(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsRefinal],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsRefinal],Resto):-
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                          cons_catalan(Cons),
                                           refinalTotal_catalan(ConsRefinal,[Cons_f,ConsRefinal]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsRefinal],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsRefinal]),
                                          !.
                                                                                  


% brain-le

posibilidades_catalan(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,Cons|W],Resto):-
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongo_catalan(Vocal,[Vocal,Vocal2|_]),
                                          cons_catalan(Cons),
                                          (cons_f_catalan(Cons_f);pseudocons_f_catalan(Cons_f,[Cons_f,Cons|_])),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,Cons|W]),
                                          !.






% pro-e-mio

posibilidades_catalan(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,Vocal2|W],Resto):-
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          hiato_catalan(Vocal,[Vocal,Vocal2|_]),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2|W]),
                                          !.





% bla-bla

posibilidades_catalan(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,ConsPareja1,Cons_tr1|W],Resto):-
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocal_catalan(Vocal),
                                          consPareja_catalan(ConsPareja1,[ConsPareja1,Cons_tr1|_]),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,ConsPareja1,Cons_tr1|W]),
                                          !.



% blas

posibilidades_catalan(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f],Resto):-
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocal_catalan(Vocal),
                                          cons_f_catalan(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f]),
                                          !.











% blan-ca

posibilidades_catalan(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f,Cons1,Vocal1|W],Resto):-
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocal_catalan(Vocal),
                                          vocal_catalan(Vocal1),
                                          (cons_f_catalan(Cons_f);pseudocons_f_catalan(Cons_f,[Cons_f,Cons1|_])),
                                          cons_catalan(Cons1),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,Cons1,Vocal1|W]),
                                          !.


% blan-bla

posibilidades_catalan(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsPareja1,Cons_tr1|W],Resto):-
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocal_catalan(Vocal),
                                          (cons_f_catalan(Cons_f);pseudocons_f_catalan(Cons_f,[Cons_f,ConsPareja1|_])),
                                          consPareja_catalan(ConsPareja1,[ConsPareja1,Cons_tr1|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsPareja1,Cons_tr1|W]),
                                          !.
                                          





% Triptongo



posibilidades_catalan(Cons,[Cons,Vocal,Vocal2,Vocal3],[Cons,Vocal,Vocal2,Vocal3,ConsBis,VocalBis],Resto):-
                                          cons_catalan(Cons),
                                          cons_catalan(ConsBis),
                                          triptongo_catalan(Vocal,[Vocal,Vocal2,Vocal3]),
                                          vocal_catalan(VocalBis),
                                          append([Cons,Vocal,Vocal2,Vocal3],Resto,[Cons,Vocal,Vocal2,Vocal3,ConsBis,VocalBis]),
                                          !.

posibilidades_catalan(Cons,[Cons,Vocal,Vocal2,Vocal3],[Cons,Vocal,Vocal2,Vocal3,ConsPareja,Cons_tr|W],Resto):-
                                          cons_catalan(Cons),
                                          triptongo_catalan(Vocal,[Vocal,Vocal2,Vocal3]),
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          append([Cons,Vocal,Vocal2,Vocal3],Resto,[Cons,Vocal,Vocal2,Vocal3,ConsPareja,Cons_tr|W]),
                                          !.

posibilidades_catalan(Cons,[Cons,Vocal,Vocal2,Vocal3],[Cons,Vocal,Vocal2,Vocal3],Resto):-
                                          cons_catalan(Cons),
                                          cons_catalan(ConsBis),
                                         triptongo_catalan(Vocal,[Vocal,Vocal2,Vocal3]),
                                          vocal_catalan(VocalBis),
                                          append([Cons,Vocal,Vocal2,Vocal3],Resto,[Cons,Vocal,Vocal2,Vocal3]),
                                          !.

% Triptongo en cons_tr

posibilidades_catalan(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],Resto):-
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          triptongo_catalan(Vocal,[Vocal,Vocal2,Vocal3]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3]),
                                          !.

% Triptongo +cons final

posibilidades_catalan(Cons,[Cons,Vocal,Vocal2,Vocal3,Cons_f],[Cons,Vocal,Vocal2,Vocal3,Cons_f],Resto):-
                                          cons_catalan(Cons),
                                          triptongo_catalan(Vocal,[Vocal,Vocal2,Vocal3]),
                                          cons_f_catalan(Cons_f),
                                          append([Cons,Vocal,Vocal2,Vocal3,Cons_f],Resto,[Cons,Vocal,Vocal2,Vocal3,Cons_f]),
                                          !.

% Triptongo en cons_tr + cons final

posibilidades_catalan(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],Resto):-
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          triptongo_catalan(Vocal,[Vocal,Vocal2,Vocal3]),
                                          cons_f_catalan(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f]),
                                          !.


% Triptongo +cons final

posibilidades_catalan(Cons,[Cons,Vocal,Vocal2,Vocal3],[Cons,Vocal,Vocal2,Vocal3,ConsBis,VocalBis|W],Resto):-
                                          cons_catalan(Cons),
                                          triptongo_catalan(Vocal,[Vocal,Vocal2,Vocal3]),
                                          cons_catalan(ConsBis),
                                                        vocal_catalan(VocalBis),
                                          append([Cons,Vocal,Vocal2,Vocal3],Resto,[Cons,Vocal,Vocal2,Vocal3ConsBis,VocalBis|W]),
                                          !.
                                          
% blans-ca

posibilidades_catalan(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocal_catalan(Vocal),
                                          consFinalTotal_catalan(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                          cons_catalan(Cons1),
                                          not(consPareja_catalan(ConsFinalTotal,[ConsFinalTotal,ConsSiguiente])),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W]),
                                          !.
                                          
%blast

posibilidades_catalan(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsRefinal,Final],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsRefinal,Final],Resto):-
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocal_catalan(Vocal),
                                          refinalTotal_catalan(Final,[Cons_f,ConsRefinal,Final]),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f,ConsRefinal,Final],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsRefinal,Final]),
                                          !.

%blast

posibilidades_catalan(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsRefinal],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsRefinal],Resto):-
                                          consPareja_catalan(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocal_catalan(Vocal),
                                          refinalTotal_catalan(ConsRefinal,[Cons_f,ConsRefinal]),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f,ConsRefinal],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsRefinal]),
                                          !.
                                          

% trans-pa



posibilidades_catalan(Cons,[Cons,Cons_tr,Vocal,Cons_f,ConsFinalTotal],[Cons,Cons_tr,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,VocalBis|W],Resto):-
                                          vocal_catalan(Vocal),
                                          consPareja_catalan(Cons,[Cons,Cons_tr]),
                                          consFinalTotal_catalan(ConsFinalTotal,[Cons_f,ConsFinalTotal]),
                                                        cons_catalan(ConsSiguiente),
                                          vocal_catalan(VocalBis),
                                          append([Cons,Cons_tr,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Cons_tr,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,VocalBis|W]),
                                          !.
                                          
% posibilidades para CONS+VOCAL+VOCAL AISLADO: HIATO

posibilidades_catalan(Cons,[Cons,Vocal],[Cons,Vocal,Vocal2],Resto):-
                                          cons_catalan(Cons),
                                          hiato_catalan(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Vocal2]),
                                          !.


% posibilidaddes para CONS+VOCAL EN PALABRA: HIATO

posibilidades_catalan(Cons,[Cons,Vocal],[Cons,Vocal,Vocal2|W],Resto):-
                                          cons_catalan(Cons),
                                          hiato_catalan(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Vocal2|W]),
                                          !.

%l'ortografia

posibilidades_catalan(Cons,[Cons,'\''],[Cons,'\'',Vocal|W],Resto):-
                                          cons_catalan(Cons),
                                          (vocal_catalan(Vocal); Vocal='h'),
                                          append([Cons,'\''],Resto,[Cons,'\'',Vocal|W]),
                                          !.


posibilidades_catalan(Cons,[Cons,'’'],[Cons,'’',Vocal|W],Resto):-
                                          cons_catalan(Cons),
                                          (vocal_catalan(Vocal); Vocal='h'),
                                          append([Cons,'’'],Resto,[Cons,'’',Vocal|W]),
                                          !.
                                          
posibilidades_catalan(Cons,[Cons,'´'],[Cons,'´',Vocal|W],Resto):-
                                          cons_catalan(Cons),
                                          (vocal_catalan(Vocal); Vocal='h'),
                                          append([Cons,'´'],Resto,[Cons,'´',Vocal|W]),
                                          !.




posibilidades_catalan(Cons,[Cons,Vocal,'\'',ConsBis],[Cons,Vocal,'\'',ConsBis],Resto):-
                                          cons_catalan(Cons),
                                          vocal_catalan(Vocal),
                                          cons_catalan(ConsBis),
                                          append([Cons,Vocal,'\'',ConsBis],Resto,[Cons,Vocal,'\'',ConsBis]),
                                          !.

