

italiano(X,Restos,Origen):-
                           italiano(X,Restos),
                           origen_etimologicoItaliano(Restos,Origen).

origen_etimologicoItaliano([],[]).

origen_etimologicoItaliano([Restos|MasRestos],[Origen|MasOrigen]):-
                                                 atom_chars(Restos,Deletreo),
                                                 combinacionesItaliano(Restos,Deletreo,Origen),
                                                 !,
                                                 origen_etimologicoItaliano(MasRestos,MasOrigen).

combinacionesItaliano(Restos,[b,l|T],griego(Restos)).

combinacionesItaliano(Restos,[J],desconocido(Restos)).

combinacionesItaliano(Restos,[X,Y|W],Origen):-
                                       combinacionesItaliano(Restos,[Y|W],Origen).

italiano(X,Restos):-
               transcribir(X,J),
               !,
               lista(J,Y),
               comprobar_palabrasItaliano(Y,N),
               recuperar_descartesItaliano(N,Restos).


recuperar_descartesItaliano([],[]).

recuperar_descartesItaliano([N|M],[Restos|MasRestos]):-
               atom_codes(Restos,N),
               recuperar_descartesItaliano(M,MasRestos).




% Transcribir convierte las mayusculas en minusculas

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




%corte(Inicial, Resfinal, Resprov, Sublista)

lista(X, Y):- corte(X, Y, _, []), !.


%%%%METODO AUXILIAR%%%%

%Reglas base
corte([], Resprov, Resfinal, []):-append(Resprov, [], Resfinal).
corte([], Resfinal, Resprov, Sublista):-Sublista\=[], append(Resprov, [Sublista], Resfinal).

%listas vacias cuando aparece dos veces consecutivas 1000 cambiar las dos reglas anteriores por corte([], Resfinal, Resprov, Sublista):-append(Resprov, [Sublista], Resfinal).


% Esta regla sirve para evitar meter listas vacias si te encuentras dos veces seguidas 1000

corte([SignPunt|Resto], Resfinal, Resprov, Sublista):-
                                                    signPunt(SignPunt),
                                                    Sublista=[],
                                                    corte(Resto, Resfinal, Resprov, []).

% Reglas recursivas
corte([SignPunt|Resto], Resfinal, Resprov, Sublista):-
                                                       signPunt(SignPunt),
                                                       append(Resprov, [Sublista], Res),
                                                       corte(Resto, Resfinal, Res, []).

corte([Cab|Resto], Resfinal, Resprov, Sublista):-
                                                   not(signPunt(Cab)),
                                                   append(Sublista, [Cab], Prov),
                                                   corte(Resto, Resfinal, Resprov, Prov).

% Signos de puntuacion considerados

signPunt(32).
signPunt(33).
signPunt(34).
signPunt(38).
signPunt(40).
signPunt(41).
signPunt(44).
signPunt(45).
signPunt(46).
signPunt(47).
signPunt(58).
signPunt(59).
signPunt(63).
signPunt(132).
signPunt(147).
signPunt(148).
signPunt(161).
signPunt(191).
signPunt(171).
signPunt(187).
signPunt(8220).
signPunt(8221).
signPunt(91).
signPunt(93).
signPunt(92).
signPunt(8212).
signPunt(60).
signPunt(62).

signPunt(X):-
             X>47,
             X<58.

comprobar_palabrasItaliano([],[]).


comprobar_palabrasItaliano([Z|W],Sobras):-
                       silabearItaliano(Z,F),
                       comprobar_palabrasItaliano(W,Sobras),
                       !.

comprobar_palabrasItaliano([Z|W],[Z|Sobras]):-
                       not(silabearItaliano(Z,F)),
                       comprobar_palabrasItaliano(W,Sobras),
                       !.













% El proceso de silabeo se incia con la transformacion de lista de codigo ANSII
% a letras sueltas para poder silabear, y lo manda ya en forma de letras al
% proceso auxiliar de silabeo


% El proceso de silabeo se incia con la transformacion de lista de codigo ANSII a letras sueltas para poder silabear, y lo manda ya en forma de letras al proceso auxiliar de silabeo


silabearItaliano(X,Z):-
               atom_codes(Y,X),
               atom_chars(Y,W),
               olfatear_italiano(W),
               olfatear_italianoBis(W),
               silabear_auxItaliano(W,Z).

silabeoItaliano(X,Z):-
               silabear_auxItaliano(X,Z).

olfatear_italiano([]).

olfatear_italiano([Cons,b,l|T]):-
                                 Cons\='b',
                                 !,
                                 fail.

olfatear_italiano([H|T]):-
                                                olfatear_italiano(T).

olfatear_italianoBis(W):-
                                                contar_letras(W,A),
                                                A>3,
                                                reverse(W,[Cons|T]),
                                                not(vocalItaliano(Cons)),
                                                !,
                                                fail.
                                                
olfatear_italianoBis(W):-
                                                contar_letras(W,A),
                                                A=<3,
                                                reverse(W,[Cons|T]),
                                                (Cons=l;Cons=r;Cons=n;Cons=d),
                                                !,
                                                true.
                                                
olfatear_italianoBis(W):-
                                                reverse(W,[Cons|T]),
                                                vocalItaliano(Cons),
                                                !,
                                                true.
                                                
contar_letras([],0).

 
contar_letras([H|T],A):-
                    contar_letras(T,ABis),
                    A is ABis+1.







% El proceso auxiliar de silabeo

silabear_auxItaliano([],[]).




silabear_auxItaliano([Z|T],[Silaba|Silaba_resto]):-
                posibilidadesItaliano(Z,Silabeo,[Z|T],Resto),
                 atom_chars(Silaba,Silabeo),
                 silabear_auxItaliano(Resto,Silaba_resto).





/*
silabear(X,Z):-
               atom_chars(X,W),
               silabear_aux(W,Z).

% El proceso auxiliar de silabeo

silabear_aux([],[]).

silabear_aux([Z|T],[Silaba|Silaba_resto]):-
                 posibilidadesItaliano(Z,Silabeo,[Z|T],Resto),
                 atom_chars(Silaba,Silabeo),
                 silabear_aux(Resto,Silaba_resto).

*/











% Letras en español




vocal_Italiano(i).
vocal_Italiano(u).
vocal_Italiano(a).
vocal_Italiano(o).
vocal_Italiano(e).

vocalItaliano(X):-
               vocal_Italiano(X),
               !.

vocalItaliano(X):-
               letras_equivalentesItaliano(Z,X),
               vocal_Italiano(Z),
               !.




bilabial(p).
bilabial(b).

liquida(r).
liquida(l).



nasal(n).
nasal(m).

oclusiva_sorda(p).
oclusiva_sorda(t).
oclusiva_sorda(q).
oclusiva_sorda(c).

oclusiva_sonora(b).
oclusiva_sonora(d).
oclusiva_sonora(g).


equivalenteItaliano(e,e).
equivalenteItaliano(e,è).
equivalenteItaliano(e,é).
equivalenteItaliano(a,à).
equivalenteItaliano(a,a).
equivalenteItaliano(i,ì).
equivalenteItaliano(i,i).
equivalenteItaliano(o,ò).
equivalenteItaliano(o,o).
equivalenteItaliano(u,u).
equivalenteItaliano(u,ù).


letras_equivalentesItaliano(A,B):-
                          equivalenteItaliano(A,B).

letras_equivalentesItaliano(A,B):-
                          equivalenteItaliano(B,A).


letras_equivalentesItaliano(X,Y):-
                          equivalenteItaliano(X,Z),
                          equivalenteItaliano(Z,Y).

letras_equivalentesItaliano(X,Y):-
                          equivalenteItaliano(X,Z),
                          equivalenteItaliano(Y,Z).

letras_equivalentesItaliano(X,Y):-
                          equivalenteItaliano(Z,X),
                          equivalenteItaliano(Z,Y).


















%% Consontantes emparejadas

% b

consParejaItaliano(b,[b,Liquida|_]):-
                              liquida(Liquida).
% f

consParejaItaliano(f,[f,Liquida|_]):-
                              liquida(Liquida).

% g

consParejaItaliano(g,[g,Liquida|_]):-
                              liquida(Liquida).

% c

consParejaItaliano(c,[c,Liquida|_]):-
                              liquida(Liquida).

% g

consParejaItaliano(p,[p,Liquida|_]):-
                              liquida(Liquida).

% d

consParejaItaliano(d,[d,r|_]).

% t

consParejaItaliano(t,[t,r|_]).

% qu

consParejaItaliano(q,[q,u|_]).

% gu

% consParejaItaliano(g,[g,u|_]).



% s liquida

consParejaItaliano(s,[s,OclusivaSorda|_]):-
oclusiva_sorda(OclusivaSorda).

% s liquida

% consParejaItaliano(s,[s,s|_]).

% gu

consParejaItaliano(g,[g,u|_]).

% ch

consParejaItaliano(c,[c,h|_]).

% gh

consParejaItaliano(g,[g,h|_]).

consTrioItaliano(s,[s,Cons1,Cons2]):-
                             oclusiva_sorda(Cons1),
                             consParejaItaliano(Cons1,[Cons1,Cons2]).

% consTrioItaliano(c,[c,h,r]).



%% Vocales emparejadas: DIPTONGOS

diptongoItaliano(Vocal_a,[Vocal_a,Vocal_u|_]):-
                                            letras_equivalentesItaliano(a,Vocal_a),
                                            letras_equivalentesItaliano(u,Vocal_u).

diptongoItaliano(Vocal_u,[Vocal_u,Vocal_i|_]):-
                                             letras_equivalentesItaliano(i,Vocal_i),
                                             letras_equivalentesItaliano(u,Vocal_u).
hiatoItaliano(Vocal_a,[Vocal_a,Vocal_i|_]):-
                                            letras_equivalentesItaliano(a,Vocal_a),
                                            letras_equivalentesItaliano(i,Vocal_i).
                                            
hiatoItaliano(Vocal_a,[Vocal_a,Vocal_e|_]):-
                                            letras_equivalentesItaliano(a,Vocal_a),
                                            letras_equivalentesItaliano(e,Vocal_e).

hiatoItaliano(Vocal_e,[Vocal_e,Vocal_i|_]):-
                                            letras_equivalentesItaliano(e,Vocal_e),
                                             letras_equivalentesItaliano(i,Vocal_i).

hiatoItaliano(Vocal_e,[Vocal_e,Vocal_u|_]):-
                                            letras_equivalentesItaliano(e,Vocal_e),
                                            letras_equivalentesItaliano(u,Vocal_u).

hiatoItaliano(Vocal_e,[Vocal_e,Vocal_u|_]):-
                                            letras_equivalentesItaliano(e,Vocal_e),
                                            letras_equivalentesItaliano(u,Vocal_u).

hiatoItaliano(Vocal_i,[Vocal_i,Vocal_a|_]):-
                                            letras_equivalentesItaliano(a,Vocal_a),
                                             letras_equivalentesItaliano(i,Vocal_i).


hiatoItaliano(Vocal_i,[Vocal_i,Vocal_e|_]):-
                                            letras_equivalentesItaliano(e,Vocal_e),
                                             letras_equivalentesItaliano(i,Vocal_i).

hiatoItaliano(Vocal_i1,[Vocal_i1,Vocal_i2|_]):-
                                             letras_equivalentesItaliano(i,Vocal_i1),
                                              letras_equivalentesItaliano(i,Vocal_i2).
hiatoItaliano(Vocal_i,[Vocal_i,Vocal_o|_]):-
                                             letras_equivalentesItaliano(i,Vocal_i),
                                             letras_equivalentesItaliano(o,Vocal_o).
hiatoItaliano(Vocal_i,[Vocal_i,Vocal_u|_]):-
                                             letras_equivalentesItaliano(i,Vocal_i),
                                             letras_equivalentesItaliano(u,Vocal_u).

hiatoItaliano(Vocal_o,[Vocal_o,Vocal_i|_]):-
                                         letras_equivalentesItaliano(i,Vocal_i),
                                         letras_equivalentesItaliano(o,Vocal_o).
                                         
hiatoItaliano(Vocal_o,[Vocal_o,Vocal_e|_]):-
                                         letras_equivalentesItaliano(e,Vocal_e),
                                         letras_equivalentesItaliano(o,Vocal_o).

hiatoItaliano(Vocal_u,[Vocal_u,Vocal_a|_]):-
                                            letras_equivalentesItaliano(a,Vocal_a),
                                            letras_equivalentesItaliano(u,Vocal_u).

hiatoItaliano(Vocal_u,[Vocal_u,Vocal_e|_]):-
                                            letras_equivalentesItaliano(e,Vocal_e),
                                            letras_equivalentesItaliano(u,Vocal_u).

hiatoItaliano(Vocal_u,[Vocal_u,Vocal_o|_]):-
                                            letras_equivalentesItaliano(o,Vocal_o),
                                            letras_equivalentesItaliano(u,Vocal_u).
hiatoItaliano(Vocal_u,[Vocal_u,Vocal_u2|_]):-
                                            letras_equivalentesItaliano(u,Vocal_u),
                                            letras_equivalentesItaliano(u,Vocal_u2).


hiatoItaliano(Vocal_e,[Vocal_e,Vocal_a|_]):-
                                         letras_equivalentesItaliano(a,Vocal_a),
                                         letras_equivalentesItaliano(e,Vocal_e).


hiatoItaliano(Vocal_e,[Vocal_e,Vocal_e2|_]):-
                                          letras_equivalentesItaliano(e,Vocal_e),
                                          letras_equivalentesItaliano(e,Vocal_e2).
hiatoItaliano(Vocal_e,[Vocal_e,Vocal_o|_]):-
                                         letras_equivalentesItaliano(e,Vocal_e),
                                         letras_equivalentesItaliano(o,Vocal_o).


hiatoItaliano(Vocal_o,[Vocal_o,Vocal_a|_]):-
                                         letras_equivalentesItaliano(a,Vocal_a),
                                         letras_equivalentesItaliano(o,Vocal_o).



triptongoItaliano(Vocal_i,[Vocal_i,Vocal_e,Vocal_i2]):-
                                                    letras_equivalentesItaliano(i,Vocal_i),
                                                    letras_equivalentesItaliano(e,Vocal_e),
                                                    letras_equivalentesItaliano(i,Vocal_i2).
                                                    
triptongoItaliano(Vocal_i,[Vocal_i,Vocal_o,Vocal_e]):-
                                                    letras_equivalentesItaliano(i,Vocal_i),
                                                    letras_equivalentesItaliano(e,Vocal_e),
                                                    letras_equivalentesItaliano(o,Vocal_o).



cons_fItaliano(n).
cons_fItaliano(r).
cons_fItaliano(l).
cons_fItaliano(s).

%% Consontantes pseudofinales: contextos

% Letra m

pseudocons_fItaliano(m,[m,Bilabial|_]):-
                                  bilabial(Bilabial).



% Letra g


pseudocons_fItaliano(g,[g,n|_]).

pseudocons_fItaliano(g,[g,g|_]).


% Letra p



pseudocons_fItaliano(p,[p,Dentales|_]):-
                                  % Dentales=t;
                                  Dentales=p.






% Letra c

pseudocons_fItaliano(c,[c,c|_]).

% Letra b

pseudocons_fItaliano(b,[b,b|_]).

% Letra t

pseudocons_fItaliano(t,[t,t|_]).

% Letra f

pseudocons_fItaliano(f,[f,f|_]).

% Letra v

pseudocons_fItaliano(v,[v,v|_]).

% Letra z

pseudocons_fItaliano(z,[z,z|_]).

% Letra m

pseudocons_fItaliano(m,[m,m|_]).

% Letra d

pseudocons_fItaliano(d,[d,d|_]).




consItaliano(b).
consItaliano(c).
consItaliano(d).
consItaliano(f).
consItaliano(g).
consItaliano(l).
consItaliano(m).
consItaliano(n).
consItaliano(p).
consItaliano(r).
consItaliano(s).
consItaliano(t).
consItaliano(v).
consItaliano(x).
consItaliano(i).
consItaliano(z).


% Silabas en español

% posibilidades/4: letra que encabeza la silaba/silaba/letras que siguen/resto de la palabra (sin la silaba que acabamos de aislar, pero si con el contexto.

%% PREFIJOS

posibilidadesItaliano(s,[s,u,b],[s,u,b,Cons|W],Resto):-
                                                                consItaliano(Cons),
                                                                Cons\=i,
                                                                append([s,u,b],Resto,[s,u,b,Cons|W]),
                                                              !.

posibilidadesItaliano(s,[s,u,b],[s,u,b],Resto):-
                                                                append([s,u,b],Resto,[s,u,b]),
                                                                 !.



/*
posibilidadesItaliano(o,[o,b,s],[o,b,s,Cons|W],Resto):-
                                                                consItaliano(Cons),
                                                                Cons\=i,
                                                                append([o,b,s],Resto,[o,b,s,Cons|W]),
                                                                 !.
                                                                 
*/



posibilidadesItaliano(o,[o,b],[o,b],Resto):-
                                                                append([o,b],Resto,[o,b]),
                                                                 !.


posibilidadesItaliano(o,[o,b],[o,b,Cons|W],Resto):-
                                                                consItaliano(Cons),
                                                                Cons\=i,
                                                                not(consParejaItaliano(b,[b,Cons])),
                                                                append([o,b],Resto,[o,b,Cons|W]),
                                                                 !.



posibilidadesItaliano(a,[a,b,s],[a,b,s,Cons|W],Resto):-
                                                                consItaliano(Cons),
                                                                append([a,b,s],Resto,[a,b,s,Cons|W]),
                                                                !.




posibilidadesItaliano(a,[a,b],[a,b],Resto):-
                                                                append([a,b],Resto,[a,b]),
                                                                 !.









posibilidadesItaliano(a,[a,b],[a,b,Cons|W],Resto):-
                                                               consItaliano(Cons),
                                                               Cons\=i,
                                                                not(consParejaItaliano(b,[b,Cons])),
                                                                 append([a,b],Resto,[a,b,Cons|W]),
                                                                !.


%ex

posibilidadesItaliano(e,[e,x],[e,x,Cons|W],Resto):-
                                                                consItaliano(Cons),
                                                                Cons\=i,
                                                                append([e,x],Resto,[e,x,Cons|W]),
                                                                 !.





/*
posibilidadesItaliano(a,[a,b,s],[a,b,s,Cons|W],Resto):-
                                                                 consItaliano(Cons),
                                                                 % not(consParejaItaliano(s,[s,Cons])),
                                                                 append([a,b,s],Resto,[a,b,s,Cons|W]),
                                                                 !.

posibilidadesItaliano(o,[o,b,s],[o,b,s,Cons|W],Resto):-
                                                                 consItaliano(Cons),
                                                                 % not(consParejaItaliano(s,[s,Cons])),
                                                                 append([o,b,s],Resto,[o,b,s,Cons|W]),
                                                                 !.


posibilidadesItaliano(i,[i,n,s],[i,n,s,Cons|W],Resto):-
                                                                 consItaliano(Cons),
                                                                 % not(consParejaItaliano(s,[s,Cons])),
                                                                 append([i,n,s],Resto,[i,n,s,Cons|W]),
                                                                 !.

posibilidadesItaliano(t,[t,r,a,n,s],[t,r,a,n,s,Cons|W],Resto):-
                                                                 consItaliano(Cons),
                                                                 % not(consParejaItaliano(s,[s,Cons])),
                                                                 append([t,r,a,n,s],Resto,[t,r,a,n,s,Cons|W]),
                                                                 !.

*/


/*
% post-quam



posibilidadesItaliano(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal],[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,Cons_tr|W],Resto):-
                                          consItaliano(Cons),
                                          vocalItaliano(Vocal),
                                          consParejaItaliano(ConsSiguiente,[ConsSiguiente,Cons_tr]),
                                          not(consTrioItaliano(ConsFinalTotal,[ConsFinalTotal,ConsSiguiente,Cons_tr])),
                                          consFinalTotalItaliano(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                          append([Cons,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,Cons_tr|W]),
                                          !.
                                          
*/



posibilidadesItaliano(h,[h,o],[h,o],[]).

posibilidadesItaliano(h,[h,a,i],[h,a,i],[]).

posibilidadesItaliano(h,[h,a],[h,a],[]).

posibilidadesItaliano(h,[h,a,n],[h,a,n,n,o],Resto):-
                                                    append([h,a,n],Resto,[h,a,n,n,o]),
                                                    !.



% a

posibilidadesItaliano(Vocal,[Vocal],[Vocal],Resto):-
                                                               vocalItaliano(Vocal),
                                                                append([Vocal],Resto,[Vocal]),
                                                                !.


% Au-un

posibilidadesItaliano(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,Vocal3|W],Resto):-
                                                                diptongoItaliano(Vocal,[Vocal,Vocal2]),
                                                                hiatoItaliano(Vocal2,[Vocal2,Vocal3]),
                                                                append([Vocal,Vocal2],Resto,[Vocal,Vocal2,Vocal3|W]),
                                                                !.




% brai-le

posibilidadesItaliano(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2,ConsBis,VocalBis|W],Resto):-
                                          consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          consItaliano(ConsBis),
                                          vocalItaliano(VocalBis),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,ConsBis,VocalBis|W]),
                                          !.
                                          
/*

% post-quam



posibilidadesItaliano(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal],[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,Cons_tr|W],Resto):-
                                          consItaliano(Cons),
                                          vocalItaliano(Vocal),
                                          consParejaItaliano(ConsSiguiente,[ConsSiguiente,Cons_tr]),
                                          not(consTrioItaliano(ConsFinalTotal,[ConsFinalTotal,ConsSiguiente,Cons_tr])),
                                          consFinalTotalItaliano(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                          append([Cons,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente,Cons_tr|W]),
                                          !.
                                          
*/

% antro, an-cla


% antro, an-cla

posibilidadesItaliano(Vocal,[Vocal,Cons_f],[Vocal,Cons_f,ConsPareja,Cons_tr,Vocal2|W],Resto):-
                                                                 vocalItaliano(Vocal),
                                                                (cons_fItaliano(Cons_f);pseudocons_fItaliano(Cons_f,[Cons_f,ConsPareja|_])),
                                                                (not(consParejaItaliano(Cons_f,[Cons_f,ConsPareja]));not(consTrioItaliano(Cons_f,[Cons_f,ConsPareja,Cons_tr]))),
                                                                ((consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr]),vocalItaliano(Vocal2));consTrioItaliano(ConsPareja,[ConsPareja,Cons_tr,Vocal2])),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f,ConsPareja,Cons_tr,Vocal2|W]),
                                                                !.




% posibilidades para CONS+VOCAL AISLADO

posibilidadesItaliano(Cons,[Cons,Vocal],[Cons,Vocal],Resto):-
                                          consItaliano(Cons),
                                          vocalItaliano(Vocal),
                                          append([Cons,Vocal],Resto,[Cons,Vocal]),
                                          !.

% posibilidaddes para CONS+VOCAL EN PALABRA


posibilidadesItaliano(Cons,[Cons,Vocal],[Cons,Vocal,Cons1,Vocal1|W],Resto):-
                                          consItaliano(Cons),
                                          vocalItaliano(Vocal),
                                          consItaliano(Cons1),
                                          vocalItaliano(Vocal1),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Cons1,Vocal1|W]),
                                          !.

% CONS+VOCAL+CONS_F: pen-sar. OJO: para el italiano es importante que esta regla
% aparezca aqui. Res-su-co; al ir la s tb como vocal, si cambiamos el orden, lo
% interpreta como re-s-su-co.


posibilidadesItaliano(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f,Cons1,Vocal1|W],Resto):-
                                          consItaliano(Cons),
                                          vocalItaliano(Vocal),
                                          (cons_fItaliano(Cons_f);pseudocons_fItaliano(Cons_f,[Cons_f,Cons1|_])),
                                          consItaliano(Cons1),
                                          vocalItaliano(Vocal1),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f,Cons1,Vocal1|W]),
                                          !.


% posibilidades para CONS+VOCAL+VOCAL AISLADO:

posibilidadesItaliano(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2],Resto):-
                                          consItaliano(Cons),
                                          diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2]),
                                          !.



% posibilidaddes para CONS+VOCAL+VOCAL EN PALABRA:

posibilidadesItaliano(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2,Cons1,Vocal1|W],Resto):-
                                          consItaliano(Cons),
                                          diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          consItaliano(Cons1),
                                          vocalItaliano(Vocal1),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2,Cons1,Vocal1|W]),
                                          !.



% posibilidaddes para CONS+VOCAL EN PALABRA: HIATO

posibilidadesItaliano(Cons,[Cons,Vocal],[Cons,Vocal,Vocal2|W],Resto):-
                                          consItaliano(Cons),
                                          hiatoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Vocal2|W]),
                                          !.



% nai-blar; nai-trar

posibilidadesItaliano(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2,ConsPareja,Cons_tr|W],Resto):-
                                          consItaliano(Cons),
                                          diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2,ConsPareja,Cons_tr|W]),
                                          !.



% CONS+VOCAL+VOCAL+CONS_F: DIPT DESCENDENTE mais-car



posibilidadesItaliano(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f,Cons1,Vocal1|W],Resto):-
                                          consItaliano(Cons),
                                          diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          (cons_fItaliano(Cons_f);pseudocons_fItaliano(Cons_f,[Cons_f,Cons1|_])),
                                          consItaliano(Cons1),
                                          vocalItaliano(Vocal1),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f,Cons1,Vocal1|W]),
                                          !.

% CONS+VOCAL+VOCAL+CONS_F: DIPT DESCENDENTE mais-bla



posibilidadesItaliano(Cons,[Cons,Vocal,Vocal2,Cons_f],[Cons,Vocal,Vocal2,Cons_f,ConsTipo,Cons_tr|W],Resto):-
                                          consItaliano(Cons),
                                          diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          (cons_fItaliano(Cons_f);pseudocons_fItaliano(Cons_f,[Cons_f,ConsTipo|_])),
                                          consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          append([Cons,Vocal,Vocal2,Cons_f],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsPareja,Cons_tr|W]),
                                          !.


% CONS+VOCAL+VOCAL+CONS_F*CONSFINALTOTAL: DIPT DESCENDENTE mains-bla


/*
posibilidadesItaliano(Cons,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                          consItaliano(Cons),
                                          diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          consFinalTotalItaliano(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                          append([Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal,ConsSiguiente|_]),
                                          !.
                                          
*/

/*
%panc-to

posibilidadesItaliano(Cons,[Cons,Vocal,Cons_f,ConsFinalTotal],[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                                                  consItaliano(Cons),
                                                                 vocalItaliano(Vocal),
                                                                 consFinalTotalItaliano(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                                                append([Cons,Vocal,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W]),
                                                                !.
                                                                
*/


/*
%neins

posibilidadesItaliano(Cons,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],Resto):-
                                          consItaliano(Cons),
                                          diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          consFinalTotalItaliano(ConsFinalTotal,[Cons_f,ConsFinalTotal]),
                                          append([Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal],Resto,[Cons,Vocal,Vocal2,Cons_f,ConsFinalTotal]),
                                          !.
                                          
*/



% CONS+VOCAL+CONS_F: pen-trar


posibilidadesItaliano(Cons,[Cons,Vocal,Cons_f],[Cons,Vocal,Cons_f,ConsPareja,Cons_tr|W],Resto):-
                                          consItaliano(Cons),
                                          vocalItaliano(Vocal),
                                          (cons_fItaliano(Cons_f);pseudocons_fItaliano(Cons_f,[Cons_f,ConsPareja|W])),
                                          consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr]),
                                          append([Cons,Vocal,Cons_f],Resto,[Cons,Vocal,Cons_f,ConsPareja,Cons_tr|W]),
                                          !.





% nu-blar  pa-dre

posibilidadesItaliano(Cons,[Cons,Vocal],[Cons,Vocal,ConsPareja,Cons_tr|W],Resto):-
                                          consItaliano(Cons),
                                          consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr]),
                                          vocalItaliano(Vocal),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,ConsPareja,Cons_tr|W]),
                                          !.







%%% silabas que empiezan por vocal

%% OJO PARA TODO EL CÓDIGO QUE SIGUE: OI, AU y AI son los unicos dipt que forman dipt tal cual. Los desmas son extranj: comprobar

% a


posibilidadesItaliano(Vocal,[Vocal],[Vocal],Resto):-
                                                                vocalItaliano(Vocal),
                                                                append([Vocal],Resto,[Vocal]),
                                                                !.
% a-la

posibilidadesItaliano(Vocal,[Vocal],[Vocal,Cons,Vocal1|W],Resto):-
                                                                vocalItaliano(Vocal),
                                                                consItaliano(Cons),
                                                                vocalItaliano(Vocal1),
                                                                append([Vocal],Resto,[Vocal,Cons,Vocal1|W]),
                                                                !.
                                                                
/*
%anc-to

posibilidadesItaliano(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                                                 vocalItaliano(Vocal),
                                                                 consFinalTotalItaliano(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                                                append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W]),
                                                                !.
                                                                
*/








% a-pro-xi-mar-se; a-dri

posibilidadesItaliano(Vocal,[Vocal],[Vocal,ConsPareja,Cons_tr,Vocal2|W],Resto):-
                                                                 vocalItaliano(Vocal),
                                                                 consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr|_]),
                                                               append([Vocal],Resto,[Vocal,ConsPareja,Cons_tr,Vocal2|W]),
                                                                !.

% ai-rar, au-lar, eusebio

posibilidadesItaliano(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,Cons,Vocal1|W],Resto):-
                                                                        diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                                                        consItaliano(Cons),
                                                                        vocalItaliano(Vocal1),
                                                                        append([Vocal,Vocal2],Resto,[Vocal,Vocal2,Cons,Vocal1|W]),
                                                                         !.


% ai-trar

posibilidadesItaliano(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,ConsPareja,Cons_tr|W],Resto):-
                                                                        diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                                                        consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr]),
                                                                        append([Vocal,Vocal2],Resto,[Vocal,Vocal2,ConsPareja,Cons_tr|W]),
                                                                         !.



% AIS-lar

posibilidadesItaliano(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f,Cons|W],Resto):-
                                                                diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                                                (cons_fItaliano(Cons_f);pseudocons_fItaliano(Cons_f,[Cons_f,Cons|_])),
                                                                consItaliano(Cons),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f,Cons|W]),
                                                                !.

% AIS-que

posibilidadesItaliano(Vocal,[Vocal,Vocal2,Cons_f],[Vocal,Vocal2,Cons_f,Cons,Constr|W],Resto):-
                                                                diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                                                (cons_fItaliano(Cons_f);pseudocons_fItaliano(Cons_f,[Cons_f,Cons|_])),
                                                                consParejaItaliano(Cons,[Cons,Constr]),
                                                                append([Vocal,Vocal2,Cons_f],Resto,[Vocal,Vocal2,Cons_f,Cons,Constr|W]),
                                                                !.


% Au-un

posibilidadesItaliano(Vocal,[Vocal,Vocal2],[Vocal,Vocal2,Vocal3|W],Resto):-
                                                                diptongoItaliano(Vocal,[Vocal,Vocal2]),
                                                                hiatoItaliano(Vocal2,[Vocal2,Vocal3]),
                                                                append([Vocal,Vocal2],Resto,[Vocal,Vocal2,Vocal3|W]),
                                                                !.






% a-e-re-o

posibilidadesItaliano(Vocal,[Vocal],[Vocal,Vocal2|W],Resto):-
                                                                 hiatoItaliano(Vocal,[Vocal,Vocal2|_]),
                                                                 append([Vocal],Resto,[Vocal,Vocal2|W]),
                                                                 !.




% is-la

posibilidadesItaliano(Vocal,[Vocal,Cons_f],[Vocal,Cons_f,ConsBis,VocalBis|W],Resto):-
                                                                 vocalItaliano(Vocal),
                                                                (cons_fItaliano(Cons_f);pseudocons_fItaliano(Cons_f,[Cons_f,ConsBis|W])),
                                                                consItaliano(ConsBis),
                                                                vocalItaliano(VocalBis),
                                                                append([Vocal,Cons_f],Resto,[Vocal,Cons_f,ConsBis,VocalBis|W]),
                                                                !.


/*

%ens-ta

posibilidadesItaliano(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                                                 vocalItaliano(Vocal),
                                                                  consFinalTotalItaliano(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                                                append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W]),
                                                                !.
                                                                
*/

/*

%ens

posibilidadesItaliano(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal],Resto):-
                                                                 vocalItaliano(Vocal),
                                                                consFinalTotalItaliano(ConsFinalTotal,[Cons_f,ConsFinalTotal]),
                                                                append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal]),
                                                                !.
                                                                
*/


/*


%anc-to

posibilidadesItaliano(Vocal,[Vocal,Cons_f,ConsFinalTotal],[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                                                 vocalItaliano(Vocal),
                                                                cons_fItaliano(Cons_f),
                                                                 consFinalTotalItaliano(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                                                append([Vocal,Cons_f,ConsFinalTotal],Resto,[Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W]),
                                                                !.

*/






% ay, uy: unicas palabras encontradas con dipt que solo son eso: el dipt, pero meto todos los casos pq nada lo prohibe. No meto el dipt ascendente ni el doble pq no parece probable

posibilidadesItaliano(Vocal,[Vocal,Vocal2],[Vocal,Vocal2],Resto):-
                                                                diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                                                append([Vocal,Vocal2],Resto,[Vocal,Vocal2]),
                                                                !.


% ea: hiato aislado. Solo encontrado "ea", pero meto todo porque nada lo impide

posibilidadesItaliano(Vocal,[Vocal],[Vocal,Vocal2],Resto):-
                                                                hiatoItaliano(Vocal,[Vocal,Vocal2|_]),
                                                                append([Vocal],Resto,[Vocal,Vocal2]),
                                                                !.


% posibilidades para CONS+VOCAL+VOCAL AISLADO: HIATO

posibilidadesItaliano(Cons,[Cons,Vocal],[Cons,Vocal,Vocal2],Resto):-
                                          consItaliano(Cons),
                                          hiatoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          append([Cons,Vocal],Resto,[Cons,Vocal,Vocal2]),
                                          !.

% tue-or

posibilidadesItaliano(Cons,[Cons,Vocal,Vocal2],[Cons,Vocal,Vocal2,Vocal3|W],Resto):-
                                          consItaliano(Cons),
                                          diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          hiatoItaliano(Vocal2,[Vocal2,Vocal3|_]),
                                          append([Cons,Vocal,Vocal2],Resto,[Cons,Vocal,Vocal2,Vocal3|W]),
                                          !.

%% Caso particular: trabadas en mitad de palabra






% bla

posibilidadesItaliano(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal],Resto):-
                                          consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalItaliano(Vocal),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal]),
                                          !.



% spra

posibilidadesItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal],[ConsPareja,ConsPareja2,Cons_tr,Vocal],Resto):-
                                                                 vocalItaliano(Vocal),
                                                                 consTrioItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                                               append([ConsPareja,ConsPareja2,Cons_tr,Vocal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal]),
                                                                !.



% gra-pa

posibilidadesItaliano(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,Cons,Vocal1|W],Resto):-
                                          consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalItaliano(Vocal),
                                          vocalItaliano(Vocal1),
                                          consItaliano(Cons),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,Cons,Vocal1|W]),
                                          !.

% stru-to

posibilidadesItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal],[ConsPareja,ConsPareja2,Cons_tr,Vocal,Cons,Vocal1|W],Resto):-
                                                                 vocalItaliano(Vocal),
                                                                 consItaliano(Cons),
                                                                 vocalItaliano(Vocal1),
                                                                 consTrioItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                                               append([ConsPareja,ConsPareja2,Cons_tr,Vocal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Cons,Vocal1|W]),
                                                                !.

% blau?

posibilidadesItaliano(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2],Resto):-
                                          consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2]),
                                          !.
%strae

posibilidadesItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2],[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2],Resto):-
                                          consTrioItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                          diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          append([ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2]),
                                          !.


%strae-le

posibilidadesItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2],[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsBis,VocalBis],Resto):-
                                          consTrioItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                          diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          consItaliano(ConsBis),
                                          vocalItaliano(VocalBis),
                                          append([ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsBis,VocalBis]),
                                          !.


% blau-bla

posibilidadesItaliano(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2],[ConsPareja,Cons_tr,Vocal,Vocal2,ConsParejaBis,Cons_trBis|W],Resto):-
                                          consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          consParejaItaliano(ConsParejaBis,[ConsParejaBis,Cons_trBis|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,ConsParejaBis,Cons_trBis|W]),
                                          !.

%strae-bla

posibilidadesItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2],[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsParejaBis,Cons_trBis],Resto):-
                                          consTrioItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                          diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          consParejaItaliano(ConsParejaBis,[ConsPareja2,Cons_trBis|_]),
                                          append([ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsParejaBis,Cons_trBis]),
                                          !.


%straen

posibilidadesItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsFinal],[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsFinal],Resto):-
                                          consTrioItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                          diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          cons_fItaliano(ConsFinal),
                                          append([ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsFinal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsFinal]),
                                          !.
                                          
/*

% brains

posibilidadesItaliano(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal],Resto):-
                                          consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          consItaliano(Cons),
                                          consFinalTotalItaliano(ConsFinalTotal,[Cons_f,ConsFinalTotal]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,ConsFinalTotal]),
                                          !.
                                          
*/

% brain-le

posibilidadesItaliano(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,Cons|W],Resto):-
                                          consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          consItaliano(Cons),
                                          (cons_fItaliano(Cons_f);pseudocons_fItaliano(Cons_f,[Cons_f,Cons|_])),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Cons_f,Cons|W]),
                                          !.

%straen-le

posibilidadesItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsFinal],[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsFinal,Cons,Vocal],Resto):-
                                          consTrioItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                          diptongoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          cons_fItaliano(ConsFinal),
                                          consItaliano(Cons),
                                          vocalItaliano(Vocal),
                                          append([ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsFinal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2,ConsFinal,Cons,Vocal]),
                                          !.





% pro-e-mio

posibilidadesItaliano(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,Vocal2|W],Resto):-
                                          consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          hiatoItaliano(Vocal,[Vocal,Vocal2|_]),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2|W]),
                                          !.

% stra-o-mio

posibilidadesItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal],[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2|W],Resto):-
                                                                 hiatoItaliano(Vocal,[Vocal,Vocal2|_]),
                                                                 consTrioItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                                               append([ConsPareja,ConsPareja2,Cons_tr,Vocal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,Vocal2|W]),
                                                                !.





% bla-bla

posibilidadesItaliano(ConsPareja,[ConsPareja,Cons_tr,Vocal],[ConsPareja,Cons_tr,Vocal,ConsPareja1,Cons_tr1|W],Resto):-
                                          consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalItaliano(Vocal),
                                          consParejaItaliano(ConsPareja1,[ConsPareja1,Cons_tr1|_]),
                                          append([ConsPareja,Cons_tr,Vocal],Resto,[ConsPareja,Cons_tr,Vocal,ConsPareja1,Cons_tr1|W]),

%stra-bla
                                          !.

posibilidadesItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal],[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsParejaBis,Cons_trBis|W],Resto):-
                                                                 vocalItaliano(Vocal),
                                                                 consTrioItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                                                 consParejaItaliano(ConsParejaBis,[ConsPareja1,Cons_trBis|_]),
                                                               append([ConsPareja,ConsPareja2,Cons_tr,Vocal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsParejaBis,Cons_trBis|W]),
                                                                !.

/*
%blans

posibilidadesItaliano(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],Resto):-
                                          consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalItaliano(Vocal),
                                          consFinalTotalItaliano(ConsFinalTotal,[Cons_f,ConsFinalTotal]),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal]),
                                          !.
                                          
*/

%stras

posibilidadesItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal],[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal],Resto):-
                                                                 vocalItaliano(Vocal),
                                                                 consTrioItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                                                 cons_fItaliano(ConsFinal),
                                                               append([ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal]),
                                                                !.
                                                                
/*

%strans

posibilidadesItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,ConsFinalTotal],[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,ConsFinalTotal],Resto):-
                                                                 vocalItaliano(Vocal),
                                                                 consTrioItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                                                 consFinalTotalItaliano(ConsFinalTotal,[ConsFinal,ConsFinalTotal]),
                                                               append([ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,ConsFinalTotal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,ConsFinalTotal]),
                                                                !.
                                                                
*/





% blan-ca

posibilidadesItaliano(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f,Cons1,Vocal1|W],Resto):-
                                          consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalItaliano(Vocal),
                                          vocalItaliano(Vocal1),
                                          (cons_fItaliano(Cons_f);pseudocons_fItaliano(Cons_f,[Cons_f,Cons1|_])),
                                          consItaliano(Cons1),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,Cons1,Vocal1|W]),
                                          !.

%stras-ta

posibilidadesItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal],[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,Cons,Vocal2|W],Resto):-
                                                                 vocalItaliano(Vocal),
                                                                 vocalItaliano(Vocal2),
                                                                 consTrioItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                                                 (cons_fItaliano(ConsFinal);pseudocons_fItaliano(ConsFinal,[ConsFinal,Cons|_])),
                                                                 consItaliano(Cons),
                                                               append([ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,Cons,Vocal2|W]),
                                                                !.

% blan-bla

posibilidadesItaliano(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsPareja1,Cons_tr1|W],Resto):-
                                          consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalItaliano(Vocal),
                                          (cons_fItaliano(Cons_f);pseudocons_fItaliano(Cons_f,[Cons_f,ConsPareja1|_])),
                                          consParejaItaliano(ConsPareja1,[ConsPareja1,Cons_tr1|_]),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsPareja1,Cons_tr1|W]),
                                          !.

% stram-bla

posibilidadesItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal],[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,ConsPareja1,Cons_tr1|W],Resto):-
                                                                 vocalItaliano(Vocal),
                                                                 consTrioItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                                                 consParejaItaliano(ConsParejaBis,[ConsPareja1,Cons_trBis|_]),
                                                                 cons_fItaliano(ConsFinal),
                                                                 (cons_fItaliano(ConsFinal);pseudocons_fItaliano(ConsFinal,[ConsFinal,ConsPareja1|_])),
                                                                 consParejaItaliano(ConsPareja1,[ConsPareja1,Cons_tr1|_]),
                                                               append([ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,Cons,Vocal2|W]),
                                                                !.


/*
% blans-ca

posibilidadesItaliano(ConsPareja,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W],Resto):-
                                          consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocalItaliano(Vocal),
                                          consFinalTotalItaliano(ConsFinalTotal,[Cons_f,ConsFinalTotal,ConsSiguiente]),
                                          consItaliano(Cons1),
                                          not(consParejaItaliano(ConsFinalTotal,[ConsFinalTotal,ConsSiguiente])),
                                          append([ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal],Resto,[ConsPareja,Cons_tr,Vocal,Cons_f,ConsFinalTotal,ConsSiguiente|W]),
                                          !.
                                          


% strans

posibilidadesItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,ConsFinalTotal],[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,ConsFinalTotal],Resto):-
                                                                 vocalItaliano(Vocal),
                                                                 consTrioItaliano(ConsPareja,[ConsPareja,ConsPareja2,Cons_tr]),
                                                                 consFinalTotalItaliano(ConsFinalTotal,[ConsFinal,ConsFinalTotal,ConsSiguiente]),
                                                                 append([ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal],Resto,[ConsPareja,ConsPareja2,Cons_tr,Vocal,ConsFinal,ConsFinalTotal]),
                                                                !.
*/
                                                                
posibilidadesItaliano(Cons,[Cons,Vocal,n],[Cons,Vocal,n],Resto):-
                                                                 vocalItaliano(Vocal),
                                                                 consItaliano(Cons).
                                                                 
posibilidadesItaliano(Cons,[Cons,Vocal,l],[Cons,Vocal,l],Resto):-
                                                                 vocalItaliano(Vocal),
                                                                 consItaliano(Cons).

posibilidadesItaliano(Cons,[Cons,Vocal,r],[Cons,Vocal,r],Resto):-
                                                                 vocalItaliano(Vocal),
                                                                 consItaliano(Cons).
                                                                 
                                                                 
posibilidadesItaliano(Vocal,[Vocal,n],[Vocal,n],Resto):-
                                                                 vocalItaliano(Vocal).
                                                                 
posibilidadesItaliano(Vocal,[Vocal,l],[Vocal,l],Resto):-
                                                                 vocalItaliano(Vocal).



posibilidadesItaliano(Vocal,[Vocal,d],[Vocal,d],Resto):-
                                                                 vocalItaliano(Vocal).



% Triptongo



posibilidadesItaliano(Cons,[Cons,Vocal,Vocal2,Vocal3],[Cons,Vocal,Vocal2,Vocal3,ConsBis,VocalBis],Resto):-
                                          consItaliano(Cons),
                                          consItaliano(ConsBis),
                                          triptongoItaliano(Vocal,[Vocal,Vocal2,Vocal3]),
                                          vocalItaliano(VocalBis),
                                          append([Cons,Vocal,Vocal2,Vocal3],Resto,[Cons,Vocal,Vocal2,Vocal3,ConsBis,VocalBis]),
                                          !.

posibilidadesItaliano(Cons,[Cons,Vocal,Vocal2,Vocal3],[Cons,Vocal,Vocal2,Vocal3,ConsPareja,Cons_tr|W],Resto):-
                                          consItaliano(Cons),
                                          triptongoItaliano(Vocal,[Vocal,Vocal2,Vocal3]),
                                          consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          append([Cons,Vocal,Vocal2,Vocal3],Resto,[Cons,Vocal,Vocal2,Vocal3,ConsPareja,Cons_tr|W]),
                                          !.

posibilidadesItaliano(Cons,[Cons,Vocal,Vocal2,Vocal3],[Cons,Vocal,Vocal2,Vocal3],Resto):-
                                          consItaliano(Cons),
                                          triptongoItaliano(Vocal,[Vocal,Vocal2,Vocal3]),
                                           append([Cons,Vocal,Vocal2,Vocal3],Resto,[Cons,Vocal,Vocal2,Vocal3]),
                                          !.

% Triptongo en cons_tr

posibilidadesItaliano(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],Resto):-
                                          consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          triptongoItaliano(Vocal,[Vocal,Vocal2,Vocal3]),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3]),
                                          !.





/*

% Cuadriptongos

posibilidadesItaliano(ConsPareja,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Vocal4],[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Vocal4],Resto):-
                                          consParejaItaliano(ConsPareja,[ConsPareja,Cons_tr|_]),
                                          vocal_fuerteItaliano(Vocal),
                                          vocal_fuerteItaliano(Vocal2),
                                          vocal_debilItaliano(Vocal3),
                                          cons_fItaliano(Cons_f),
                                          append([ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f],Resto,[ConsPareja,Cons_tr,Vocal,Vocal2,Vocal3,Cons_f]),
                                          !.


*/

%l'ortografia

posibilidadesItaliano(Cons,[Cons,'\''],[Cons,'\'',Vocal|W],Resto):-
                                          consItaliano(Cons),
                                          (vocalItaliano(Vocal); Vocal='h'),
                                          append([Cons,'\''],Resto,[Cons,'\'',Vocal|W]),
                                          !.


posibilidadesItaliano(Cons,[Cons,'’'],[Cons,'’',Vocal|W],Resto):-
                                          consItaliano(Cons),
                                          (vocalItaliano(Vocal); Vocal='h'),
                                          append([Cons,'’'],Resto,[Cons,'’',Vocal|W]),
                                          !.

posibilidadesItaliano(Cons,[Cons,'´'],[Cons,'´',Vocal|W],Resto):-
                                          consItaliano(Cons),
                                          (vocalItaliano(Vocal); Vocal='h'),
                                          append([Cons,'´'],Resto,[Cons,'´',Vocal|W]),
                                          !.




posibilidadesItaliano(Cons,[Cons,Vocal,'\'',ConsBis],[Cons,Vocal,'\'',ConsBis],Resto):-
                                          consItaliano(Cons),
                                          vocalItaliano(Vocal),
                                          consItaliano(ConsBis),
                                          append([Cons,Vocal,'\'',ConsBis],Resto,[Cons,Vocal,'\'',ConsBis]),
                                          !.


                                          
posibilidadesItaliano(Cons,[Cons,Vocal,Cons1,Cons1,'\''],[Cons,Vocal,Cons1,Cons1,'\'',Vocal2|T],Resto):-
                                          consItaliano(Cons),
                                          vocalItaliano(Vocal),
                                          consItaliano(Cons1),
                                          vocalItaliano(Vocal2),
                                          append([Cons,Vocal,Cons1,Cons1,'\''],Resto,[Cons,Vocal,Cons1,Cons1,'\'',Vocal2|T]),
                                          !.
                                          
posibilidadesItaliano(Vocal,[Vocal,Cons1,Cons1,'\''],[Vocal,Cons1,Cons1,'\'',Vocal2|T],Resto):-
                                         vocalItaliano(Vocal),
                                          consItaliano(Cons1),
                                          vocalItaliano(Vocal2),
                                          append([Vocal,Cons1,Cons1,'\''],Resto,[Vocal,Cons1,Cons1,'\'',Vocal2|T]),
                                          !.
                                          
posibilidadesItaliano(Vocal,[Vocal,Cons1,'\''],[Vocal,Cons1,'\'',Vocal2|T],Resto):-
                                         vocalItaliano(Vocal),
                                          consItaliano(Cons1),
                                          vocalItaliano(Vocal2),
                                          append([Vocal,Cons1,'\''],Resto,[Vocal,Cons1,'\'',Vocal2|T]),
                                          !.



