% Autor:
% Datum: 07/10/2011

% Author:
% Date: 09/05/2011


discriminador(X,'español'):-
               transcribir(X,W),
               lista(W,Y),
               contar(NumeroTotalPalabras,Y),
               procesar_latin(X,RatioAcertadas,NumeroTotalPalabras,Desconocidos1),
               procesar_español(X,RatioAcertadas2,NumeroTotalPalabras,Desconocidos2),
               procesar_catalan(X,RatioAcertadas3,NumeroTotalPalabras,Desconocidos3),
               procesar_portugues(X,RatioAcertadas4,NumeroTotalPalabras,Desconocidos4),
               procesar_frances(X,RatioAcertadas5,NumeroTotalPalabras,Desconocidos5),
               procesar_italiano(X,RatioAcertadas6,NumeroTotalPalabras,Desconocidos6),
               procesar_euskera(X,RatioAcertadas7,NumeroTotalPalabras,Desconocidos7),
               RatioAcertadas2>RatioAcertadas,
               RatioAcertadas2>RatioAcertadas3,
               RatioAcertadas2>RatioAcertadas4,
               RatioAcertadas2>RatioAcertadas5,
               RatioAcertadas2>RatioAcertadas6,
               RatioAcertadas2>RatioAcertadas7
               !.





discriminador(X,'latín'):-
               transcribir(X,W),
               lista(W,Y),
               contar(NumeroTotalPalabras,Y),
               procesar_latin(X,RatioAcertadas,NumeroTotalPalabras,Desconocidos1),
               procesar_español(X,RatioAcertadas2,NumeroTotalPalabras,Desconocidos2),
                procesar_catalan(X,RatioAcertadas3,NumeroTotalPalabras,Desconocidos3),
                procesar_portugues(X,RatioAcertadas4,NumeroTotalPalabras,Desconocidos4),
                procesar_frances(X,RatioAcertadas5,NumeroTotalPalabras,Desconocidos5),
                procesar_italiano(X,RatioAcertadas6,NumeroTotalPalabras,Desconocidos6),
               procesar_euskera(X,RatioAcertadas7,NumeroTotalPalabras,Desconocidos7),
               RatioAcertadas2<RatioAcertadas,
               RatioAcertadas>RatioAcertadas3,
               RatioAcertadas>RatioAcertadas4,
                RatioAcertadas>RatioAcertadas5,
                RatioAcertadas>RatioAcertadas6,
                RatioAcertadas>RatioAcertadas7
               !.

discriminador(X,'catalán'):-
              transcribir(X,W),
               lista(W,Y),
               contar(NumeroTotalPalabras,Y),
               procesar_latin(X,RatioAcertadas,NumeroTotalPalabras,Desconocidos1),
               procesar_español(X,RatioAcertadas2,NumeroTotalPalabras,Desconocidos2),
               procesar_catalan(X,RatioAcertadas3,NumeroTotalPalabras,Desconocidos3),
               procesar_portugues(X,RatioAcertadas4,NumeroTotalPalabras,Desconocidos4),
               procesar_frances(X,RatioAcertadas5,NumeroTotalPalabras,Desconocidos5),
               procesar_italiano(X,RatioAcertadas6,NumeroTotalPalabras,Desconocidos6),
               procesar_euskera(X,RatioAcertadas7,NumeroTotalPalabras,Desconocidos7),
               RatioAcertadas2<RatioAcertadas3,
               RatioAcertadas4<RatioAcertadas3,
               RatioAcertadas3>RatioAcertadas,
               RatioAcertadas3>RatioAcertadas5,
               RatioAcertadas3>RatioAcertadas6,
               RatioAcertadas3>RatioAcertadas7
               !.
               
discriminador(X,'portugués'):-
              transcribir(X,W),
               lista(W,Y),
               contar(NumeroTotalPalabras,Y),
               procesar_latin(X,RatioAcertadas,NumeroTotalPalabras,Desconocidos1),
               procesar_español(X,RatioAcertadas2,NumeroTotalPalabras,Desconocidos2),
               procesar_catalan(X,RatioAcertadas3,NumeroTotalPalabras,Desconocidos3),
               procesar_portugues(X,RatioAcertadas4,NumeroTotalPalabras,Desconocidos4),
               procesar_frances(X,RatioAcertadas5,NumeroTotalPalabras,Desconocidos5),
               procesar_italiano(X,RatioAcertadas6,NumeroTotalPalabras,Desconocidos6),
               procesar_euskera(X,RatioAcertadas7,NumeroTotalPalabras,Desconocidos7),
               RatioAcertadas2<RatioAcertadas4,
               RatioAcertadas4>RatioAcertadas3,
               RatioAcertadas4>RatioAcertadas,
               RatioAcertadas4>RatioAcertadas5,
               RatioAcertadas4>RatioAcertadas6,
               RatioAcertadas4>RatioAcertadas7
               !.
               
discriminador(X,'francés'):-
              transcribir(X,W),
               lista(W,Y),
               contar(NumeroTotalPalabras,Y),
               procesar_latin(X,RatioAcertadas,NumeroTotalPalabras,Desconocidos1),
               procesar_español(X,RatioAcertadas2,NumeroTotalPalabras,Desconocidos2),
               procesar_catalan(X,RatioAcertadas3,NumeroTotalPalabras,Desconocidos3),
               procesar_portugues(X,RatioAcertadas4,NumeroTotalPalabras,Desconocidos4),
               procesar_frances(X,RatioAcertadas5,NumeroTotalPalabras,Desconocidos5),
               procesar_italiano(X,RatioAcertadas6,NumeroTotalPalabras,Desconocidos6),
               procesar_euskera(X,RatioAcertadas7,NumeroTotalPalabras,Desconocidos7),
               RatioAcertadas2<RatioAcertadas5,
               RatioAcertadas5>RatioAcertadas3,
               RatioAcertadas5>RatioAcertadas,
               RatioAcertadas5>RatioAcertadas4,
               RatioAcertadas5>RatioAcertadas6,
               RatioAcertadas5>RatioAcertadas7
               !.
               
discriminador(X,'italiano'):-
              transcribir(X,W),
               lista(W,Y),
               contar(NumeroTotalPalabras,Y),
               procesar_latin(X,RatioAcertadas,NumeroTotalPalabras,Desconocidos1),
               procesar_español(X,RatioAcertadas2,NumeroTotalPalabras,Desconocidos2),
               procesar_catalan(X,RatioAcertadas3,NumeroTotalPalabras,Desconocidos3),
               procesar_portugues(X,RatioAcertadas4,NumeroTotalPalabras,Desconocidos4),
               procesar_frances(X,RatioAcertadas5,NumeroTotalPalabras,Desconocidos5),
               procesar_italiano(X,RatioAcertadas6,NumeroTotalPalabras,Desconocidos6),
               procesar_euskera(X,RatioAcertadas7,NumeroTotalPalabras,Desconocidos7),
               RatioAcertadas2<RatioAcertadas6,
               RatioAcertadas6>RatioAcertadas3,
               RatioAcertadas6>RatioAcertadas,
               RatioAcertadas6>RatioAcertadas4,
               RatioAcertadas6>RatioAcertadas5,
               RatioAcertadas6>RatioAcertadas7
               !.
               
discriminador(X,'euskera'):-
              transcribir(X,W),
               lista(W,Y),
               contar(NumeroTotalPalabras,Y),
               procesar_latin(X,RatioAcertadas,NumeroTotalPalabras,Desconocidos1),
               procesar_español(X,RatioAcertadas2,NumeroTotalPalabras,Desconocidos2),
               procesar_catalan(X,RatioAcertadas3,NumeroTotalPalabras,Desconocidos3),
               procesar_portugues(X,RatioAcertadas4,NumeroTotalPalabras,Desconocidos4),
               procesar_frances(X,RatioAcertadas5,NumeroTotalPalabras,Desconocidos5),
               procesar_italiano(X,RatioAcertadas6,NumeroTotalPalabras,Desconocidos6),
               procesar_euskera(X,RatioAcertadas7,NumeroTotalPalabras,Desconocidos7),
               RatioAcertadas6<RatioAcertadas7,
               RatioAcertadas7>RatioAcertadas3,
               RatioAcertadas7>RatioAcertadas,
               RatioAcertadas7>RatioAcertadas4,
               RatioAcertadas7>RatioAcertadas5,
               RatioAcertadas7>RatioAcertadas2
               !.

               
discriminador(X,'empate').


azrael(X,Intersection):-
               transcribir(X,W),
               !,
               lista(W,Y),
               contar(NumeroTotalPalabras,Y),
               procesar_latin(X,RatioAcertadas,NumeroTotalPalabras,Desconocidos1),
               procesar_español(X,RatioAcertadas2,NumeroTotalPalabras,Desconocidos2),
               procesar_catalan(X,RatioAcertadas3,NumeroTotalPalabras,Desconocidos3),
               procesar_portugues(X,RatioAcertadas4,NumeroTotalPalabras,Desconocidos4),
               procesar_frances(X,RatioAcertadas5,NumeroTotalPalabras,Desconocidos5),
               procesar_italiano(X,RatioAcertadas6,NumeroTotalPalabras,Desconocidos6),
               procesar_euskera(X,RatioAcertadas7,NumeroTotalPalabras,Desconocidos7),
               write('texto en latín:'),
               write(''),
               write(RatioAcertadas),
               write('%'),
               nl,
               write('texto en español:'),
               write(''),
               write(RatioAcertadas2),
               write('%'),
               nl,
               write('texto en catalán:'),
               write(''),
               write(RatioAcertadas3),
               write('%'),
               nl,
               write('texto en portugués:'),
               write(''),
               write(RatioAcertadas4),
               write('%'),
               nl,
               write('texto en francés:'),
               write(''),
               write(RatioAcertadas5),
               write('%'),
               nl,
               write('texto en italiano:'),
               write(''),
               write(RatioAcertadas6),
               write('%'),
                nl,
               write('texto en euskera:'),
               write(''),
               write(RatioAcertadas7),
               write('%'),
               intersection(Desconocidos1,Desconocidos2,D12),
               intersection(D12,Desconocidos3,_interseccion),
               intersection(_interseccion,Desconocidos4,Interseccion),
               intersection(Interseccion,Desconocidos5,Inter),
               intersection(Inter,Desconocidos6,InterSect),
               intersection(InterSect,Desconocidos7,InterSectBis),
               limpiar_interseccion(InterSectBis,Intersection).
               
limpiar_interseccion([],[]).
               
limpiar_interseccion([desconocido(X)|T],[X|Y]):-
                                                limpiar_interseccion(T,Y).

               

               
               





procesar_latin(X,RatioAcertadas,NumeroTotalPalabras,Origen):-
               latin(X,Restos,Origen),
               contarDesconocidos(Desconocidos,Origen),
               !,
               Acertadas is NumeroTotalPalabras-Desconocidos,
               RatioAcertadas is (Acertadas/NumeroTotalPalabras)*100.


procesar_español(X,RatioAcertadas,NumeroTotalPalabras,Origen):-
               español(X,Restos,Origen),
               contarDesconocidos(Desconocidos,Origen),
               !,
               Acertadas is NumeroTotalPalabras-Desconocidos,
               RatioAcertadas is (Acertadas/NumeroTotalPalabras)*100.
               
procesar_catalan(X,RatioAcertadas,NumeroTotalPalabras,Origen):-
               catalan(X,Restos,Origen),
               contarDesconocidos(Desconocidos,Origen),
               !,
               Acertadas is NumeroTotalPalabras-Desconocidos,
               RatioAcertadas is (Acertadas/NumeroTotalPalabras)*100.
               
procesar_portugues(X,RatioAcertadas,NumeroTotalPalabras,Origen):-
               portugues(X,Restos,Origen),
               contarDesconocidos(Desconocidos,Origen),
               !,
               Acertadas is NumeroTotalPalabras-Desconocidos,
               RatioAcertadas is (Acertadas/NumeroTotalPalabras)*100.
               
procesar_frances(X,RatioAcertadas,NumeroTotalPalabras,Origen):-
               frances(X,Restos,Origen),
               contarDesconocidos(Desconocidos,Origen),
               !,
               Acertadas is NumeroTotalPalabras-Desconocidos,
               RatioAcertadas is (Acertadas/NumeroTotalPalabras)*100.
               
               
               
procesar_italiano(X,RatioAcertadas,NumeroTotalPalabras,Origen):-
               italiano(X,Restos,Origen),
               contarDesconocidos(Desconocidos,Origen),
               !,
               Acertadas is NumeroTotalPalabras-Desconocidos,
               RatioAcertadas is (Acertadas/NumeroTotalPalabras)*100.
               
procesar_euskera(X,RatioAcertadas,NumeroTotalPalabras,Origen):-
               euskera(X,Restos,Origen),
               contarDesconocidos(Desconocidos,Origen),
               !,
               Acertadas is NumeroTotalPalabras-Desconocidos,
               RatioAcertadas is (Acertadas/NumeroTotalPalabras)*100.

contar(0,[]).

contar(Numero,[H|T]):-
                       contar(NumeroBis,T),
                       Numero is NumeroBis+1.

contarDesconocidos(0,[]).

contarDesconocidos(Numero,[desconocido(H)|T]):-
                      contarDesconocidos(NumeroBis,T),
                       Numero is NumeroBis+1,
                      !.

contarDesconocidos(Numero,[H|T]):-
                      contarDesconocidos(Numero,T).





recuperar_descartesLatín([],[]).

recuperar_descartesLatín([N|M],[Restos|MasRestos]):-
               atom_codes(Restos,N),
               recuperar_descartesLatín(M,MasRestos).
               
               
