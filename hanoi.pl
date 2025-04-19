:- use_module(library(lists)). % Necesario para reverse/2 y maybe length/2

% --- Predicado principal para iniciar el juego ---
jugar_hanoi(N) :-
    N > 0,
    % Cambiado: inicializar_estado ahora usa [1,..,N] internamente
    inicializar_estado(N, EstadoInicial, DiscosObjetivo),
    write('--- ¡Bienvenido a las Torres de Hanoi! ---'), nl,
    write('Mueve los discos de la clavija \'a\' a la clavija \'c\'.'), nl,
    write('Introduce los movimientos como \'a.\' y luego \'c.\'.'), nl, % Prompt simplificado
    ciclo_juego(N, EstadoInicial, 0, DiscosObjetivo).
jugar_hanoi(N) :-
    N =< 0,
    write('El número de discos debe ser positivo.'), nl.

% --- Inicialización del estado ---
% Estado = state(ListaA, ListaB, ListaC)
% Representación interna: El disco más pequeño (tope) está en la CABEZA de la lista.
inicializar_estado(N, state(DiscosIniciales, [], []), DiscosIniciales) :-
    findall(Num, between(1, N, Num), DiscosIniciales). % Correcto: [1, 2, ..., N]

% --- Ciclo principal del juego ---
ciclo_juego(N, Estado, ContadorMovimientos, DiscosObjetivo) :-
    mostrar_estado(Estado),
    (estado_final(Estado, DiscosObjetivo) -> % Condición de parada usa el nuevo estado_final
        nl, write('¡Felicidades! Has resuelto el puzzle en '), write(ContadorMovimientos), write(' movimientos.'), nl,
        MovimientosMinimos is 2^N - 1,
        (ContadorMovimientos == MovimientosMinimos ->
            write('¡Lo hiciste en el número mínimo de movimientos!'), nl
        ;   write('El número mínimo de movimientos es: '), write(MovimientosMinimos), nl
        )
    ; % Si no es el estado final, continuar
        repeat, % Pedir movimiento hasta que sea válido
        write('Movimiento #'), NuevoContador is ContadorMovimientos + 1, write(NuevoContador), write(': '),
        % leer_movimiento ahora retorna true/false
        leer_movimiento(Origen, Destino), % leer_movimiento valida el formato
        ( validar_y_aplicar_movimiento(Estado, Origen, Destino, NuevoEstado) -> % Intenta aplicar
            !, % Corta el repeat si el movimiento fue válido y aplicado
            ciclo_juego(N, NuevoEstado, NuevoContador, DiscosObjetivo) % Recursión
        ;   % Si validar_y_aplicar falla (imprime error dentro), el repeat continúa
            fail % Asegura que repeat continúe si validar_y_aplicar falla
        )
    ).


% --- Mostrar el estado actual del juego ---
mostrar_estado(state(A, B, C)) :-
    nl, write('Estado Actual:'), nl,
    write('  a: '), imprimir_clavija(A), nl,
    write('  b: '), imprimir_clavija(B), nl,
    write('  c: '), imprimir_clavija(C), nl.

% Corregido: Imprime con el más grande abajo (revierte la representación interna [1..N])
imprimir_clavija([]) :- write('(vacía)').
imprimir_clavija(Lista) :-
    reverse(Lista, ListaRev), % Revierte [1..N] a [N..1] para mostrar
    format('~w', [ListaRev]).

% --- Leer el movimiento del usuario ---
% Simplificado y corregido el prompt
leer_movimiento(Origen, Destino) :-
    repeat, % Loop interno hasta que la entrada sea válida
    catch(
        (   write('Clavija origen (a, b, c): '), flush_output, read(OrigenAtom),
            write('Clavija destino (a, b, c): '), flush_output, read(DestinoAtom),
            ( es_clavija(OrigenAtom), es_clavija(DestinoAtom) ->
                Origen = OrigenAtom, Destino = DestinoAtom,
                ! % Éxito, corta el repeat interno
            ;   write('Error: Usa solo los átomos a, b, o c para las clavijas.'), nl,
                fail % Falla para repetir la lectura
            )
        ),
        Error,
        (   write('Error de entrada: '), print_message(error, Error), nl,
            write('Por favor, introduce átomos como a, b, c.'), nl,
            fail % Falla para repetir la lectura
        )
    ).


es_clavija(a).
es_clavija(b).
es_clavija(c).

% --- Validar y aplicar el movimiento ---
% Llama a mover_disco que ahora sí contiene toda la lógica y errores.
% Falla si el movimiento no es válido.

validar_y_aplicar_movimiento(state(Pa, Pb, Pc), a, b, state(NPa, NPb, Pc)) :-
    mover_disco(a, b, Pa, Pb, NPa, NPb). % Pasa nombres de clavijas para mensajes de error
validar_y_aplicar_movimiento(state(Pa, Pb, Pc), a, c, state(NPa, Pb, NPc)) :-
    mover_disco(a, c, Pa, Pc, NPa, NPc).
validar_y_aplicar_movimiento(state(Pa, Pb, Pc), b, a, state(NPa, NPb, Pc)) :-
    mover_disco(b, a, Pb, Pa, NPb, NPa).
validar_y_aplicar_movimiento(state(Pa, Pb, Pc), b, c, state(Pa, NPb, NPc)) :-
    mover_disco(b, c, Pb, Pc, NPb, NPc).
validar_y_aplicar_movimiento(state(Pa, Pb, Pc), c, a, state(NPa, Pb, NPc)) :-
    mover_disco(c, a, Pc, Pa, NPc, NPa).
validar_y_aplicar_movimiento(state(Pa, Pb, Pc), c, b, state(Pa, NPb, NPc)) :-
    mover_disco(c, b, Pc, Pb, NPc, NPb).

% Caso base de error: Mover a la misma clavija
validar_y_aplicar_movimiento(_, Origen, Destino, _) :-
    Origen == Destino,
    write('Error: La clavija origen y destino no pueden ser la misma.'), nl,
    fail.

% --- Lógica interna de mover un disco ---
% mover_disco(+NombreOrigen, +NombreDestino, +ClavijaOrigen, +ClavijaDestino, -NuevaClavijaOrigen, -NuevaClavijaDestino)
% Falla si el movimiento es inválido, imprimiendo la razón.

% Caso 1: Mover de una clavija no vacía a una clavija vacía
mover_disco(_NomO, _NomD, [Disco|RestoOrigen], [], RestoOrigen, [Disco]) :- !.

% Caso 2: Mover de una clavija no vacía a otra no vacía (regla de Hanoi)
mover_disco(_NomO, _NomD, [Disco|RestoOrigen], [TopeDestino|RestoDestino], RestoOrigen, [Disco, TopeDestino|RestoDestino]) :-
    Disco < TopeDestino, % ¡Regla clave! El disco a mover debe ser más pequeño
    !.

% Caso 3: Intento de mover disco grande sobre pequeño
mover_disco(NomO, NomD, [Disco|_], [TopeDestino|_], _, _) :-
    Disco > TopeDestino, % Condición de error específica
    format('Error: No se puede mover el disco ~w de ~w sobre el disco ~w en ~w.~n', [Disco, NomO, TopeDestino, NomD]),
    fail.

% Caso 4: Intento de mover de una clavija vacía
mover_disco(NomO, _NomD, [], _, _, _) :-
    format('Error: La clavija origen (~w) está vacía.~n', [NomO]),
    fail.

% --- Comprobar si es el estado final ---
% Corregido: El estado final es cuando A y B están vacías y C tiene [1, 2, ..., N].
estado_final(state([], [], DiscosC), DiscosObjetivo) :-
    DiscosC == DiscosObjetivo. % Comparación directa con el objetivo [1, ..., N]

% --- Ejemplo de uso ---
% ?- [hanoi].
% ?- jugar_hanoi(3).