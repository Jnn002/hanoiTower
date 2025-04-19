% --- Configuración ---
:- dynamic default_port/1.
default_port(8080). % <---- ¡DEFINE TU PUERTO AQUÍ!

% --- Carga de Módulos ---
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(lists)).
:- use_module(library(apply)).

% --- Configuración CORS ---
:- set_setting(http:cors, [*]).

% --- Estado del Juego ---
:- dynamic game_state/6.

% --- Rutas HTTP ---
:- http_handler(root(.), http_reply_file('hanoi.html', []), []).
:- http_handler(root(hanoi_style), http_reply_file('hanoi_style.css', []), []).
:- http_handler(root(hanoi_script), http_reply_file('hanoi_script.js', []), []).
:- http_handler(root(api/state), handle_get_state, [methods([get])]).
:- http_handler(root(api/move), handle_post_move, [methods([post])]).
:- http_handler(root(api/reset), handle_post_reset, [methods([post])]).

% --- Lógica de los Handlers (sin cambios significativos) ---
handle_get_state(_Request) :- % Cambiado _Request para evitar singleton warning si no se usa
    cors_enable,
    game_state(A, B, C, Moves, Target, MinMoves),
    state_to_json_dict(state(A,B,C), Moves, Target, MinMoves, Dict),
    reply_json_dict(Dict).

handle_post_move(Request) :-
    cors_enable(Request, [methods([post])]), % Añadido CORS preflight
    http_read_json_dict(Request, MoveData),
    atom_string(OrigenAtom, MoveData.from),
    atom_string(DestinoAtom, MoveData.to),
    game_state(Pa, Pb, Pc, Moves, Target, MinMoves),
    EstadoActual = state(Pa, Pb, Pc),
    (   validar_y_aplicar_movimiento(EstadoActual, OrigenAtom, DestinoAtom, NuevoEstado)
    ->  NuevoContador is Moves + 1,
        NuevoEstado = state(NPa, NPb, NPc),
        retractall(game_state(_,_,_,_,_,_)),
        assertz(game_state(NPa, NPb, NPc, NuevoContador, Target, MinMoves)),
        ( estado_final(NuevoEstado, Target) -> Win = true ; Win = false ),
        state_to_json_dict(NuevoEstado, NuevoContador, Target, MinMoves, DictOut),
        reply_json_dict(json{success: true, message: 'Movimiento realizado.', win: Win, state: DictOut})
    ;   state_to_json_dict(EstadoActual, Moves, Target, MinMoves, DictOut),
        reply_json_dict(json{success: false, message: 'Movimiento inválido.', win: false, state: DictOut}, [status(400)]) % Devolver Bad Request
    ).

handle_post_reset(Request) :-
    cors_enable(Request, [methods([post])]), % Añadido CORS preflight
    http_read_json_dict(Request, ResetData),
    N = ResetData.get(disks, 3), % Obtener con default
    ( integer(N), N > 0 ->
        inicializar_juego_predeterminado(N),
        game_state(A, B, C, Moves, Target, MinMoves),
        state_to_json_dict(state(A,B,C), Moves, Target, MinMoves, DictOut),
        reply_json_dict(json{success: true, message: 'Juego reiniciado.', state: DictOut})
    ;   reply_json_dict(json{success: false, message: 'Número de discos inválido.'}, [status(400)])
    ).

% --- Lógica del Juego ---
inicializar_juego_predeterminado(N) :-
    inicializar_estado_interno(N, state(DiscosA, DiscosB, DiscosC), Objetivo),
    MinMoves is 2^N - 1,
    retractall(game_state(_,_,_,_,_,_)),
    assertz(game_state(DiscosA, DiscosB, DiscosC, 0, Objetivo, MinMoves)).

inicializar_estado_interno(N, state(DiscosIniciales, [], []), DiscosIniciales) :-
    findall(Num, between(1, N, Num), DiscosIniciales).

es_clavija(a).
es_clavija(b).
es_clavija(c).

validar_y_aplicar_movimiento(state(Pa, Pb, Pc), a, b, state(NPa, NPb, Pc)) :- mover_disco(a, b, Pa, Pb, NPa, NPb).
validar_y_aplicar_movimiento(state(Pa, Pb, Pc), a, c, state(NPa, Pb, NPc)) :- mover_disco(a, c, Pa, Pc, NPa, NPc).
validar_y_aplicar_movimiento(state(Pa, Pb, Pc), b, a, state(NPa, NPb, Pc)) :- mover_disco(b, a, Pb, Pa, NPb, NPa).
validar_y_aplicar_movimiento(state(Pa, Pb, Pc), b, c, state(Pa, NPb, NPc)) :- mover_disco(b, c, Pb, Pc, NPb, NPc).
validar_y_aplicar_movimiento(state(Pa, Pb, Pc), c, a, state(NPa, Pb, NPc)) :- mover_disco(c, a, Pc, Pa, NPc, NPa).
validar_y_aplicar_movimiento(state(Pa, Pb, Pc), c, b, state(Pa, NPb, NPc)) :- mover_disco(c, b, Pc, Pb, NPc, NPb).
validar_y_aplicar_movimiento(_, Origen, Destino, _) :- Origen == Destino, !, fail.

mover_disco(_NomO, _NomD, [Disco|RestoOrigen], [], RestoOrigen, [Disco]) :- !.
mover_disco(_NomO, _NomD, [Disco|RestoOrigen], [TopeDestino|RestoDestino], RestoOrigen, [Disco, TopeDestino|RestoDestino]) :-
    Disco < TopeDestino, !.

estado_final(state([], [], DiscosC), DiscosObjetivo) :-
    DiscosC == DiscosObjetivo.

state_to_json_dict(state(A, B, C), Moves, _Target, MinMoves, Dict) :-
    reverse(A, A_rev),
    reverse(B, B_rev),
    reverse(C, C_rev),
    Dict = json{
        a: A_rev,
        b: B_rev,
        c: C_rev,
        moves: Moves,
        min_moves: MinMoves
    }.

% --- Inicialización del estado del juego ---
initialize_game_state :-
    % Solo inicializa si no hay un estado ya cargado (evita reinicio en recargas)
    (current_predicate(game_state/6), game_state(_,_,_,_,_,_)) ->
        true
    ;
        inicializar_juego_predeterminado(3)
    .

% --- Iniciar el servidor HTTP ---
start_server(Port) :-
     catch(
        (   % Llama a http_server ANTES de los mensajes para asegurar que el puerto está asignado
            http_server(http_dispatch, [port(Port)]),
            format('~N*** Servidor de Hanoi iniciado exitosamente en http://localhost:~w ***~n', [Port]),
            format('*** Abre tu navegador en esa dirección para jugar. ***~n'),
            format('*** Presiona Ctrl+C para detener el servidor. ***~n~n'),
            flush_output
        ),
        Error,
        (   format(user_error, 'Error al iniciar el servidor HTTP: ~w~n', [Error]),
            halt(1) % Termina con error si el servidor no puede iniciar
        )
    ).

% --- Mensajes de entrada/inicio ---
main :-
    initialize_game_state, % Asegura que el estado del juego exista
    default_port(Port),   % Obtiene el puerto por defecto
    start_server(Port),   % Intenta iniciar el servidor
    % Mantiene el hilo principal vivo esperando indefinidamente
    thread_get_message(_).

% --- Directiva de Inicialización ---
% Ejecuta 'main' después de cargar el archivo cuando se corre como script.
:- set_prolog_flag(toplevel_goal, main).