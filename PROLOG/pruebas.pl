:- encoding(utf8).
% Pruebas de C2
% swipl -g run_tests -t halt PROLOG/pruebas.pl
:- use_module(library(plunit)).
:- ensure_loaded(motor).
:- initialization(cargar_evidencia('salida/evidencia.json')).

veredicto(Pregunta, V) :- responder(Pregunta, respuesta(V, _, _, _, _)).
detalle(Pregunta, Clave, Valor) :-
    responder(Pregunta, respuesta(_, _, Detalle, _, _)),
    memberchk(Clave-Valor, Detalle).

:- begin_tests(recorridos).

test(el_anillo_no_cuelga_la_busqueda) :-
    findall(P, camino('COM', 'GEN', P), Caminos),
    length(Caminos, 2).                       % por RAD o por DOR
test(todos_los_caminos_terminan) :-
    forall(( modulo(A, _, _), modulo(B, _, _) ),
           ( findall(P, camino(A, B, P), Ps), Ps \== [] )).
test(lab_b_a_alm_en_3_min_no) :-
    veredicto(recorrido_posible('LAB-B', 'ALM', 3), no).
test(lab_b_a_alm_en_4_min_si) :-
    veredicto(recorrido_posible('LAB-B', 'ALM', 4), si).
test(el_recorrido_cita_las_puertas_del_plano) :-
    responder(recorrido_posible('LAB-B', 'ALM', 3), respuesta(_, _, _, Evidencia, _)),
    Evidencia == ['estacion:5', 'estacion:8'].            % P4 y P7
test(gen_a_dor_sin_registro_antes_del_corte_no) :-
    veredicto(recorrido_sin_registro('GEN', 'DOR', 5, '01:38'), no).

:- end_tests(recorridos).

:- begin_tests(ubicacion).

test(vidal_seguro_en_lab_a) :-
    veredicto(donde_pudo_estar(vidal, '02:19'), si),
    detalle(donde_pudo_estar(vidal, '02:19'), pudo_estar, ['LAB-A']).
test(ruiz_segura_en_gen_a_la_1_30) :-
    detalle(donde_pudo_estar(ruiz, '01:30'), pudo_estar, ['GEN']).
test(irene_no_pudo_llegar_a_dor_a_las_1_45) :-
    veredicto(donde_pudo_estar(valcarcel, '01:45'), no_se_puede_saber),
    detalle(donde_pudo_estar(valcarcel, '01:45'), no_pudo_estar, ['DOR']).
test(alias_la_doctora_es_irene) :-
    detalle(donde_pudo_estar('la doctora', '00:52'), persona, valcarcel).
test(en_gen_antes_del_corte_solo_irene) :-
    veredicto(quien_pudo_estar('GEN', '01:36', '01:39'), si),
    detalle(quien_pudo_estar('GEN', '01:36', '01:39'), estuvo, [valcarcel]).
test(en_alm_durante_el_corte_no_se_descarta_a_nadie) :-
    veredicto(quien_pudo_estar('ALM', '01:50', '02:00'), no_se_puede_saber),
    detalle(quien_pudo_estar('ALM', '01:50', '02:00'), no_pudo_estar, []).

:- end_tests(ubicacion).

:- begin_tests(tarjetas).

test(lp04_solo_pudo_usarla_irene) :-
    veredicto(quien_pudo_usar('LP-04', '01:39:47'), si),
    detalle(quien_pudo_usar('LP-04', '01:39:47'), candidatos, [valcarcel]).
test(tarjeta_identificada_la_usa_su_titular) :-
    detalle(quien_pudo_usar('DV-02', '02:40:05'), candidatos, [vidal]).
test(tarjeta_inexistente_es_no) :-
    veredicto(quien_pudo_usar('AK-06', '01:00'), no).
test(sin_registro_con_energia_es_no) :-
    veredicto(quien_pudo_usar('MR-03', '01:00'), no).
test(sin_registro_durante_el_corte_no_se_puede_saber) :-
    veredicto(quien_pudo_usar('MR-03', '01:50'), no_se_puede_saber).

:- end_tests(tarjetas).

:- begin_tests(robustez).

test(pregunta_desconocida) :-
    veredicto(hola, pregunta_no_valida).
test(persona_inexistente) :-
    veredicto(donde_pudo_estar('Luka', '02:00'), pregunta_no_valida).
test(hora_mal_escrita) :-
    veredicto(donde_pudo_estar(vidal, '25:99'), pregunta_no_valida).
test(variables_sin_valor) :-
    veredicto(donde_pudo_estar(_, _), pregunta_no_valida).
test(misma_pregunta_misma_respuesta) :-
    responder(quien_pudo_estar('ALM', '01:50', '02:00'), R1),
    responder(quien_pudo_estar('ALM', '01:50', '02:00'), R2),
    R1 == R2.

:- end_tests(robustez).
