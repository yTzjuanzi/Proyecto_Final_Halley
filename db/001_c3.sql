-- C3: lecturas en vivo. Se puede ejecutar varias veces sin borrar datos.
BEGIN;

-- 1. Lo que llegó, byte por byte. Se guarda ANTES de confirmar al emisor.
CREATE TABLE IF NOT EXISTS lectura_viva (
    seq          bigserial   PRIMARY KEY,
    id_mensaje   text        UNIQUE,          -- NULL si el mensaje es ilegible
    crudo        bytea       NOT NULL,
    recibido_en  timestamptz NOT NULL DEFAULT clock_timestamp()
);

-- 2. Mismo identificador con otro contenido: también deja rastro.
CREATE TABLE IF NOT EXISTS conflicto_envio (
    id           bigserial   PRIMARY KEY,
    id_mensaje   text        NOT NULL REFERENCES lectura_viva (id_mensaje),
    crudo        bytea       NOT NULL,
    recibido_en  timestamptz NOT NULL DEFAULT clock_timestamp()
);

-- 3. La evidencia incorporada. "clave" impide contar dos veces el mismo hecho.
CREATE TABLE IF NOT EXISTS evento_vivo (
    seq    bigint PRIMARY KEY REFERENCES lectura_viva (seq),
    clave  text   NOT NULL UNIQUE,
    datos  jsonb  NOT NULL                    -- mismos campos que un evento de C1
);

-- 4. Qué pasó con cada lectura. "version" crece en el orden de confirmación.
CREATE TABLE IF NOT EXISTS resultado_vivo (
    version       bigserial   PRIMARY KEY,
    seq           bigint      NOT NULL UNIQUE REFERENCES lectura_viva (seq),
    estado        text        NOT NULL CHECK (estado IN ('usada','duplicada','descartada')),
    motivo        text,
    avisos        jsonb       NOT NULL DEFAULT '[]',
    procesado_en  timestamptz NOT NULL DEFAULT clock_timestamp(),
    CHECK (estado = 'usada' OR motivo IS NOT NULL)
);

CREATE OR REPLACE FUNCTION recibir_lectura(p_id text, p_crudo bytea)
RETURNS text LANGUAGE plpgsql AS $$
DECLARE
    previo bytea;
BEGIN
    INSERT INTO lectura_viva (id_mensaje, crudo) VALUES (p_id, p_crudo)
    ON CONFLICT (id_mensaje) DO NOTHING;
    IF FOUND THEN RETURN 'guardada'; END IF;

    SELECT crudo INTO previo FROM lectura_viva WHERE id_mensaje = p_id;
    IF previo = p_crudo THEN RETURN 'repetida'; END IF;   -- reintento

    INSERT INTO conflicto_envio (id_mensaje, crudo) VALUES (p_id, p_crudo);
    RETURN 'conflicto';
END $$;

-- Evento y resultado se guardan juntos o no se guarda ninguno.
CREATE OR REPLACE FUNCTION registrar_resultado(
    p_seq bigint, p_estado text, p_motivo text, p_avisos jsonb,
    p_clave text, p_datos jsonb)
RETURNS text LANGUAGE plpgsql AS $$
DECLARE
    v_estado  text := p_estado;
    v_motivo  text := p_motivo;
    v_previo  text;
    v_version bigint;
BEGIN
    -- Un registro a la vez: así "version" sigue el orden de confirmación.
    PERFORM pg_advisory_xact_lock(7003);

    SELECT estado INTO v_previo FROM resultado_vivo WHERE seq = p_seq;
    IF FOUND THEN RETURN v_previo; END IF;

    IF v_estado = 'usada' THEN
        INSERT INTO evento_vivo (seq, clave, datos) VALUES (p_seq, p_clave, p_datos)
        ON CONFLICT (clave) DO NOTHING;
        IF NOT FOUND THEN
            v_estado := 'duplicada';
            SELECT 'mismo hecho que la lectura viva ' || seq INTO v_motivo
              FROM evento_vivo WHERE clave = p_clave;
        END IF;
    END IF;

    INSERT INTO resultado_vivo (seq, estado, motivo, avisos)
    VALUES (p_seq, v_estado, v_motivo, p_avisos)
    RETURNING version INTO v_version;

    PERFORM pg_notify('evidencia_nueva', v_version::text);
    RETURN v_estado;
END $$;

-- La evidencia no se modifica ni se borra.
-- Esto evita accidentes; la PRUEBA de no alteración es trabajo de C5.
CREATE OR REPLACE FUNCTION prohibir_cambios() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
    RAISE EXCEPTION 'la tabla % es de solo inserción', TG_TABLE_NAME;
END $$;

DO $$
DECLARE t text;
BEGIN
    FOREACH t IN ARRAY ARRAY['lectura_viva','conflicto_envio','evento_vivo','resultado_vivo'] LOOP
        EXECUTE format('DROP TRIGGER IF EXISTS solo_insercion ON %I', t);
        EXECUTE format('CREATE TRIGGER solo_insercion BEFORE UPDATE OR DELETE OR TRUNCATE ON %I
                        FOR EACH STATEMENT EXECUTE FUNCTION prohibir_cambios()', t);
    END LOOP;
END $$;

COMMIT;
