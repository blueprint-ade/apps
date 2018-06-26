CREATE TABLE public.game_events_demo
(
    event_id text COLLATE pg_catalog."default" NOT NULL,
    game_id real,
    player_id real,
    event text COLLATE pg_catalog."default",
    date date,
    "time" text COLLATE pg_catalog."default",
    CONSTRAINT "UNIQUE" PRIMARY KEY (event_id)
)
WITH (
    OIDS = FALSE
)
TABLESPACE pg_default;
