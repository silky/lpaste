--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = off;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET escape_string_warning = off;

SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: channel; Type: TABLE; Schema: public; Owner: amelie; Tablespace: 
--

CREATE TABLE channel (
    id integer NOT NULL,
    title character varying(28) NOT NULL
);


ALTER TABLE public.channel OWNER TO amelie;

--
-- Name: channel_id_seq; Type: SEQUENCE; Schema: public; Owner: amelie
--

CREATE SEQUENCE channel_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.channel_id_seq OWNER TO amelie;

--
-- Name: channel_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: amelie
--

ALTER SEQUENCE channel_id_seq OWNED BY channel.id;


--
-- Name: channel_id_seq; Type: SEQUENCE SET; Schema: public; Owner: amelie
--

SELECT pg_catalog.setval('channel_id_seq', 24, true);


--
-- Name: language; Type: TABLE; Schema: public; Owner: amelie; Tablespace: 
--

CREATE TABLE language (
    id integer NOT NULL,
    name character varying(32) NOT NULL,
    title character varying(64) NOT NULL,
    ordinal integer DEFAULT 0 NOT NULL,
    visible boolean DEFAULT false NOT NULL
);


ALTER TABLE public.language OWNER TO amelie;

--
-- Name: language_id_seq; Type: SEQUENCE; Schema: public; Owner: amelie
--

CREATE SEQUENCE language_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


ALTER TABLE public.language_id_seq OWNER TO amelie;

--
-- Name: language_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: amelie
--

ALTER SEQUENCE language_id_seq OWNED BY language.id;


--
-- Name: language_id_seq; Type: SEQUENCE SET; Schema: public; Owner: amelie
--

SELECT pg_catalog.setval('language_id_seq', 119, true);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: amelie
--

ALTER TABLE channel ALTER COLUMN id SET DEFAULT nextval('channel_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: amelie
--

ALTER TABLE language ALTER COLUMN id SET DEFAULT nextval('language_id_seq'::regclass);


--
-- Data for Name: channel; Type: TABLE DATA; Schema: public; Owner: amelie
--

COPY channel (id, title) FROM stdin;
1	#haskell
2	#xmonad
3	#javascript
4	#python
5	#ruby
6	#lisp
7	#scala
8	#agda
9	#coffeescript
10	#arc
11	##c
12	#clojure
13	#scheme
14	##prolog
15	#emacs
16	#hpaste
17	#happs
18	#fay
19	#haskell-lens
20	#ghc
21	#hakyll
22	#diagrams
23	#haskell-mobile
24	#idris
\.


--
-- Data for Name: language; Type: TABLE DATA; Schema: public; Owner: amelie
--

COPY language (id, name, title, ordinal, visible) FROM stdin;
14	d	D	999	f
27	xml	XML	999	f
32	actionscript 3	ActionScript 3	999	f
47	css	CSS	999	f
71	irc logs	IRC logs	999	f
23	perl	Perl	999	t
8	javascript	JavaScript	999	t
19	lua	Lua	999	t
13	cpp	C++	999	t
16	java	Java	999	t
22	objectivec	Objective-C	999	t
116	smalltalk	Smalltalk	999	t
117	cs	C#	999	t
29	sql	SQL	999	t
51	diff	Diff	999	t
10	bash	Bash/shell	999	t
1	haskell	Haskell	0	t
28	agda	Agda (via Haskell)	1	t
21	ocaml	OCaml	2	t
9	lisp	Common Lisp	3	t
114	lisp	Emacs Lisp	3	t
25	ruby	Ruby	4	t
24	prolog	Prolog	5	t
113	elisp	Elisp	6	f
26	scala	Scala	3	t
15	erlang	Erlang	3	t
18	literatehaskell	Literate Haskell	3	t
115	go	Go	4	t
4	python	Python	4	t
12	c	C	999	t
106	tex	TeX	999	t
118	scheme	Scheme	5	t
119	idris	Idris	5	t
\.


--
-- Name: channel_pkey; Type: CONSTRAINT; Schema: public; Owner: amelie; Tablespace: 
--

ALTER TABLE ONLY channel
    ADD CONSTRAINT channel_pkey PRIMARY KEY (id);


--
-- Name: language_pkey; Type: CONSTRAINT; Schema: public; Owner: amelie; Tablespace: 
--

ALTER TABLE ONLY language
    ADD CONSTRAINT language_pkey PRIMARY KEY (id);


--
-- PostgreSQL database dump complete
--

