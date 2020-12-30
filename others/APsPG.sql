-- Database generated with pgModeler (PostgreSQL Database Modeler).
-- pgModeler  version: 0.9.2
-- PostgreSQL version: 12.0
-- Project Site: pgmodeler.io
-- Model Author: ---


-- Database creation must be done outside a multicommand file.
-- These commands were put in this file only as a convenience.
-- -- object: "APs" | type: DATABASE --
-- -- DROP DATABASE IF EXISTS "APs";
-- CREATE DATABASE "APs"
-- 	ENCODING = 'UTF8'
-- 	LC_COLLATE = 'pt_BR.UTF-8'
-- 	LC_CTYPE = 'pt_BR.UTF-8'
-- 	TABLESPACE = pg_default
-- 	OWNER = postgres;
-- -- ddl-end --
-- 

-- object: public.edges | type: TABLE --
-- DROP TABLE IF EXISTS public.edges CASCADE;
CREATE TABLE public.edges (
	"nId" integer NOT NULL,
	"nName" text NOT NULL,
	subs integer NOT NULL,
	prod integer NOT NULL,
	type text NOT NULL,
	reversible integer NOT NULL
);
-- ddl-end --
-- ALTER TABLE public.edges OWNER TO postgres;
-- ddl-end --

-- object: public.enzime | type: TABLE --
-- DROP TABLE IF EXISTS public.enzime CASCADE;
CREATE TABLE public.enzime (
	"eId" integer NOT NULL,
	"eName" text NOT NULL,
	"eLabel" text,
	CONSTRAINT entry_pk PRIMARY KEY ("eId")

);
-- ddl-end --
-- ALTER TABLE public.enzime OWNER TO postgres;
-- ddl-end --

-- object: public.path | type: TABLE --
-- DROP TABLE IF EXISTS public.path CASCADE;
CREATE TABLE public.path (
	"pId" integer NOT NULL,
	"pName" text,
	"pDesc" text NOT NULL,
	"pImage" text,
	"pLink" text,
	CONSTRAINT map_pk PRIMARY KEY ("pId")

);
-- ddl-end --
-- ALTER TABLE public.path OWNER TO postgres;
-- ddl-end --

-- object: public."nodesOnPath" | type: TABLE --
-- DROP TABLE IF EXISTS public."nodesOnPath" CASCADE;
CREATE TABLE public."nodesOnPath" (
	"nId" integer,
	"pId" integer,
	x integer,
	y integer
);
-- ddl-end --
-- ALTER TABLE public."nodesOnPath" OWNER TO postgres;
-- ddl-end --

-- object: public.reaction | type: TABLE --
-- DROP TABLE IF EXISTS public.reaction CASCADE;
CREATE TABLE public.reaction (
	"rId" integer NOT NULL,
	"rName" text NOT NULL,
	"rReversible" integer NOT NULL,
	CONSTRAINT reaction_pk PRIMARY KEY ("rId")

);
-- ddl-end --
COMMENT ON COLUMN public.reaction."rReversible" IS E'Can be 0 for irreversible or 1 to reversible';
-- ddl-end --
-- ALTER TABLE public.reaction OWNER TO postgres;
-- ddl-end --

-- object: public.compound | type: TABLE --
-- DROP TABLE IF EXISTS public.compound CASCADE;
CREATE TABLE public.compound (
	"cId" integer NOT NULL,
	"cName" text NOT NULL,
	"cDesc" text,
	CONSTRAINT compound_pk PRIMARY KEY ("cId")

);
-- ddl-end --
-- ALTER TABLE public.compound OWNER TO postgres;
-- ddl-end --

-- object: public.interaction | type: TABLE --
-- DROP TABLE IF EXISTS public.interaction CASCADE;
CREATE TABLE public.interaction (
	"eId1" integer NOT NULL,
	"eId2" integer NOT NULL,
	"cId" integer NOT NULL
);
-- ddl-end --
COMMENT ON TABLE public.interaction IS E'List of two enzimes related by a compound';
-- ddl-end --
-- ALTER TABLE public.interaction OWNER TO postgres;
-- ddl-end --

-- object: public."enzPathNode" | type: TABLE --
-- DROP TABLE IF EXISTS public."enzPathNode" CASCADE;
CREATE TABLE public."enzPathNode" (
	"nId" integer NOT NULL,
	"mId" integer NOT NULL,
	"eId" integer NOT NULL
);
-- ddl-end --
COMMENT ON TABLE public."enzPathNode" IS E'Map the realtion between enzimes, pathways and nodes';
-- ddl-end --
-- ALTER TABLE public."enzPathNode" OWNER TO postgres;
-- ddl-end --

-- object: public."subsProd" | type: TABLE --
-- DROP TABLE IF EXISTS public."subsProd" CASCADE;
CREATE TABLE public."subsProd" (
	"rId" integer NOT NULL,
	"cId" integer NOT NULL,
	"spType" text NOT NULL
);
-- ddl-end --
COMMENT ON TABLE public."subsProd" IS E'List of substrate and products that belongs to an reaction.';
-- ddl-end --
COMMENT ON COLUMN public."subsProd"."cId" IS E'Substrate ID from compound table';
-- ddl-end --
COMMENT ON COLUMN public."subsProd"."spType" IS E'Product ID from compound table';
-- ddl-end --
-- ALTER TABLE public."subsProd" OWNER TO postgres;
-- ddl-end --

-- object: public."enzReac" | type: TABLE --
-- DROP TABLE IF EXISTS public."enzReac" CASCADE;
CREATE TABLE public."enzReac" (
	"eId" integer NOT NULL,
	"rId" integer NOT NULL
);
-- ddl-end --
COMMENT ON TABLE public."enzReac" IS E'Enzimes can participate of N reactions and reactions can use N enzimes';
-- ddl-end --
-- ALTER TABLE public."enzReac" OWNER TO postgres;
-- ddl-end --

-- object: public."enzOnPath" | type: TABLE --
-- DROP TABLE IF EXISTS public."enzOnPath" CASCADE;
CREATE TABLE public."enzOnPath" (
	"eId" integer NOT NULL,
	"pId" integer NOT NULL,
	x integer NOT NULL,
	y integer NOT NULL
);
-- ddl-end --
-- ALTER TABLE public."enzOnPath" OWNER TO postgres;
-- ddl-end --

-- object: public."reacOnPath" | type: TABLE --
-- DROP TABLE IF EXISTS public."reacOnPath" CASCADE;
CREATE TABLE public."reacOnPath" (
	"rId" integer NOT NULL,
	"pId" integer NOT NULL
);
-- ddl-end --
-- ALTER TABLE public."reacOnPath" OWNER TO postgres;
-- ddl-end --

-- object: public."compOnPath" | type: TABLE --
-- DROP TABLE IF EXISTS public."compOnPath" CASCADE;
CREATE TABLE public."compOnPath" (
	"cId" integer NOT NULL,
	"pId" integer NOT NULL,
	x integer NOT NULL,
	y integer NOT NULL
);
-- ddl-end --
-- ALTER TABLE public."compOnPath" OWNER TO postgres;
-- ddl-end --

-- object: public."reactionAssociation" | type: TABLE --
-- DROP TABLE IF EXISTS public."reactionAssociation" CASCADE;
CREATE TABLE public."reactionAssociation" (
	"rId" integer NOT NULL,
	"mainRId" integer NOT NULL
);
-- ddl-end --
-- ALTER TABLE public."reactionAssociation" OWNER TO postgres;
-- ddl-end --

-- object: public."fakeNode" | type: TABLE --
-- DROP TABLE IF EXISTS public."fakeNode" CASCADE;
CREATE TABLE public."fakeNode" (
	"cId" integer NOT NULL,
	"cName" text NOT NULL,
	"cDesc" text,
	CONSTRAINT compound_pk2 PRIMARY KEY ("cId")

);
-- ddl-end --
-- ALTER TABLE public."fakeNode" OWNER TO postgres;
-- ddl-end --

-- object: public."nodeName" | type: TABLE --
-- DROP TABLE IF EXISTS public."nodeName" CASCADE;
CREATE TABLE public."nodeName" (
	"nId" integer NOT NULL,
	"nName" text NOT NULL,
	"childName" text NOT NULL,
	type text NOT NULL
);
-- ddl-end --
-- ALTER TABLE public."nodeName" OWNER TO postgres;
-- ddl-end --

-- object: "edgFk" | type: CONSTRAINT --
-- ALTER TABLE public.edges DROP CONSTRAINT IF EXISTS "edgFk" CASCADE;
ALTER TABLE public.edges ADD CONSTRAINT "edgFk" FOREIGN KEY ("nId")
REFERENCES public.reaction ("rId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "nxmFk" | type: CONSTRAINT --
-- ALTER TABLE public."nodesOnPath" DROP CONSTRAINT IF EXISTS "nxmFk" CASCADE;
ALTER TABLE public."nodesOnPath" ADD CONSTRAINT "nxmFk" FOREIGN KEY ("nId")
REFERENCES public.edges ("nId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "mxnFk" | type: CONSTRAINT --
-- ALTER TABLE public."nodesOnPath" DROP CONSTRAINT IF EXISTS "mxnFk" CASCADE;
ALTER TABLE public."nodesOnPath" ADD CONSTRAINT "mxnFk" FOREIGN KEY ("pId")
REFERENCES public.path ("pId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "eId1Fk" | type: CONSTRAINT --
-- ALTER TABLE public.interaction DROP CONSTRAINT IF EXISTS "eId1Fk" CASCADE;
ALTER TABLE public.interaction ADD CONSTRAINT "eId1Fk" FOREIGN KEY ("eId1")
REFERENCES public.enzime ("eId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "eId2Fk" | type: CONSTRAINT --
-- ALTER TABLE public.interaction DROP CONSTRAINT IF EXISTS "eId2Fk" CASCADE;
ALTER TABLE public.interaction ADD CONSTRAINT "eId2Fk" FOREIGN KEY ("eId2")
REFERENCES public.enzime ("eId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "cIdFk" | type: CONSTRAINT --
-- ALTER TABLE public.interaction DROP CONSTRAINT IF EXISTS "cIdFk" CASCADE;
ALTER TABLE public.interaction ADD CONSTRAINT "cIdFk" FOREIGN KEY ("cId")
REFERENCES public.compound ("cId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "mnIdfx" | type: CONSTRAINT --
-- ALTER TABLE public."enzPathNode" DROP CONSTRAINT IF EXISTS "mnIdfx" CASCADE;
ALTER TABLE public."enzPathNode" ADD CONSTRAINT "mnIdfx" FOREIGN KEY ("nId")
REFERENCES public.edges ("nId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "mmIdfx_cp" | type: CONSTRAINT --
-- ALTER TABLE public."enzPathNode" DROP CONSTRAINT IF EXISTS "mmIdfx_cp" CASCADE;
ALTER TABLE public."enzPathNode" ADD CONSTRAINT "mmIdfx_cp" FOREIGN KEY ("mId")
REFERENCES public.path ("pId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "mnIdfx_cp1" | type: CONSTRAINT --
-- ALTER TABLE public."enzPathNode" DROP CONSTRAINT IF EXISTS "mnIdfx_cp1" CASCADE;
ALTER TABLE public."enzPathNode" ADD CONSTRAINT "mnIdfx_cp1" FOREIGN KEY ("eId")
REFERENCES public.enzime ("eId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "rIfFk" | type: CONSTRAINT --
-- ALTER TABLE public."subsProd" DROP CONSTRAINT IF EXISTS "rIfFk" CASCADE;
ALTER TABLE public."subsProd" ADD CONSTRAINT "rIfFk" FOREIGN KEY ("rId")
REFERENCES public.reaction ("rId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "rIfFk_cp" | type: CONSTRAINT --
-- ALTER TABLE public."subsProd" DROP CONSTRAINT IF EXISTS "rIfFk_cp" CASCADE;
ALTER TABLE public."subsProd" ADD CONSTRAINT "rIfFk_cp" FOREIGN KEY ("cId")
REFERENCES public.compound ("cId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "enzFk" | type: CONSTRAINT --
-- ALTER TABLE public."enzReac" DROP CONSTRAINT IF EXISTS "enzFk" CASCADE;
ALTER TABLE public."enzReac" ADD CONSTRAINT "enzFk" FOREIGN KEY ("eId")
REFERENCES public.enzime ("eId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "enzFk_cp" | type: CONSTRAINT --
-- ALTER TABLE public."enzReac" DROP CONSTRAINT IF EXISTS "enzFk_cp" CASCADE;
ALTER TABLE public."enzReac" ADD CONSTRAINT "enzFk_cp" FOREIGN KEY ("rId")
REFERENCES public.reaction ("rId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "eidFkp" | type: CONSTRAINT --
-- ALTER TABLE public."enzOnPath" DROP CONSTRAINT IF EXISTS "eidFkp" CASCADE;
ALTER TABLE public."enzOnPath" ADD CONSTRAINT "eidFkp" FOREIGN KEY ("eId")
REFERENCES public.enzime ("eId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "eidFkp_cp" | type: CONSTRAINT --
-- ALTER TABLE public."enzOnPath" DROP CONSTRAINT IF EXISTS "eidFkp_cp" CASCADE;
ALTER TABLE public."enzOnPath" ADD CONSTRAINT "eidFkp_cp" FOREIGN KEY ("pId")
REFERENCES public.path ("pId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "eidFkp" | type: CONSTRAINT --
-- ALTER TABLE public."reacOnPath" DROP CONSTRAINT IF EXISTS "eidFkp" CASCADE;
ALTER TABLE public."reacOnPath" ADD CONSTRAINT "eidFkp" FOREIGN KEY ("rId")
REFERENCES public.reaction ("rId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "eidFkp_cp" | type: CONSTRAINT --
-- ALTER TABLE public."reacOnPath" DROP CONSTRAINT IF EXISTS "eidFkp_cp" CASCADE;
ALTER TABLE public."reacOnPath" ADD CONSTRAINT "eidFkp_cp" FOREIGN KEY ("pId")
REFERENCES public.path ("pId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "eidFkp" | type: CONSTRAINT --
-- ALTER TABLE public."compOnPath" DROP CONSTRAINT IF EXISTS "eidFkp" CASCADE;
ALTER TABLE public."compOnPath" ADD CONSTRAINT "eidFkp" FOREIGN KEY ("cId")
REFERENCES public.compound ("cId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "eidFkp_cp" | type: CONSTRAINT --
-- ALTER TABLE public."compOnPath" DROP CONSTRAINT IF EXISTS "eidFkp_cp" CASCADE;
ALTER TABLE public."compOnPath" ADD CONSTRAINT "eidFkp_cp" FOREIGN KEY ("pId")
REFERENCES public.path ("pId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "rAssFk1" | type: CONSTRAINT --
-- ALTER TABLE public."reactionAssociation" DROP CONSTRAINT IF EXISTS "rAssFk1" CASCADE;
ALTER TABLE public."reactionAssociation" ADD CONSTRAINT "rAssFk1" FOREIGN KEY ("rId")
REFERENCES public.reaction ("rId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "rAssFk2" | type: CONSTRAINT --
-- ALTER TABLE public."reactionAssociation" DROP CONSTRAINT IF EXISTS "rAssFk2" CASCADE;
ALTER TABLE public."reactionAssociation" ADD CONSTRAINT "rAssFk2" FOREIGN KEY ("mainRId")
REFERENCES public."reactionAssociation" ("rId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --

-- object: "nnmFk" | type: CONSTRAINT --
-- ALTER TABLE public."nodeName" DROP CONSTRAINT IF EXISTS "nnmFk" CASCADE;
ALTER TABLE public."nodeName" ADD CONSTRAINT "nnmFk" FOREIGN KEY ("nId")
REFERENCES public.edges ("nId") MATCH FULL
ON DELETE NO ACTION ON UPDATE NO ACTION;
-- ddl-end --


