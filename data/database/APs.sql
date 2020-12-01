DROP TABLE IF EXISTS "nodesOnPath" ;
DROP TABLE IF EXISTS interaction ;
DROP TABLE IF EXISTS "enzPathNode" ;
DROP TABLE IF EXISTS "subsProd" ;
DROP TABLE IF EXISTS "enzReac" ;
DROP TABLE IF EXISTS "enzOnPath" ;
DROP TABLE IF EXISTS "reacOnPath" ;
DROP TABLE IF EXISTS "compOnPath" ;
DROP TABLE IF EXISTS reaction ;
DROP TABLE IF EXISTS compound ;
DROP TABLE IF EXISTS enzime ;
DROP TABLE IF EXISTS nodes ;
DROP TABLE IF EXISTS path ;


CREATE TABLE nodes (
	"nId" integer NOT NULL,
	"nName" text,
	CONSTRAINT nodes_pk PRIMARY KEY ("nId")

);

CREATE TABLE enzime (
	"eId" integer NOT NULL,
	"eName" text  NOT NULL UNIQUE,
	"eLabel" text,
	CONSTRAINT entry_pk PRIMARY KEY ("eId")

);

CREATE TABLE path (
	"pId" integer NOT NULL,
	"pName" text NOT NULL UNIQUE,
	"pOrganism" text,
	"pImage" text,
	"pLink" text,
	CONSTRAINT map_pk PRIMARY KEY ("pId")

);

CREATE TABLE "nodesOnPath" (
	"nId" integer,
	"pId" integer,
	x integer,
	y integer,
	FOREIGN KEY ("nId")
    REFERENCES nodes ("nId"),
    FOREIGN KEY ("pId")
    REFERENCES path ("pId")
);

CREATE TABLE reaction (
	"rId" integer NOT NULL,
	"rName" text NOT NULL ,
	"rReversible" integer NOT NULL,
	CONSTRAINT reaction_pk PRIMARY KEY ("rId")

);

CREATE TABLE compound (
	"cId" integer NOT NULL,
	"cName" text NOT NULL UNIQUE,
	"cDesc" text,
	CONSTRAINT compound_pk PRIMARY KEY ("cId")

);

CREATE TABLE interaction (
	"eId1" integer NOT NULL,
	"eId2" integer NOT NULL,
	"cId" integer NOT NULL,
	FOREIGN KEY ("eId1")
    REFERENCES enzime ("eId"),
    FOREIGN KEY ("eId2")
    REFERENCES enzime ("eId"),
    FOREIGN KEY ("cId")
    REFERENCES compound ("cId")
);

CREATE TABLE "enzPathNode" (
	"nId" integer NOT NULL,
	"mId" integer NOT NULL,
	"eId" integer NOT NULL,
	FOREIGN KEY ("nId")
    REFERENCES nodes ("nId"),
    FOREIGN KEY ("mId")
    REFERENCES path ("pId")
);


CREATE TABLE "subsProd" (
	"rId" integer NOT NULL,
	"cId" integer NOT NULL,
	"spType" TEXT NOT NULL,
	FOREIGN KEY ("rId")
    REFERENCES reaction ("rId"),
    FOREIGN KEY ("cId")
    REFERENCES compound ("cId") 
);

CREATE TABLE "enzReac" (
	"eId" integer NOT NULL,
	"rId" integer NOT NULL,
	FOREIGN KEY ("eId")
    REFERENCES enzime ("eId"),
    FOREIGN KEY ("rId")
    REFERENCES reaction ("rId")
);

CREATE TABLE "enzOnPath" (
	"eId" integer NOT NULL,
	"pId" integer NOT NULL,
	x integer NOT NULL,
	y integer NOT NULL,
	FOREIGN KEY ("eId")
    REFERENCES enzime ("eId"),
    FOREIGN KEY ("pId")
    REFERENCES path ("pId")
);


CREATE TABLE "reacOnPath" (
	"rId" integer NOT NULL,
	"pId" integer NOT NULL,
	FOREIGN KEY ("rId")
    REFERENCES reaction ("rId"),
    FOREIGN KEY ("pId")
    REFERENCES path ("pId") 
);

CREATE TABLE "compOnPath" (
	"cId" integer NOT NULL,
	"pId" integer NOT NULL,
	x integer NOT NULL,
	y integer NOT NULL,
	FOREIGN KEY ("cId")
    REFERENCES compound ("cId"),
    FOREIGN KEY ("pId")
    REFERENCES path ("pId") 
);
