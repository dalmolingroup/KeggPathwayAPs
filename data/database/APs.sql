DROP VIEW IF EXISTS subsProdNames ;
DROP VIEW IF EXISTS fCompound ;
drop VIEW IF EXISTS wNamesSubsProd;
DROP VIEW IF EXISTS wAllNodes;

DROP TABLE IF EXISTS nodebyorgs;
DROP TABLE IF EXISTS nodeMetric;
DROP TABLE IF EXISTS nodes;
DROP TABLE IF EXISTS nodeAlias;
DROP TABLE IF EXISTS reactionAssociation;
DROP TABLE IF EXISTS interaction ;
DROP TABLE IF EXISTS subsProd ;
DROP TABLE IF EXISTS enzReac ;
DROP TABLE IF EXISTS enzOnPath ;
DROP TABLE IF EXISTS reacOnPath ;
DROP TABLE IF EXISTS compOnPath ;
DROP TABLE IF EXISTS reaction ;
DROP TABLE IF EXISTS compound ;
DROP TABLE IF EXISTS enzime ;
DROP TABLE IF EXISTS edges ;
DROP TABLE IF EXISTS path ;
DROP TABLE IF EXISTS fakeEdge;
DROP TABLE IF EXISTS fakeNode;

CREATE TABLE fakeEdge (
	"nextId" integer
);

INSERT INTO fakeEdge VALUES (100000);

CREATE TABLE edges (
	"nId" integer NOT NULL,
	"nName" text NOT NULL,
	"rName" text NOT NULL,
	subs integer NOT NULL,
	prod integer NOT NULL,
	type text NOT NULL,
	reversible integer NOT NULL,
	CONSTRAINT "edgFk" FOREIGN KEY ("nId")
    REFERENCES reaction ("rId")
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

CREATE TABLE reaction (
	"rId" integer NOT NULL,
	"rName" text NOT NULL ,
	"rReversible" integer NOT NULL,
	CONSTRAINT reaction_pk PRIMARY KEY ("rId")
);

INSERT INTO reaction 
VALUES (0, "None", 1);

CREATE TABLE compound (
	"cId" integer NOT NULL,
	"cName" text NOT NULL UNIQUE,
	"cDesc" text,
	CONSTRAINT compound_pk PRIMARY KEY ("cId")

);


CREATE TABLE fakeNode (
	"cId" integer NOT NULL,
	"cName" text NOT NULL ,
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

CREATE TABLE "subsProd" (
	"rId" integer NOT NULL,
	"cId" integer NOT NULL,
	"spType" TEXT NOT NULL,
	secondary integer default 0,
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

CREATE TABLE "reactionAssociation" (
	"rId" integer NOT NULL,
	"mainRId" integer NOT NULL,
	CONSTRAINT "raPk" PRIMARY KEY ("rId")
	FOREIGN KEY ("rId")
    REFERENCES reaction ("rId")
);

CREATE TABLE "nodes" (
	"nId" integer NOT NULL,
	"eName" text NOT NULL,
	"rName" text NOT NULL,
	CONSTRAINT nodes_pk PRIMARY KEY ("nId"),
	CONSTRAINT "nFk" FOREIGN KEY ("nId")
    REFERENCES reaction ("rId") 
);

CREATE TABLE "nodeAlias" (
	"nId" integer NOT NULL,
	"childId" integer NOT NULL,
	type text NOT NULL,
	CONSTRAINT "nnmFk" FOREIGN KEY ("nId")
    REFERENCES reaction ("rId")
);

CREATE TABLE nodemetric (
	"nId" integer NOT NULL,
	"pId" integer NOT NULL,
	"isAP" integer NOT NULL,
	"connectivity" integer NOT NULL,
	"triangles" integer NOT NULL,
	"community" integer NOT NULL,
	"eccentricity" integer NOT NULL,
	"radius" integer NOT NULL,
	"diameter" integer NOT NULL,
	"degree" integer NOT NULL,
	"betweenness" real NOT NULL,
	"clusteringCoef" real NOT NULL,
	"closenessCoef" real NOT NULL,
	"eigenvScore" real NOT NULL,
	"authScore" real NOT NULL,
	"hubScore" real NOT NULL,
	CONSTRAINT nm_pk PRIMARY KEY ("nId","pId"),
	FOREIGN KEY ("nId")
    REFERENCES nodes ("nId"),
    FOREIGN KEY ("pId")
    REFERENCES path ("pId")
);

CREATE TABLE nodebyorgs (
	"nId" integer NOT NULL,
	"pId" integer NOT NULL,
	"org" text NOT NULL,
	CONSTRAINT "nodeByOrgs_pk" PRIMARY KEY ("nId","pId","org"),
	CONSTRAINT "nboFk" FOREIGN KEY ("pId","nId")
    REFERENCES nodemetric ("nId","pId")

);



PRAGMA foreign_keys=OFF;
INSERT INTO reactionAssociation
VALUES (0,0);
PRAGMA foreign_keys=ON;



create VIEW subsProdNames as
select r.rName || ' ' || p.pName as rId, s.spType || ' ' || c.cName as cId
  from subsProd as s inner JOIN
		reaction as r on s.rId = r.rId inner JOIN
		reacOnPath as rp on r.rId = rp.rId INNER JOIN
		path as p on p.pId = rp.pId INNER JOIN
		compound as c on c.cId = s.cId
  where secondary == 0
  order by rId, spType DESC, cId;
  
CREATE VIEW fCompound as
select DISTINCT c1.cId, c1.cName ,p.pId, p.pName
from compound  as c1 INNER JOIN
	compOnPath as c2 on c1.cId = c2.cId INNER JOIN
	path as p on p.pId = c2.pId
where c1.cId not in (select DISTINCT cId from interaction);


create VIEW wNamesSubsProd as
select r.rName, p.pName, s.spType , c.cName as cId
  from subsProd as s inner JOIN
		reaction as r on s.rId = r.rId inner JOIN
		reacOnPath as rp on r.rId = rp.rId INNER JOIN
		path as p on p.pId = rp.pId INNER JOIN
		compound as c on c.cId = s.cId
  where secondary == 0
  order by r.rId, spType DESC, c.cId;
  
create VIEW wAllNodes as
select * from compound
UNION
select * from fakeNode;

CREATE VIEW pathEnzReaction as
SELECT DISTINCT p.pName,p.pId, e.eName, e.eId, r.rName, r.rId
FROM path as p INNER JOIN
	enzOnPath as ep on ep.pId = p.pId INNER JOIN
	enzime as e on e.eId = ep.eId INNER JOIN
	enzReac as er on er.eId = e.eId INNER JOIN
	reaction as r on r.rId = er.rId INNER JOIN
	reacOnPath as rp on rp.pId = p.pId AND 
					rp.rId = r.rId
