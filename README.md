# KEGG PATHWAY BOTTLENECK DETECTION
Pipeline for detection of articulation points in KEGG (Kyoto Encyclopedia of Genes and Genomes) metabolic pathways.

## What's new about this?

Until now there isn't a global way to detect the weaknesses in metabolic pathways. This project'll provide biomarkers to appoint bottlenecks that potentially could be applied in various medical and pharmaceutical contexts to identify key proteins.

## Method

In order to gather the data and processed it, the following pipeline is applied:

#### DATA GATHERING
This work uses KEGGREST R/Bioconductor package to obtain a reference pathway of interest from the REST API provided by KEGG; 
The retrieved KGML (KEGG XML) file is processed and transformed into a graph object.

#### DATA PROCESSING 
The igraph R/CRAN package is used to extract features like communities, betweenness, closeness, and others from each enzyme of the pathway; 
The presence of enzymes is accounted for each specie in a pathway; 
The bottleneck calculation is based on concepts related to graph theory.

#### STATISTICS 
Aiming to identifying if the bottlenecks are essential or not, a study of significance will be applied based on its frequency compared with non-bottlenecks enzymes.

#### DATA VISUALIZATION 
The visNetwork R/CRAN package will be used to provide the visualization of the pathways applying the previously calculated properties for each node.

## Project structure:

This project folders are organized in the following way:

**dictionaries** => Dictionaries used as support to the pipelines.

**output** => The data generate by the project pipeline.

**R** => Contains the main R scripts and auxiliary functions.

**sql_scrpts** => SQL scripts to handle data stored into Mysql database.

## Team:

* Igor Brandão ( igorabrandao@gmail.com )
* Diego Arthur ( arthur.vinx@gmail.com )
* Alice Câmara ( alicecamara@ufrn.edu.br )
* Leonardo Campos ( leofields@gmail.com )
* Clóvis Reis ( cfreis@ufrn.edu.br )
* Rodrigo Dalmolin ( dalmolin_r@yahoo.com.br )

## Changelog:

In order to update the `CHANGELOG.md`, install the extension `auto-changelog`: [**npm install -g auto-changelog**] and after run the following command:

    auto-changelog --template keepachangelog
