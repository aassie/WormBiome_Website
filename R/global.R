cat(file=stderr(), "Loading Globals\n")

start<-proc.time()

library(shiny)
library(shinyjs)
library(reactable)
library(tidyverse)
library(ggtree)
library(treeio)
library(DBI)
require(XML)
library(plyr)
library(dplyr)
library(DT)

#public database
wormreader_password <- ""

custom_db<-"All.ref"
print("Loading database")

#First connection to database
wbdb <- dbConnect(
  RMariaDB::MariaDB(),
  host = "127.0.0.1",
  port = '3306',
  user = "wormreader",
  password = wormreader_password,
  dbname = "wormbiome"
)
#Get genome names
ugenome=pull((dbGetQuery(wbdb, "SELECT DISTINCT Genome FROM wb")))
ugenes=pull((dbGetQuery(wbdb, "SELECT COUNT(DISTINCT WBM_geneID) FROM wb")))
# Get wb column column names
column_names <- dbGetQuery(wbdb, sprintf("SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = '%s' AND TABLE_NAME = '%s'", "wormbiome", "wb"))
dbDisconnect(wbdb)

print("Loading phylogenies")
phylo<-read_tsv("/srv/shiny-server/data/Bacteria_Phylogeny.txt",show_col_types = FALSE) %>% filter( ID %in% ugenome)

print("Loading metadata")
kegg<-read_csv("/srv/shiny-server/data/Kegg.metadata.csv",show_col_types = FALSE) %>% select(!`...1`)
getPalette = colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))
#Load the tree
print("Loading tree")
tree<-read.tree("/srv/shiny-server/data/Phylogenomic.tre")
#Reroot tree
trda2 <- root(tree, outgroup = "GCF_000011365.1_Bradyrhizobium_diazoefficiens_USDA110", edgelabel = TRUE)
#Make a circular tree
p_tree <- ggtree(trda2, layout = 'circular', branch.length='none')
#Get x
tibtree <- as_tibble(tree)

ImpColumn=c("Bakta_Cazy",  "Bakta_EC",  "Bakta_Gene",  "Bakta_GO",
            "Bakta_ID",  "Bakta_IS",  "Bakta_KEGG",  "Bakta_KO",
            "Bakta_Name",  "Bakta_NCBIFam",  "Bakta_NCBIProtein",  "Bakta_PFAM",
            "Bakta_product",  "Bakta_RefSeq",  "Bakta_UniParc",  "Bakta_UniRef",
            "Bakta_VFDB",  "PATRIC_product",  "PATRIC_Pathway",  "PATRIC_subclass",  
            "PATRIC_subsystem",   "PATRIC_superclass",  "Prokka_COG",  "Prokka_KO",
            "Prokka_EC_number",  "Prokka_product",  "IMG_superfam",  "IMG_tigrfam",
            "IMG_tmhmm",  "IMG_type",  "IMG_ko",  "IMG_pfam",
            "IMG_cog",  "IMG_product",  "IMG_signalp")