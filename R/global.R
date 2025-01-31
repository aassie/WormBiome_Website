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
library(dplyr)
library(DT)

#Dev section
## SQL password to public database
#wormreader_password <- Sys.getenv("KEY1")
wormreader_password=""

## File location
srvloc="/srv/shiny-server/"
#srvloc="../accessory_files/"

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
column_names <- c("Genome", "Contig_name", "WBM_geneID", "gene_cluster_id", "Bakta_ID", 
                  "Bakta_product", "Bakta_start", "Bakta_end", "Bakta_frame", "Bakta_Cazy",
                  "Bakta_COG", "Bakta_EC", "Bakta_Gene", "Bakta_GO", "Bakta_KO", "Bakta_Name", 
                  "Bakta_NCBIFam", "Bakta_NCBIProtein", "Bakta_PFAM", "Bakta_RefSeq", "Bakta_RFAM", 
                  "Bakta_score", "Bakta_SO", "Bakta_strand", "Bakta_type", "Bakta_UniParc", 
                  "Bakta_UniRef", "Bakta_VFDB", "Bakta_BlastRules", "contig_Bakta", "gapseq_ID",
                  "gapseq_strand", "gapseq_start", "gapseq_end", "gapseq_frame", "gapseq_BiocycRxn", 
                  "gapseq_SeedID", "gapseq_substances", "gapseq_tc", "gapseq_type", "contig_gapseq",
                  "IMG_ID", "IMG_product", "IMG_start", "IMG_end", "IMG_frame", "IMG_cog", "IMG_ko",
                  "IMG_pfam", "IMG_score", "IMG_signalp", "IMG_smart", "IMG_strand", "IMG_superfam",
                  "IMG_tigrfam", "IMG_tmhmm", "IMG_type", "contig_IMG", "PATRIC_ID", "PATRIC_type", 
                  "PATRIC_product", "PATRIC_strand", "PATRIC_start", "PATRIC_end", "PATRIC_frame", 
                  "PATRIC_pathID", "PATRIC_Pathway", "PATRIC_score", "PATRIC_SPclassification", 
                  "PATRIC_SPproperty", "PATRIC_class", "PATRIC_subclass", "PATRIC_subsystem", 
                  "PATRIC_superclass", "contig_PATRIC", "Prokka_ID", "Prokka_type", "Prokka_product", 
                  "Prokka_strand", "Prokka_start", "Prokka_end", "Prokka_COG", "Prokka_EC_number", 
                  "Prokka_frame", "Prokka_gene", "Prokka_KO", "Prokka_score", "contig_Prokka")
dbDisconnect(wbdb)

print("Loading phylogenies")
phylo<-read_tsv(paste0(srvloc,"data/Bacteria_Phylogeny.txt"),show_col_types = FALSE) %>% filter( ID %in% ugenome)

print("Loading metadata")
kegg<-read_csv(paste0(srvloc,"data/Kegg.metadata.csv"),show_col_types = FALSE) %>% select(!`...1`)
getPalette = colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))
#Load the tree
print("Loading tree")
tree<-read.tree(paste0(srvloc,"data/Phylogenomic.tre"))
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