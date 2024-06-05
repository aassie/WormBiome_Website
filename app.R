start<-proc.time()

library(shiny)
library(shinyjs)
library(reactable)
library(tidyverse)
library(ggtree)
library(treeio)
library(RMySQL)

custom_db<-"All.ref"
print("Loading database")
textOutput("Loading database")
wbdb <- dbConnect(
  RMariaDB::MariaDB(),
  host = "localhost",
  user = "wormaster",
  password = "Adrien-973",
  dbname = "wormbiome"
)

ugenome=pull((dbGetQuery(wbdb, "SELECT DISTINCT Genome FROM wb")))
dbDisconnect(wbdb)

print("Loading phylogenies")
phylo<-read_tsv("./data/Bacteria_Phylogeny.txt",show_col_types = FALSE) %>% filter( ID %in% ugenome)

print("Loading metadata")
kegg<-read_csv("./data/Kegg.metadata.csv",show_col_types = FALSE) %>% select(!`...1`)
getPalette = colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"))
#Load the tree
print("Loading tree")
tree<-read.tree("./data/Phylogenomic.tre")
#Reroot tree
trda2 <- root(tree, outgroup = "GCF_000011365.1_Bradyrhizobium_diazoefficiens_USDA110", edgelabel = TRUE)
#Make a circular tree
p_tree <- ggtree(trda2, layout = 'circular', branch.length='none')
#Get x
tibtree <- as_tibble(tree)

ui = fluidPage(
  id = "main_content",
  tags$script(src = "https://kit.fontawesome.com/a569dc8e47.js"),
  tags$head(
    tags$link(rel="stylesheet", type="text/css",href="style.css"),
    tags$script(type="text/javascript", src = "busy.js"),
    tags$script(type="text/javascript",src="https://cdn-cookieyes.com/client_data/1f11ebb827a1be59b9f748b2/script.js",id="cookieyes"),
    tags$style(HTML("
                    .navbar-default .navbar-brand {color: cyan;}
                    .navbar-default .navbar-brand:hover {color: blue;}
                    .navbar { background-color: #7a9ccc;}
                    .navbar-default .navbar-nav > li > a {color:white; display: flex;align-items: center;}
                    .navbar-nav > li > a, .navbar-brand {  padding-top:4px !important;   padding-bottom:0 !important; padding-left:0px;  padding-right:15px ;  margin-right:auto;  height: 60px;  width:100%;  display: flex;   align-items: center;}
                    .navbar-default .navbar-nav > .active > a,
                    .navbar-default .navbar-nav > .active > a:focus,
                    .navbar-default .navbar-nav > .active > a:hover {color: black;background-color: #c6d7f4;}
                    .navbar-default .navbar-nav > li > a:hover {color: white;background-color:#415368;text-decoration:underline;}
                    "
    ))),

  navbarPage(
    collapsible = TRUE,
    windowTitle = "WormBiome",
    tags$img(src ="WL.png", height="50 px"),
    id = "tabset",
    #Home
    tabPanel(tags$div(
      tags$i(class = "fa-solid fa-home"),
      tags$span("Home")
    ), home),
    #Genome Metadata
    tabPanel(title=tags$div(
               tags$i(class = "fa-solid fa-bacteria"),
               tags$span("Microbial genomes")
               ),
             value = "tab1",
             bacteriaListUI("BL")),
    #Sequence search
    tabPanel(title=tags$div(
      tags$i(class = "fa-solid fa-magnifying-glass"),
      tags$span("Gene Search")
    ),
    value = "tab2",
    genseSearchUI("GS",wbdb,phylo,utable,ugenome)),
    #Annotation Browser
    tabPanel(tags$div(
      tags$i(class = "fa-solid fa-dna"),
      tags$span("Annotations Browser")
    ), 
    value = "tab4",
    geneListUI("GL",wbdb,phylo,ugenome)),
    #Tool Menu
    navbarMenu(
      title = tags$div(
      tags$i(class = "fa-solid fa-wrench"),
      tags$span("Tools")),
      # Feautre comparator
      tabPanel(tags$div(
        HTML("<i class=\"fas fa-dna\" data-fa-transform=\"right-6\"></i>
              <i class=\"fas fa-dna\"  data-fa-transform=\"left-6\" style=\"color:#8c8c8c\"></i>"),
        tags$span("Compare Features")
      ),
      value = "tab5",
      comparatorUI("Comp",wbdb,phylo,kegg,tree,p_tree,ugenome)),
      #Blast Tool
      tabPanel(tags$div(
        HTML("<i class=\"fas fa-dna\" data-fa-transform=\"right-6\"></i>
              <i class=\"fas fa-magnifying-glass\"  data-fa-transform=\"left-6\" style=\"color:#8c8c8c\"></i>"),
        tags$span("Genome Blast")),
        value="tab3",
        blastUI("BL",custom_db,wbdb,phylo,ugenome))),
    #Documentation
    tabPanel(tags$div(
      tags$i(class = "fa-solid fa-book"),
      tags$span("Documentation")),
      PageDoc),
    #Download
    tabPanel(tags$div(
      tags$i(class = "fa-solid fa-download"),
      tags$span("Download")), ),
    #Contact
    tabPanel(tags$div(
      tags$i(class = "fa-solid fa-envelope"),
      tags$span("Contact")),
      PageContact("PC")),
    #Gene Cart
    tabPanel(tags$div(
      tags$i(class = "fa-solid fa-cart-shopping"),
      uiOutput(NS("GL","uCartLabel"))),
      userGeneCartUI("UGL",wbdb,phylo,utable)
    )
  )
)

server <- function(input, output, session) {
  nrUTable <- reactiveValues(nrow = 0L)
  observeEvent(input$Mbutton, {
    updateTabsetPanel(session, inputId = "tabset", selected = "tab1")
  })
  
  observeEvent(input$Sbutton, {
    updateTabsetPanel(session, inputId = "tabset", selected = "tab2")
  })
  
  observeEvent(input$Bbutton, {
    updateTabsetPanel(session, inputId = "tabset", selected = "tab3")
  })
  
  observeEvent(input$Gbutton, {
    updateTabsetPanel(session, inputId = "tabset", selected = "tab4")
  })
  
  observeEvent(input$Cbutton, {
    updateTabsetPanel(session, inputId = "tabset", selected = "tab5")
  })
  
  utable <- reactiveValues(x=tibble())
  #load_data()
  output$panel=renderUI(input$tabset)
  observeEvent(input$controller, {
    updateTabsetPanel(session, "hidden_tabs", selected = paste0("panel", input$controller))
  })

  #Genelist
  genelistserv("GL",wbdb, phylo,utable,nrUTable)
  comparatorserv("Comp", wbdb, kegg, phylo,p_tree,getPalette,tibtree)
  blastServer("BL",custom_db,wbdb,phylo)
  userGeneCartserv("UGL",utable,wbdb,phylo,nrUTable)
  genseSearchServ("GS",wbdb, phylo,utable,ugenome,nrUTable)
}

print(proc.time()-start)
app<-shinyApp(ui, server)
