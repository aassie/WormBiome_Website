start<-proc.time()

library(shiny)
library(shinyjs)
library(reactable)
library(tidyverse)
library(ggtree)
library(treeio)
library(DBI)

readRenviron("~/.Renviron")

custom_db<-"All.ref"
print("Loading database")

#First connection to database
wbdb <- dbConnect(
  RMariaDB::MariaDB(),
  host = "127.0.0.1",
  port = '3306',
  user = "wormreader",
  password = Sys.getenv("KEY1"),
  dbname = "wormbiome"
)
#Get genome names
ugenome=pull((dbGetQuery(wbdb, "SELECT DISTINCT Genome FROM wb")))
ugenes=pull((dbGetQuery(wbdb, "SELECT COUNT(DISTINCT WBM_geneID) FROM wb")))
# Get wb column column names
column_names <- dbGetQuery(wbdb, sprintf("SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_SCHEMA = '%s' AND TABLE_NAME = '%s'", "wormbiome", "wb"))
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

ImpColumn=c("Bakta_Cazy",  "Bakta_EC",  "Bakta_Gene",  "Bakta_GO",
            "Bakta_ID",  "Bakta_IS",  "Bakta_KEGG",  "Bakta_KO",
            "Bakta_Name",  "Bakta_NCBIFam",  "Bakta_NCBIProtein",  "Bakta_PFAM",
            "Bakta_product",  "Bakta_RefSeq",  "Bakta_UniParc",  "Bakta_UniRef",
            "Bakta_VFDB",  "PATRIC_product",  "PATRIC_Pathway",  "PATRIC_subclass",  
            "PATRIC_subsystem",   "PATRIC_superclass",  "Prokka_COG",  "Prokka_KO",
            "Prokka_EC_number",  "Prokka_product",  "IMG_superfam",  "IMG_tigrfam",
            "IMG_tmhmm",  "IMG_type",  "IMG_ko",  "IMG_pfam",
            "IMG_cog",  "IMG_product",  "IMG_signalp")

ui = fluidPage(
  id = "main_content",
  tags$script(src = "https://kit.fontawesome.com/a569dc8e47.js"),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@500;700;900&display=swap"),
    tags$link(rel="stylesheet", type="text/css",href="style.css"),
    tags$script(type="text/javascript", src = "busy.js"),
    tags$script(type="text/javascript",src="https://cdn-cookieyes.com/client_data/1f11ebb827a1be59b9f748b2/script.js",id="cookieyes"),
    tags$style(HTML(".navbar-default .navbar-brand {color: cyan;}
                    .navbar-default .navbar-brand:hover {color: blue;}
                    .navbar { background-color: #7a9ccc;}
                    .navbar-default .navbar-nav > li > a {color:white; display: flex;align-items: center;}
                    .navbar-nav > li > a, .navbar-brand {  padding-top:4px !important;   padding-bottom:0 !important; padding-left:0px;  padding-right:15px ;  margin-right:auto;  height: 60px;  width:100%;  display: flex;   align-items: center;}
                    .navbar-default .navbar-nav > .active > a,
                    .navbar-default .navbar-nav > .active > a:focus,
                    .navbar-default .navbar-nav > .active > a:hover {color: black;background-color: #c6d7f4;}
                    .navbar-default .navbar-nav > li > a:hover {color: white;background-color:#415368;text-decoration:underline;}"
    )),
    tags$script(HTML("
    <!-- Google tag (gtag.js) -->
    <script async src='https://www.googletagmanager.com/gtag/js?id=G-140FXSDHXK'></script>
    <script>
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());
    
      gtag('config', 'G-140FXSDHXK');
    </script>
      "))),

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
    genseSearchUI("GS",phylo,utable,ugenome)),
    #Annotation Browser
    tabPanel(tags$div(
      tags$i(class = "fa-solid fa-dna"),
      tags$span("Annotations Browser")
    ), 
    value = "tab4",
    geneListUI("GL",phylo,ugenome)),
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
      comparatorUI("Comp",phylo,kegg,ugenome)),
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
      userGeneCartUI("UGL",phylo,utable)
    )
  )
)

server <- function(input, output, session) {
  wbdb <- pool::dbPool(
    drv = RMariaDB::MariaDB(),
    host = "127.0.0.1",
    port = '3306',
    user = "wormreader",
    password = Sys.getenv("KEY1"),
    dbname = "wormbiome"
  )
  
  onStop(function() {
    pool::poolClose(wbdb)
  })
  
  print(date())
  
  # Initialize nrUTable as a reactiveVal
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
  
  utable <- reactiveValues(x=tibble(
    WBM_geneID = character(),  # Empty character column
    Genome = character(),      # Empty character column
    Bakta_ID = character(),    # Add other columns as needed
    Contig_name = character(), # Empty character column
    Bakta_product = character()# Add other columns as needed
  ))
  output$panel=renderUI(input$tabset)
  observeEvent(input$controller, {
    updateTabsetPanel(session, "hidden_tabs", selected = paste0("panel", input$controller))
  })
  
  #Dynamic Markdown reading
  
  output$newsMarkdown <- renderUI({
    # Fetch the markdown content on GitHub raw markdown URL
    markdown_url <- "https://raw.githubusercontent.com/aassie/WormBiome_Website/main/static/News.md"
    response <- httr::GET(markdown_url)
    if (httr::status_code(response) == 200) {
      # Render the markdown in the UI
      includeMarkdown(httr::content(response, "text"))
    } else {
      # Show an error message if the markdown can't be fetched
      h4("Unable to fetch the markdown from GitHub.")
    }
  })
  
  genelistserv("GL", wbdb, column_names, phylo, utable, nrUTable)
  comparatorserv("Comp", wbdb, column_names, kegg, phylo, p_tree, getPalette, tibtree)
  blastServer("BL", custom_db, wbdb, phylo)
  userGeneCartserv("UGL", utable, wbdb, phylo, nrUTable)
  genseSearchServ("GS", wbdb, column_names, phylo, utable, ugenome, nrUTable)
}

print(proc.time()-start)
app<-shinyApp(ui, server)
