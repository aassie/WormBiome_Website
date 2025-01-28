#Wormbiome website app
#28/01/2025 - V1.0

cat(file=stderr(),"Starting App\n")

ui = fluidPage(
  id = "main_content",
  tags$script(src = "https://kit.fontawesome.com/a569dc8e47.js"),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@500;700;900&display=swap"),
    tags$link(rel="stylesheet", type="text/css",href="style.css"),
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
    includeHTML("www/ga.html")
    ),

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
    bslib::nav_item(a(href = "https://wormbiome-website.readthedocs.io/en/latest/",
                    target = "_blank",
                    tags$i(class = "fa-solid fa-book"),
                    tags$span("Documentation"))),
    #Contact
    tabPanel(tags$div(
      tags$i(class = "fa-solid fa-envelope"),
      tags$span("Contact")),
      PageContact("PC")),
    #Gene Cart
    tabPanel(tags$div(
      style = "display: inline-flex; align-items: center;",
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
    password = wormreader_password,
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
  
  output$sankeyPlot <- plotly::renderPlotly({
    # Load the saved Plotly Sankey plot
    sankey_plot <- readRDS(url("https://raw.githubusercontent.com/aassie/WormBiome_Website/main/static/home_plot1.RDS"))
    sankey_plot # Return the plot object to render
  })
  
  genelistserv("GL", wbdb, column_names, phylo, utable, nrUTable)
  comparatorserv("Comp", wbdb, column_names, kegg, phylo, p_tree, getPalette, tibtree)
  blastServer("BL", custom_db, wbdb, phylo)
  userGeneCartserv("UGL", utable, wbdb, phylo, nrUTable)
  genseSearchServ("GS", wbdb, column_names, phylo, utable, ugenome, nrUTable)
}

cat(file=stderr(),"Loading time:\n")
cat(file=stderr(),paste((proc.time()-start)[3],"\n"))
app<-shinyApp(ui, server)

