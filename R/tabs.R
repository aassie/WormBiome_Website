home=fluidPage(
  fluidRow(class = "MainRow",
     column(8,
        column(12,  h1("Welcome to Wormbiome", style="color:white;font-family: 'Montserrat', sans-serif; font-weight: 500;"),
               style="background-color:#29313f;border-radius: 10px;box-shadow: -5px 5px #e2e2e2;"),
        column(12, p("Wormbiome is a database dedicated to collecting genomes associated with  C. elegans! This database is a comprehensive collection of bacterial genomes related to the C. elegans nematode. This powerful model organism is naturally colonized by diverse bacteria, making it an ideal system for studying the interactions between host and microbes. With this database, researchers can access the genetic information of these associated bacteria and use it to understand the role of the microbiome in shaping host physiology and physiology and its implications for human health. Explore the relationships between different bacterial species, and track the spread of antibiotic-resistant strains. Dive into the metabolic pathways and biochemistry of the organisms and discover new applications in biotechnology and synthetic biology. Join us in unraveling the secrets of the C. elegans microbiome, and unlock the potential of this powerful model organism."),
               style="padding-top:1em")
     ),
     column(4,
            column(12,h1("News", style="color:white;font-family: 'Montserrat', sans-serif; font-weight: 500;"),
               style="background-color:#ea8b8b;border-radius: 10px;box-shadow: -5px 5px #e2e2e2;"),
            column(12,uiOutput("newsMarkdown"),
               style="padding-top:1em")
     ),
  ),
  fluidRow(class = "NumberRow",
           column(6,
                  column(12,  h1("In the Database", style="color:white;font-family: 'Montserrat', sans-serif; font-weight: 500;"),
                         style="background-color:#29313f;border-radius: 10px;box-shadow: -5px 5px #e2e2e2;")),
           column(12,  
             div(style = "padding-top:1em;display: flex; justify-content: center; align-items: center; height: 100%;", 
               column(3,
                 div(p(paste(length(ugenome), "Genomes")),
                     style = "font-size: 24px; text-align: center; 
                     font-family: 'Montserrat', sans-serif; font-weight: 900; 
                     padding: 10px; border-radius: 10px; box-shadow: -5px 5px #e2e2e2;
                     margin: 0; height: 50%;"
                 )),
               column(3,
                 div(p(paste(length(unique(phylo$Genus)), "Genera")),
                     style = "font-size: 24px; text-align: center; font-family: 'Montserrat', sans-serif; font-weight: 900; padding: 10px; border-radius: 10px; box-shadow: -5px 5px #e2e2e2;margin: 0; height: 50%;"
                 )),
               column(3,
                 div(p(paste(ugenes, "Annotations")),
                     style = "font-size: 24px; text-align: center; font-family: 'Montserrat', sans-serif; font-weight: 900; padding: 10px; border-radius: 10px; box-shadow: -5px 5px #e2e2e2;margin: 0; height: 50%;"
                     ))
             ))
  ),
  br(),
  fluidRow(class = "FunctionRow",
     column(4,style="height:15em",
        column(12,
               h3("Browse the collection", style="color:white;font-family: 'Montserrat', sans-serif; font-weight: 500;"),
               style="background-color:#29313f;border-radius: 10px;box-shadow: -5px 5px #e2e2e2;"),
        column(12, 
               p("Look for the available bacteria in the collection and their associated metadata"),
               actionButton("Mbutton","Collection"),
               style="padding-top:1em")
   ),
     column(4,style="height:15em",
        column(12,
               h3("Search Gene", style="color:white;font-family: 'Montserrat', sans-serif; font-weight: 500;"),
               style="background-color:#29313f;border-radius: 10px;box-shadow: -5px 5px #e2e2e2;"),
        column(12,
               p("Look for a specific gene and its distribution accross the available bacteria"),
               actionButton("Sbutton", "Name search"),
               actionButton("Bbutton", "Sequence search"),
               style="padding-top:1em")
     ),
     column(4,style="height:15em",
        column(12,
               h3("Browse Genomes", style="color:white;font-family: 'Montserrat', sans-serif; font-weight: 500;"),
               style="background-color:#29313f;border-radius: 10px;box-shadow: -5px 5px #e2e2e2;"),
        column(12,
               p("Explore the genetic annotations for one or a group of bacteria"),
               tags$div(
                 actionButton("Gbutton", "Browse Genome"),br(),
                 actionButton("Cbutton", "Compare Genomes"),
                 tags$style(type="text/css","align:center")
               ),
               style="padding-top:1em")
     )
  ),
)


PageDoc=fluidPage( 
  fluidRow(class = "PageDoc",
           column(12, h2("Documentation", style="color:white;font-family: 'Montserrat', sans-serif; font-weight: 500;"), 
                  style="background-color:#29313f;border-radius: 10px;box-shadow: -5px 5px #e2e2e2;"),
           column(12,uiOutput("Documentation"),
                  style="padding-top:1em"),
           tags$head(tags$style(".NewsRow{padding-top: 25px}"))
  )
)

PageContact=function(ida){
  fluidPage( 
    fluidRow(
      tags$iframe(
        class="gform-embed", 
        src="https://docs.google.com/forms/d/e/1FAIpQLSeR9yXyrXGHpPWJGFZuMRfKjnZi96O0TU6VO-20sCtYpmA4Fw/viewform?embedded=true",
        frameborder="0",
        onmousewheel="", 
        width="100%", 
        height="1200",
        marginheight="0",
        marginwidth="0",
        style="background: transparent",
        seamless = "seamless"
      )
    )
  )
}

geneListUI <- function(ida, phylo, ugenome) {
  fluidPage(
    tags$head(
      tags$style(HTML("
        .custom-button {
          width: 100%;
        }"))
    ),
    fluidRow(
      column(2,
             div(selectInput(NS(ida, "AnnotDB"), "Annotation database:",
                             c("Bakta" = "Bakta",
                               "Prokka" = "Prokka",
                               "IMG" = "IMG",
                               "PATRIC" = "PATRIC")
             )),
             div(selectizeInput(NS(ida, "genome"), "Genome:",
                                choices = ugenome,
                                options = list(
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }')
                                ), multiple = T
             )),
             div(style = "display: flex; flex-direction: column; height: 100%; justify-content: space-between;",
                 div(selectizeInput(NS(ida, "GLTL"), "Taxonomic filtering (optional):",
                                    choices = colnames(phylo)[-c(1, 2, 8)],
                                    options = list(
                                      placeholder = 'Please select an option below',
                                      onInitialize = I('function() { this.setValue("Phyla"); }')
                                    ),
                                    multiple = FALSE
                 )),
                 div(uiOutput(NS(ida, "GLTL.second")))
             ),
             div(style = "display: flex; flex-direction: column;",
                 actionButton(NS(ida, "selectGene"), "Select Gene(s)", class = "custom-button"),
                 actionButton(NS(ida, "resetGene"), "Clear Gene(s)", class = "custom-button"),
                 div(style="padding-top:10px",
                   downloadButton(NS(ida, "downloadSData"), "Download selected", class = "custom-button"),
                   downloadButton(NS(ida, "downloadAData"), "Download All", class = "custom-button")
                 )
             ),
             h4("Select columns to display:"),
             div(style = "height: 200px; overflow-y: scroll;padding-top:10px",  
                 uiOutput(NS(ida, "column_selector"))
             )
      ),
      column(10,
             reactableOutput(NS(ida, "data"))
      )
    )
  )
}


genseSearchUI <- function(ida, phylo, utable, ugenome) {
  fluidPage(
    fluidRow(
      h1("Search settings")
    ),
    fluidRow(
      # First Row: Inputs
      column(
        width = 4,
        div(style = "display: flex; flex-direction: column; height: 100%; justify-content: space-between;",
            textInput(NS(ida, "geneSearch"), "Text Search", "Search"))
      ),
      column(
        width = 4,
        p(""),
        div(style = "display: flex; flex-direction: column; height: 100%; justify-content: space-between;",
            actionButton(NS(ida, "actionSearch"), "Search"))
      ),
      column(width = 4),
      column(width = 4)
    ),
    tags$hr(),
    fluidRow(
      # Second Row: Filters
      column(
        width = 4,
        div(style="display: flex; flex-direction: column; height: 100%; justify-content: space-between;",
            #Debug
            #div(p("Checking: "), verbatimTextOutput(NS(ida, "value"))),
            div(selectizeInput(NS(ida, "ColumnFilter"), "Filter by Column (Optional)",
                               choices = c("All", column_names),
                               multiple = TRUE,
                               selected = "All"
                               )
                ),
            div(style = "display: flex; flex-direction: column; height: 100%; justify-content: space-between;",
                tags$b("Result Summary:"),
                uiOutput(NS(ida, "uSearchDesc"))
            )
            )
      ),
      column(
        width = 4,
        div(style = "display: flex; flex-direction: column; height: 100%; justify-content: space-between;",
            div(selectizeInput(NS(ida, "TL"), "Taxonomic filtering (optional):",
                               choices = colnames(phylo)[-c(1, 2, 8)],
                               options = list(
                                 placeholder = 'Please select an option below',
                                 onInitialize = I('function() { this.setValue("Phyla"); }')),
                               multiple = FALSE)
                ),
            div(uiOutput(NS(ida, "TL.second"))),
            div(selectizeInput(NS(ida, "Sgenome"), "Filter by Genome (Optional)",
                               choices = c("All", ugenome),
                               multiple = TRUE,
                               selected = "All")
                )
            )
      ),
      column(width = 4),
      column(
        width = 4,
        div(style = "height: 200px; overflow-y: scroll;",
            tags$b("Result Columns to display:"),
            uiOutput(NS(ida, "column_selector"))
      ))
    ),
    tags$hr(),
    fluidRow(
      # Third Row: Actions
      column(width = 12,
             div(style = "display: flex; justify-content: center;align-items: center",
                 actionButton(NS(ida, "selectGene"), "Select Gene(s)"),
                 actionButton(NS(ida, "resetGene"), "Clear Gene(s)"),
                 downloadButton(NS(ida, "downloadSData"), "Download selected"),
                 downloadButton(NS(ida, "downloadAData"), "Download All"))
             )
            ),
    fluidRow(h1("Results")
             ),
    fluidRow(
      # Results Table
      div(
        style = "width: 100%; overflow-x: auto;",
        reactableOutput(NS(ida, "STable"))
      )
    )
  )
}

userGeneCartUI<-function(ida,phylo,utable){
  fluidPage( 
    fluidRow(column(12,
                    div(style = "display: flex; justify-content: center;align-items: center",
                    uiOutput(NS(ida,"uCartDescription")),
                    actionButton(NS(ida,"resetGene"), "Clear Gene(s)"),
                    downloadButton(NS(ida,"downloadSData"), "Download selected"),
                    downloadButton(NS(ida,"downloadAData"), "Download All"))
             )),
    #Column Filter
    fluidRow(
      column(6,
             div(selectInput(NS(ida,"UCAnnotDB"), "Annotation database:",
                             c("Bakta" = "Bakta",
                               "Prokka" = "Prokka",
                               "IMG" = "IMG",
                               "PATRIC" = "PATRIC"))),
      ),
      column(6,
            h4("Select columns to display:"),
            div(style = "height: 200px; overflow-y: scroll;",
                 uiOutput(NS(ida, "UCcolumn_selector"))
             ))
    ),
    #Table
    fluidRow(column(12,
                    reactableOutput(NS(ida,"userGeneCart"))
             )),
    hr(),
    fluidRow(h3("Taxonomic Overview"),
             column(12,
             column(4,
                    h4("Display option"),
                    selectInput(NS(ida,"GCTaxLevel"), "Taxonomic Level:",
                                c(colnames(phylo)[3:7],"Genome"))
                    )),
             column(4,
                    reactableOutput(NS(ida,"GCTable1"))
                     ),
             column(8,
                    plotly::plotlyOutput(NS(ida,"GCoverview1"))
             )),
     fluidRow(h3("Annotation Categories Overview"),
              column(12,
              column(4,
                     h4("Display option"),
                     selectInput(NS(ida,"GCgpby"), "Group by:",
                                 c(ImpColumn,"Genome"),selected = "Bakta_KO")
              )),
              column(12,
              column(4,
                       reactableOutput(NS(ida,"GCTable2"))
              ),
              column(8,
                     plotly::plotlyOutput(NS(ida,"GCoverview2"))
                     )
             )))
}

comparatorUI <- function(ida, phylo, kegg, ugenome) {
  ns <- NS(ida)
  fluidPage(
    fluidRow(
      column(4,
        # Annotation track
        div(
          selectInput(
            ns("anot"), 
            "Annotation track:",
            choices = c("Bakta" = "Bakta")
          )
        ),
        # Selection Mode (Taxonomy or Custom Group)
        div(
          radioButtons(
            ns("selection_mode"),
            "Select Genomes By:",
            choices = c("Taxonomy" = "taxonomy", "Custom Group" = "custom"),
            inline = TRUE
          )
        ),
        # Taxonomic selection panel (shown when "Taxonomy" is selected)
        conditionalPanel(
          condition = "input.selection_mode == 'taxonomy'",
          ns = ns,
          div(
            selectizeInput(
              ns("TL"),
              "Taxonomic level:",
              choices = colnames(phylo)[-c(1, 2, 8)],
              options = list(
                placeholder = 'Please select an option below',
                onInitialize = I('function() { this.setValue("Phyla"); }')
              ),
              multiple = FALSE
            )
          ),
          div(uiOutput(ns("TL.second")))
        )
      ),
      column(8,
        # Hierarchical categories
        div(
          selectizeInput(
            ns("kolevel_A"),
            "Select Category:",
            choices = NULL,
            multiple = TRUE
          )),
        fluidRow(
          column(4,uiOutput(ns("dropdown_B"))),
          column(4,uiOutput(ns("dropdown_C"))),
          column(4,uiOutput(ns("dropdown_D")))
          ),
        fluidRow(
          column(4,uiOutput(ns("dropdown_E"))),
          column(4,uiOutput(ns("dropdown_F"))),
          column(4,uiOutput(ns("dropdown_G")))
          ),
      ),
      column(12,
             # Custom group selection panel (shown when "Custom Group" is selected)
             conditionalPanel(
               condition = "input.selection_mode == 'custom'",
               ns = ns,
               div(
                 numericInput(
                   ns("custom_group_count"),
                   "Number of Groups:",
                   value = 2,
                   min = 2,
                   max = 4
                 )
               ),
              uiOutput(ns("custom_groups_ui"))
             )
             )),
    #Download buttons
    fluidRow(column(12,
                    downloadButton(NS(ida,"downloadRawData"), "Download Raw data"),
                    downloadButton(NS(ida,"downloadAggData"), "Download Aggregated data"),
                    downloadButton(NS(ida,"downloadPics"), "Download Pictures")
                    )),
    # Main outputs
    fluidRow(
        column(10,shinycssloaders::withSpinner(plotly::plotlyOutput(ns("StackBarData"))))
      ),
    
    # Additional outputs
    fluidRow(
      column(6, shinycssloaders::withSpinner(plotly::plotlyOutput(ns("pcoa")))
      ),
      column(6, shinycssloaders::withSpinner(plotOutput(ns("tree")))
      )
    )
  )
}

bacteriaListUI<- function(ida){
  fluidPage( 
    fluidRow(
      tags$iframe(
        class="airtable-embed", 
        src="https://airtable.com/embed/shrvRzg90syj80wAi?backgroundColor=green&viewControls=on",
        frameborder="0",
        onmousewheel="", 
        width="100%", 
        height="1200", 
        style="background: transparent; border: 1px solid #ccc;",
        seamless = "seamless"
      )
    )
  )
}

blastUI<- function(ida, custom_db,wb,phylo,ugenome){
  ns<-NS(ida)
  fluidPage(
    #This block gives us all the inputs:
    fluidRow(
      h1('Wormbiome Blast'),
      p("looking for your sequence since 2023"),
      textAreaInput(NS(ida,'query'), 'Input sequence:', value = "", placeholder = "", width = "600px", height="200px"),
      selectInput(NS(ida,"bdb"), "Database:", choices=c("All","Custom"), width="120px"),
      #Where you can choose which genome to use
      conditionalPanel( ns = ns,
                        condition = "input['bdb'] == 'Custom'",
                        div(style="display:inline-block",
                            selectizeInput(NS(ida,"bgenome"), "Select specific genome:",
                                           choices =   ugenome,
                                           options = list(
                                             placeholder = 'Please select an option below',
                                             onInitialize = I('function() { this.setValue(""); }')
                                           ),
                                           multiple=T),
                            selectizeInput(NS(ida,"btaxa"), "OR select specific taxa:",
                                           choices =   colnames(phylo)[-c(1,2,8)],
                                           options = list(
                                             placeholder = 'Please select an option below',
                                             onInitialize = I('function() { this.setValue(""); }')
                                           ),multiple=T)
                        ),
                        conditionalPanel(
                          condition = "exists(input.btaxa)",
                          uiOutput(NS(ida,"bsecond"))
                        )
      ),
      div(style="display:inline-block",
          selectInput(NS(ida,"program"), "Program:", choices=c("blastn","tblastn"), width="100px")),
      div(style="display:inline-block",
          selectInput(NS(ida,"eval"), "e-value:", choices=c(1,0.001,1e-4,1e-5,1e-10), width="120px")),
      actionButton(NS(ida,"blast"), "BLAST!")
    ),
    
    #Basic results output
    fluidRow(column(12,
      h3("Results"),
      DT::dataTableOutput(NS(ida,"blastResults")),
      plotOutput(ns("blast_plot"),height = "800px" ),
      h4("Options:"),
      column(12, #Display context size around blast result
             numericInput(
               inputId = ns("Blast_window_Size"),      
               label = "Enter Context Window Size:",  
               value = 2500,                    
               min = 1000,                       
               max = 10000,                    
               step = 1000                     
               ),
             textOutput(ns("Blast_outputValue"))),        
      p("Alignment:", tableOutput(NS(ida,"clicked"))),
      verbatimTextOutput(NS(ida,"alignment"))
    ))
  )
}
