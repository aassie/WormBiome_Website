home=fluidPage(
  fluidRow(class = "MainRow",
     column(8,
        column(12,  h1("Welcome to Wormbiome", style="color:white;font-family: 'Montserrat', sans-serif;"),
               style="background-color:#29313f;border-radius: 10px;box-shadow: -5px 5px #e2e2e2;"),
        column(12, p("Wormbiome is a database dedicated to collecting genomes associated with  C. elegans! This database is a comprehensive collection of bacterial genomes related to the C. elegans nematode. This powerful model organism is naturally colonized by diverse bacteria, making it an ideal system for studying the interactions between host and microbes. With this database, researchers can access the genetic information of these associated bacteria and use it to understand the role of the microbiome in shaping host physiology and physiology and its implications for human health. Explore the relationships between different bacterial species, and track the spread of antibiotic-resistant strains. Dive into the metabolic pathways and biochemistry of the organisms and discover new applications in biotechnology and synthetic biology. Join us in unraveling the secrets of the C. elegans microbiome, and unlock the potential of this powerful model organism."),
               style="padding-top:1em")
     ),
     column(4,
        column(12, h1("News", style="color:white;font-family: 'Montserrat', sans-serif;"),
               style="background-color:#ea8b8b;border-radius: 10px;box-shadow: -5px 5px #e2e2e2;"),
        column(12,
               includeMarkdown("./static/News.md"),
               style="padding-top:1em")
     ),
  ),
  fluidRow(class = "FunctionRow",
     column(4,style="height:15em",
        column(12,
               h3("Browse the collection", style="color:white;font-family: 'Montserrat', sans-serif;"),
               style="background-color:#29313f;border-radius: 10px;box-shadow: -5px 5px #e2e2e2;"),
        column(12, 
               p("Look for the available bacteria in the collection and their associated metadata"),
               actionButton("Mbutton","Collection"),
               style="padding-top:1em")
   ),
     column(4,style="height:15em",
        column(12,
               h3("Search Gene", style="color:white;font-family: 'Montserrat', sans-serif;"),
               style="background-color:#29313f;border-radius: 10px;box-shadow: -5px 5px #e2e2e2;"),
        column(12,
               p("Look for a specific gene and its distribution accross the available bacteria"),
               actionButton("Sbutton", "Name search"),
               actionButton("Bbutton", "Sequence search"),
               style="padding-top:1em")
     ),
     column(4,style="height:15em",
        column(12,
               h3("Browse Genomes", style="color:white;font-family: 'Montserrat', sans-serif;"),
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
  fluidRow(
    column(4),
    column(4,
    h3("Featured Bacteria", style="color:white;font-family: 'Montserrat', sans-serif;"), 
    img(src="BH3.png", style="  display: block;  margin-left: auto;  margin-right: auto;max-height: 30vh"),
    style="background-color:#cacaca;border-radius: 10px;box-shadow: -5px 5px #e2e2e2;",
    p("")
    ),
    column(4)
  )
)


PageDoc=fluidPage( 
  fluidRow(class = "PageDoc",
           column(12, h2("Documentation", style="color:white;font-family: 'Montserrat', sans-serif;"), 
                  style="background-color:#29313f;border-radius: 10px;box-shadow: -5px 5px #e2e2e2;"),
           column(12, 
                  includeMarkdown("./static/Manual.md"),
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

geneListUI= function(ida,wb,phylo,ugenome){
  fluidPage( 
    fluidRow( 
      column(2,
             div(selectInput(NS(ida,"variable"), "Variable:",
                             c("Prokka" = "Prokka",
                               "IMG" = "IMG",
                               "PATRIC" = "PATRIC"))),
             div(selectizeInput(NS(ida,"genome"), "Genome:",
                                choices =   ugenome,
                                options = list(
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }')
                                ),multiple=T
             ),
             ),
             div(selectizeInput(NS(ida,"genus"), "Genus:",
                                choices =   unique(phylo$Genus),
                                options = list(
                                  placeholder = 'Please select an option below',
                                  onInitialize = I('function() { this.setValue(""); }')
                                ),multiple=T
             ),
             ),
             actionButton(NS(ida,"selectGene"), "Select Gene(s)"),
             actionButton(NS(ida,"resetGene"), "Clear Gene(s)"),
             downloadButton(NS(ida,"downloadSData"), "Download selected"),
             downloadButton(NS(ida,"downloadAData"), "Download All")
      ),
      column(10,
             reactableOutput(NS(ida,"data"))
      )
    )
  )
}

genseSearchUI<-function(ida,wb,phylo,utable,ugenome){
  fluidPage( 
    fluidRow(
      h1("Search settings")
    ),
    fluidRow(
      column(
        width = 3,
        div(style="display:inline-block",
            textInput(NS(ida,"geneSearch"), "Text Search", "Search"))
        ),
      column(
        width = 3,
        div(style="display:inline-block",
            selectInput(NS(ida,"Svariable"), "Annotation track(s):",
                        c("All" = ".",
                          "Prokka" = "Prokka",
                          "IMG" = "IMG",
                          "PATRIC" = "PATRIC"),
                        selected="All"))
        ),
      column(
        width = 3,
        actionButton(NS(ida,"actionSearch"), "Search"),),
      column(
        width = 3,
        h3("Summary:"),
        uiOutput(NS(ida,"uSearchDesc"))
      )
    ),
    fluidRow(
      column(
        width = 3,
        div(p("Checking: "),verbatimTextOutput(NS(ida,"value"))),
        div(uiOutput(NS(ida,"searchSecond")))
      ),
      column(
        width = 3,
        div(
          selectizeInput(NS(ida,"TL"), "Taxonomic filtering (optional):",
                         choices =   colnames(phylo)[-c(1,2,8)],
                         options = list(
                           placeholder = 'Please select an option below',
                           onInitialize = I('function() { this.setValue("Phyla"); }')
                         ),multiple=F)),
        div(
          uiOutput(NS(ida,"TL.second"))
        ),
        div(selectizeInput(NS(ida,"Sgenome"), "Filter by Genome (Optional)",
                              choices =   c("All",ugenome),
                           multiple=T,
                           selected="All"
                              )))
        ),
    fluidRow(
      width = 3,
           actionButton(NS(ida,"selectGene"), "Select Gene(s)"),
           actionButton(NS(ida,"resetGene"), "Clear Gene(s)"),
           downloadButton(NS(ida,"downloadSData"), "Download selected"),
           downloadButton(NS(ida,"downloadAData"), "Download All")
           
    ),
    fluidRow(
      h1("Results")
      ),
    fluidRow(
      reactableOutput(NS(ida,"STable")))
  )
}

userGeneCartUI<-function(ida,wb,phylo,utable){
  fluidPage( 
    fluidRow(column(10,
                    uiOutput(NS(ida,"uCartDescription")),
                    actionButton(NS(ida,"resetGene"), "Clear Gene(s)"),
                    downloadButton(NS(ida,"downloadSData"), "Download selected"),
                    downloadButton(NS(ida,"downloadAData"), "Download All"))
    ),
    fluidRow( column(12,
                     reactableOutput(NS(ida,"userGeneCart"))
    )
    )
  )
}

comparatorUI<- function(ida,wb,phylo,kegg,tree,p_tree,ugenome){
  ns <- NS(ida)
  fluidPage( 
    fluidRow( 
      column(2,
             div(
               selectInput(NS(ida,"anot"), "Annotation track:",
                           c("Prokka" = "Prokka"))),
             div(
               selectizeInput(NS(ida,"TL"), "Taxonomic level:",
                              choices =   colnames(phylo)[-c(1,2,8)],
                              options = list(
                                placeholder = 'Please select an option below',
                                onInitialize = I('function() { this.setValue("Phyla"); }')
                              ),multiple=F)),
             div(
               uiOutput(NS(ida,"TL.second"))
             ),
             div(
               selectizeInput(NS(ida,"genome"), "Add specific genome:",
                              choices =   ugenome,
                              options = list(
                                placeholder = 'Please select an option below',
                                onInitialize = I('function() { this.setValue(""); }')
                              ),multiple=T)
             ),
             div(
               selectizeInput(NS(ida,"kolevel"), "Kegg Level*:",
                              choices =   colnames(kegg)[-c(8)],
                              options = list(
                                placeholder = 'Please select an option below',
                                onInitialize = I('function() { this.setValue("A"); }')
                              ),multiple=F),
             ),
             div(
               selectizeInput(NS(ida,"filter"), "filter*:",
                              choices =   colnames(kegg)[-c(8)],
                              options = list(
                                placeholder = 'Please select an option below',
                                onInitialize = I('function() { this.setValue(""); }')
                              ),multiple=F)
             ),
             div(
               uiOutput(NS(ida,"secondSelection"))
             )
      ),
      column(10,
             plotOutput(NS(ida,"data"))
      )
    ),
    fluidRow(
      column(6,
             plotOutput(NS(ida,"pcoa"))
      ),
      column(6,
             plotOutput(NS(ida,"tree"))
      ),
    ))
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
    
    #this snippet generates a progress indicator for long BLASTs
    fluidRow(
      div(class = "busy",  
          p("Calculation in progress.."), 
          img(src="https://i.stack.imgur.com/8puiO.gif", height = 100, width = 100,align = "center")
      )),
    
    #Basic results output
    mainPanel(
      h4("Results"),
      DT::dataTableOutput(NS(ida,"blastResults")),
      p("Alignment:", tableOutput(NS(ida,"clicked"))),
      verbatimTextOutput(NS(ida,"alignment"))
    )
  )
}
