## Gene list per genome page
genelistserv<-function(ida, wbdb, column_names, phylo,utable,nrUTable){
  moduleServer(
    ida,
    function(input, output, session) {
      annotation=reactive(as.vector(unlist(column_names)[str_detect(unlist(column_names), pattern = input$AnnotDB)]))
      
      output$GLTL.second<-renderUI({
        ns=session$ns
        createTaxonomyFilterUI(ns,phylo,input$GLTL,"GLTL2")
        })
      
      gensel<-reactive(if(is_empty(input$GLTL)){
        phylo %>% 
          filter(ID %in% input$genome)%>% select(ID) %>% pull()
      }else{
        phylo %>% 
          filter(ID %in% input$genome|get(input$GLTL) %in% input$GLTL2)%>% select(ID) %>% pull()
      })
      
      tt=reactiveValues(x=tibble(
        WBM_geneID = character(),  # Empty character column
        Genome = character(),      # Empty character column
        Bakta_ID = character(),    # Add other columns as needed
        Contig_name = character(), # Empty character column
        Bakta_product = character()# Add other columns as needed
      ))
      
      # Create the search result table
      Rtable<-reactive({
        withProgress(message = "Querying database...", value = 0, {
          tryCatch({
            if(length(gensel())==0||is.null(gensel())){
              ts=tt$x
            }else{
              ts<-dbGetQuery(wbdb,paste0("SELECT *",
                                         " FROM wb WHERE Genome IN ('", paste0(gensel(), collapse = "','"),"') ",
                                         "AND ",paste0(input$AnnotDB,"_ID")," != ''"))
            }
            return(ts)
            })
          })
        })
      
      output$column_selector <- renderUI({
        # Create a vector of column names that start with the selected Annotation database
        selected_variable <- input[[NS(ida, "AnnotDB")]]
        default_columns <- grep(paste0("^", selected_variable), annotation(), value = TRUE)
        
        # Include these columns in the default selection along with some fixed columns
        default_selection <- unique(c("Genome", "WBM_geneID", default_columns))
        
        checkboxGroupInput(session$ns(ida, "columns"), "Select columns to display:",
                           choices = c("All", as.vector(unlist(column_names))),
                           selected = default_selection)
      })
      
      #Table for the UI
      output$data <- renderReactable({
        dt<-Rtable()
        if(nrow(dt)>0){
          #print(input$columns)
          if ("All" %in% input$columns) {
            reactable(dt,
                      searchable = TRUE,
                      filterable = TRUE,
                      selection="multiple",
                      minRows = 20,
                      defaultPageSize = 20,
                      language = reactableLang(
                        searchPlaceholder = "Search...",
                        noData = "No data available.",
                        pageInfo = "{rowStart} to {rowEnd} of {rows} entries",
                        pagePrevious = "\u276e",
                        pageNext = "\u276f",
                        
                        # Accessible labels for assistive technologies such as screen readers.
                        # These are already set by default, but don't forget to update them when
                        # changing visible text.
                        pagePreviousLabel = "Previous page",
                        pageNextLabel = "Next page")
                      )
          } else {
            reactable(dt %>% select(input$columns),
                      searchable = TRUE,
                      filterable = TRUE,
                      selection="multiple",
                      minRows = 20,
                      defaultPageSize = 20,
                      language = reactableLang(
                        searchPlaceholder = "Search...",
                        noData = "No data available.",
                        pageInfo = "{rowStart} to {rowEnd} of {rows} entries",
                        pagePrevious = "\u276e",
                        pageNext = "\u276f",
                        
                        # Accessible labels for assistive technologies such as screen readers.
                        # These are already set by default, but don't forget to update them when
                        # changing visible text.
                        pagePreviousLabel = "Previous page",
                        pageNextLabel = "Next page")
                      )
            }
          }else{
            reactable(dt,
                      language = reactableLang(
                        searchPlaceholder = "Search...",
                        noData = "Select a Genome or a taxnomic unit to begin",)
            )
          }
        })
      
      geneSelectionHandler(input, output, utable, nrUTable, wbdb, "data",Rtable())
      }
  )
}

## Gene search page
genseSearchServ<-function(ida,wbdb, column_names,phylo,utable,ugenome,nrUTable){
  moduleServer(
    ida,
    function(input, output, session) {
      output$column_selector <- renderUI({
        default_selection <- unique(c("Genome", "WBM_geneID",  "Bakta_ID", "gapseq_ID", "IMG_ID", "PATRIC_ID", "Prokka_ID", "Contig_name","Bakta_product","IMG_product","PATRIC_product","Prokka_product"))
        checkboxGroupInput(session$ns(ida, "Dcolumns"), "",
                           choices = c("All", as.vector(unlist(column_names))),
                           selected = default_selection)
      })
      
      #column selection
      annotation=reactive(as.vector(column_names[str_detect(column_names, pattern = AnotTrack())]))

      #Taxonomy Filter Functions
      output$TL.second <- renderUI({
        ns <- session$ns
        createTaxonomyFilterUI(ns,phylo,input$TL,"TL2")
      })
      
      GenomeList=reactive(if(any(input$Sgenome %in% "All")){ugenome}else{input$Sgenome})
      Cfilter=reactive(if(any(input$ColumnFilter %in% "All")){as.vector("All")}else{input$ColumnFilter})
      
      gensel<-reactive(if(is_empty(input$TL)){
        phylo %>% 
          filter(ID %in% GenomeList())%>% select(ID) %>% pull()
        }else{
          phylo %>% 
            filter(ID %in% GenomeList()|get(input$TL) %in% input$TL2)%>%
            select(ID) %>% pull()
          })

      #Debug
      output$value <-renderText({ input$geneSearch })
      #Subsection filter

      qsearch<-eventReactive(input$actionSearch,{
        input$geneSearch
      }, ignoreNULL = FALSE,
      )
      AnotTrack=eventReactive(input$actionSearch,{
        input$Svariable
      }, ignoreNULL = FALSE,
      )
      #Some statistics
      output$uSearchDesc<-renderText({
        paste0("Your search includes ",nrow(Sdata())," gene",ifelse(nrow(Sdata())>1,"s","")," from ",length(unique(Sdata()$Genome))," genome", ifelse(length(unique(Sdata()$Genome))>1,"s",""))
      })
      
      Sdata <- reactive({
        req(qsearch())
        #Debug
        #print(Cfilter())
        #print(GenomeList())
        if (qsearch() == "" ||qsearch() == "Search" || is.null(qsearch())) {
          return(data.frame(Example = "No search term provided"))
        } else {
        
          #Select the column to search in (Or all of them)
          filter_clause <- if(any(Cfilter() %in% "All")){
          paste0("CONCAT_WS(' ',", paste(input$Dcolumns, collapse = ", "), ") LIKE '%", qsearch(), "%'")
          } else {
          paste0("CONCAT_WS(' ',", paste0(Cfilter(), collapse = ", "), ") LIKE '%", qsearch(), "%'")
          }
          
          #Build the query
          query <- paste0(
            "SELECT ", paste(input$Dcolumns, collapse = ", "),
            " FROM wb WHERE ", filter_clause, 
            " AND Genome IN ('", paste(gensel(), collapse = "','"), "')"
          )
        
        #print(query)  # Debug query
        
        withProgress(message = "Querying database...", value = 0, {
        tryCatch({
          result <- dbGetQuery(wbdb, query)
        }, error = function(e) {
          print(e)
          result <- data.frame(Example = "Error in database query")
        })
        
        return(result)
      })}})
      
      output$STable <- renderReactable({
        reactable(Sdata(),
                  searchable = TRUE,
                  filterable = TRUE,
                  selection="multiple",
                  minRows = 20,
                  showPageSizeOptions = TRUE,
                  striped = TRUE,
                  defaultPageSize = 20)
      })
      
      #Gene selection and cart saving section

      geneSelectionHandler(input, output, utable, nrUTable, wbdb, "STable",Sdata())
    })
  }

## Comparative tool page
comparatorserv<-function(ida,wbdb, column_names, kegg, phylo,p_tree,getPal=getPalette,tibtree){
  moduleServer(
    ida,
    function(input, output, session) {
      #Taxonomy Filter
      output$TL.second <- renderUI({
        ns <- session$ns
        createTaxonomyFilterUI(ns,phylo,input$TL,"TL2")
      })
      #Get the annotation names based on which database is selected
      annotation=reactive(as.character(unlist(column_names))[str_detect(as.character(unlist(column_names)), pattern = input$anot)])
      #Genome selection
      gensel<-reactive(if(is_empty(input$TL)){
        phylo %>% 
          filter(ID %in% input$genome)%>% select(ID) %>% pull()
      }else{
        phylo %>% 
          filter(ID %in% input$genome|get(input$TL) %in% input$TL2)%>% select(ID) %>% pull()
      })
      #What Kegg is selected
      koname=reactive(paste0(input$anot,"_KO"))
      
      #Genome table
      Ptable<-reactive({
        klvl <- sym(input$kolevel)
        dbquery= paste0("SELECT WBM_geneID, Genome, ",
                        paste0(annotation(), collapse = ", "),
                        " FROM wb WHERE Genome IN ('", paste0(gensel(), collapse = "','"),"') ",
                        "AND ",paste0(input$anot,"_ID")," != ''")
        tt <- dbGetQuery(wbdb, dbquery) %>%
          mutate(
            KO = ifelse(is.na(get(koname())), "No Annotations", get(koname())),
            KO = strsplit(KO, "\\|")
          ) %>%
          unnest(KO) %>%
          group_by(Genome, KO) %>%
          dplyr::summarise(kegcount = n(), .groups = "drop") %>%
          left_join(kegg, relationship = "many-to-many")
        
        return(as.data.frame(tt)) 
        })

      #Filtering genome table based on the kegg filtering if selected
      
      Rtable <- reactive({
        if (is.null(last_selected())) {
          Ptable()
        } else {
          Ptable() %>%
            filter(.data[[last_selected()$column]] %in% last_selected()$value)
        }
      })
      
      #Section to select a kegg category
      
      # Initialize reactive value to store the last selected child and its column name
      last_selected <- reactiveVal(list(column = NULL, value = NULL))
      
      # Populate the first dropdown with unique values from column A
      updateSelectizeInput(session, "kolevel_A", choices = unique(kegg$A), server = TRUE)
      
      # Dynamically generate dropdown for Level B
      output$dropdown_B <- renderUI({
        req(input$kolevel_A)  # Ensure A is selected
        filtered_B <- kegg %>%
          filter(A %in% input$kolevel_A) %>%
          pull(B) %>%
          unique()
        selectizeInput(session$ns("kolevel_B"), "Select Sub-categories:", choices = filtered_B, multiple = TRUE)
      })
      
      # Dynamically generate dropdown for Level C
      output$dropdown_C <- renderUI({
        req(input$kolevel_B)  # Ensure B is selected
        filtered_C <- kegg %>%
          filter(B %in% input$kolevel_B) %>%
          pull(C) %>%
          unique()
        selectizeInput(session$ns("kolevel_C"), "", choices = filtered_C, multiple = TRUE)
      })
      
      # Dynamically generate dropdown for Level D
      output$dropdown_D <- renderUI({
        req(input$kolevel_C)  # Ensure C is selected
        filtered_D <- kegg %>%
          filter(C %in% input$kolevel_C) %>%
          pull(D) %>%
          unique()
        selectizeInput(session$ns("kolevel_D"), "", choices = filtered_D, multiple = TRUE)
      })
      
      # Dynamically generate dropdown for Level E
      output$dropdown_E <- renderUI({
        req(input$kolevel_D)  # Ensure D is selected
        filtered_E <- kegg %>%
          filter(D %in% input$kolevel_D) %>%
          pull(E) %>%
          unique()
        selectizeInput(session$ns("kolevel_E"), "Select Sub-categories", choices = filtered_E, multiple = TRUE)
      })
      
      # Dynamically generate dropdown for Level F
      output$dropdown_F <- renderUI({
        req(input$kolevel_E)  # Ensure E is selected
        filtered_F <- kegg %>%
          filter(E %in% input$kolevel_E) %>%
          pull(F) %>%
          unique()
        selectizeInput(session$ns("kolevel_F"), "", choices = filtered_F, multiple = TRUE)
      })
      
      # Dynamically generate dropdown for Level G
      output$dropdown_G <- renderUI({
        req(input$kolevel_F)  # Ensure F is selected
        filtered_G <- kegg %>%
          filter(F %in% input$kolevel_F) %>%
          pull(G) %>%
          unique()
        selectizeInput(session$ns("kolevel_G"), "", choices = filtered_G, multiple = TRUE)
      })
      
      # Output the last selected child and its column name
      output$selected_categories <- renderPrint({
        last_selected()
      })
      
      #Interactive plotly for kegg levels
      output$StackBarData <- plotly::renderPlotly({
        withProgress(message = "Creating Stack Barplot...", value = 0, {
        if (is_empty(gensel())) {
          p <- ggplot() +
            theme_void() +
            geom_text(aes(0, 0, label = "Please select a genome")) +
            xlab(NULL)
        } else {
          klvl <- sym(input$kolevel)
          p <- Rtable() %>%
            group_by(Genome, !!klvl) %>%
            dplyr::summarise(kegcount = sum(kegcount), .groups = "drop") %>%  
            filter(!is.na(!!klvl)) %>% 
            left_join(phylo, by = c("Genome" = "ID"))
          pl = p %>% 
            select(!!klvl) %>% 
            pull()
          p <- p %>%
            mutate(legend=str_wrap(pl, width = 20)) %>% 
            ggplot(aes(x = Genome,
                       y = kegcount,
                       fill = legend,
                       text = paste(
                         "Genome:", Genome,
                         "<br>Kegg Count:", kegcount,
                         "<br>Level:", legend
                         )
                       )
                   ) +
            geom_bar(alpha = 1, stat = "identity", position = "fill") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90)) +
            facet_grid(~get(input$TL), scales = "free",space = "free_y") +
            guides(fill = guide_legend(ncol = 1)) +
            ylab("Percent")
        }
        plotly::ggplotly(p,tooltip = "text")
      })})
      
      
      #Principal component analysis
      output$pcoa <- plotly::renderPlotly({
        withProgress(message = "Creating PCoA Plot...", value = 0, {
        if(length(gensel())<3){
          p2=ggplot() +
            theme_void() +
            geom_text(aes(0,0,label="Please select two genomes or more")) +
            xlab(NULL)
        }else{
          #Debug
          #print(paste(gensel(),"and",length(gensel())))
          ptbl<-Rtable()%>% 
            select(Genome,KO,kegcount) %>%
            unique() %>%
            pivot_wider( id_cols= Genome,
                         names_from = KO,
                         values_from = kegcount,
                         values_fill = 0) %>% 
            column_to_rownames("Genome")
          P.dist<-vegan::vegdist(as.matrix(ptbl), method = "bray")
          Pres2<-labdsv::pco(P.dist, k=2)
          Kplot<-as.data.frame(Pres2$points) %>% 
            rownames_to_column("ID") %>% 
            left_join(phylo, by=)
          Keig1<- round(Pres2$eig[1]/sum(Pres2$eig)*100,2)
          Keig2<- round(Pres2$eig[2]/sum(Pres2$eig)*100,2)
          head(ptbl)
          colourCount=length(unique(Kplot$Genus))
          
          p2=ggplot(Kplot, aes(x=V1,
                               y=V2,
                               col=Genus,
                               text = paste(
                                 "Genome:", ID,
                                 "Genus:",Genus
                               ))) +
            geom_point()+
            theme_bw()+
            xlab(paste("Dimension 1", Keig1, "%",sep=" "))+
            ylab(paste("Dimension 2", Keig2, "%",sep=" "))+
            ggtitle("Principal Coordinates Analysis of Kegg predictions")+
            scale_color_manual(values = getPal(colourCount), name="Genus")
        }
        plotly::ggplotly(p2, tooltip = "text")
      })})

      grp<-reactive(if(is_empty(gensel())){
        ""
      }else{
        list(not.selected =tibtree$label[-c(which(tibtree$label %in% gensel()),187:348)],
             selected     =tibtree$label[c(which(tibtree$label %in% gensel()))])
      })
      output$tree <- renderPlot(groupOTU(p_tree, grp(), 'Species') +
                                  aes(color=Species) +
                                  geom_tiplab(size=2)+
                                  theme(legend.position="none")+
                                  scale_color_manual(values = c("orangered1","gray80"),
                                                     breaks = c("selected","not.selected"))
      )
    }
  )
}

## Gene selection to user cart function
geneSelectionHandler<-function(input, output, utable, nrUTable, wbdb, Csource,Ctable) {
  uSelect <- eventReactive(
    input$selectGene, {
      selected <- getReactableState(Csource, "selected")
      req(selected)
      Ctable[selected, colnames(Ctable) == "WBM_geneID"]
    },
    ignoreNULL = FALSE
  )
  
  observeEvent(input$selectGene, { 
    withProgress(message = "Adding Genes to cart...", value = 0, {
                 sl <- dbGetQuery(wbdb, paste0("SELECT * FROM wb WHERE WBM_geneID IN ('",
                                               paste0(uSelect(), collapse = "','"),
                                               "')"))
                 utable$x <- unique(rbind(utable$x, sl))
    })
  })
  
  observeEvent(input$selectGene, {
    nrUTable$nrow = nrow(utable$x)
  })
  
  observeEvent(input$resetGene, {
    utable$x <- NULL
    utable$x <- tibble()
    nrUTable$nrow = nrow(utable$x)
  })
  
  output$uCartLabel <- renderText({
    paste0("Selected Genes (", nrUTable$nrow, ")")
  })
  
  output$downloadAData <- downloadHandler(
    filename = function() {
      paste("WormBiome-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(Rtable(), file)
    }
  )
}

## User gene cart page
userGeneCartserv <- function(ida, utable, wbdb, phylo, nrUTable) {
  moduleServer(
    ida,
    function(input, output, session) {
      # Main Table
      output$userGeneCart <- renderReactable({
        reactable(
          utable$x,
          searchable = TRUE,
          filterable = TRUE,
          selection = "multiple",
          minRows = if(nrow(utable$x)<20){nrow(utable$x)}else{20},
          defaultPageSize = 20
        )
      })
      
      # Summary
      output$uCartDescription <- renderText({
        if (is.null(utable$x) || nrow(utable$x) == 0) {
          "Your cart is currently empty."
        } else {
          paste0(
            "Your cart currently contains ",
            nrow(utable$x), " gene", ifelse(nrow(utable$x) > 1, "s", ""),
            " from ", length(unique(utable$x$Genome)),
            " genome", ifelse(length(unique(utable$x$Genome)) > 1, "s", "")
          )
        }
      })
      
      # Buttons
      observeEvent(input$resetGene, {
        utable$x <- tibble()
        nrUTable$nrow <- nrow(utable$x)
      })
      
      # Download Button
      output$downloadAData <- downloadHandler(
        filename = function() {
          paste("WormBiome-GeneCart-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write_csv(utable$x, file)
        }
      )
      
      # Display: Taxonomic Overview Table
      output$GCTable1 = renderReactable({
        if (is.null(utable$x) || nrow(utable$x) == 0) {
          # Render a placeholder table with a message
          reactable(
            tibble(Message = "Select Genes"),
            searchable = FALSE,
            filterable = FALSE,
            pagination = FALSE,
            defaultPageSize = 1,
            class = "empty-message"
          )
        } else {
          # Generate the actual table when utable$x is not empty
          TaxoSummary <- utable$x %>% 
            left_join(phylo, by = c("Genome" = "ID")) %>%
            select(Genome, Genus) %>%
            unique() %>% 
            group_by(Genus) %>% 
            dplyr::summarise(Genome_Number = n(), .groups = "drop") %>% 
            arrange(desc(Genome_Number))
          
          reactable(
            TaxoSummary,
            searchable = TRUE,
            filterable = TRUE,
            selection = "multiple",
            minRows = if (nrow(TaxoSummary) < 10) { nrow(TaxoSummary) } else { 10 },
            defaultPageSize = 10
          )
        }
      })
      
      # Display: Taxonomic Overview Graphic
      output$GCoverview1 <- plotly::renderPlotly({
        if (nrow(utable$x) == 0) {
          plotly::plot_ly(x = 0, y = 0, type = "scatter", mode = "text",
                          text = "No genes selected"
          )
        } else {
          withProgress(message = "Creating Stack Barplot...", value = 0, {
            utable$x %>% 
              left_join(phylo, by = c("Genome" = "ID")) %>%
              select(Genome, Genus) %>%
              unique() %>% 
              group_by(Genus) %>% 
              dplyr::summarise(Genome_Number = n(), .groups = "drop") %>%
              arrange(desc(Genome_Number)) %>% 
              slice(1:10) %>% 
              plotly::plot_ly(
                x = ~Genome_Number,
                y = ~Genus,
                color=~Genus,
                colors = RColorBrewer::brewer.pal(12, "Set3"),
                type = "bar", orientation = "h"
              )%>%
              plotly::layout(title = "Genus Distribution",
                             xaxis = list(title = "Count"),
                             yaxis = list(title = "Genus", categoryorder = "total ascending"), # Order by value
                             margin = list(l = 150, r = 30, t = 50, b = 120), # Add margins to center the plot
                             legend = list(
                               orientation = "h",
                               x = 0.5,                # Center the legend horizontally
                               y = -0.2,               # Place legend below the plot
                               xanchor = "center",     # Align to the center
                               yanchor = "top",
                               tracegroupgap = 5       # Add small spacing between legend items
                             ),
                             showlegend = FALSE,
                             autosize = TRUE          # Auto-adjust plot size
              )
          })
        }
      })
      
      # Display: Categories Overview Table
      
      output$GCTable2 = renderReactable({
        if (is.null(utable$x) || nrow(utable$x) == 0) {
          # Render a placeholder table with a message
          reactable(
            tibble(Message = "Select Genes"),
            searchable = FALSE,
            filterable = FALSE,
            pagination = FALSE,
            defaultPageSize = 1,
            class = "empty-message"
          )
        } else {
          CatSummary=utable$x %>% 
            separate_rows(Bakta_KO, sep = "\\|") %>%
            filter(!is.na(Bakta_KO), Bakta_KO != "NA") %>%
            group_by(Bakta_KO) %>%
            dplyr::summarise(Gene_Number = n(), .groups = "drop") %>%
            arrange(desc(Gene_Number))
          
          reactable(
            CatSummary,
            searchable = TRUE,
            filterable = TRUE,
            selection = "multiple",
            minRows = if(nrow(CatSummary)<10){nrow(CatSummary)}else{10},
            defaultPageSize = 10
          )
        }})
      
      output$GCoverview2 <- plotly::renderPlotly({
        if (nrow(utable$x) == 0) {
          plotly::plot_ly(
            x = 0, y = 0, type = "scatter", mode = "text",
            text = "No genes selected"
          )
        } else {
          withProgress(message = "Creating Stack Barplot...", value = 0, {
            gtp=utable$x %>% 
              separate_rows(Bakta_KO, sep = "\\|") %>%
              filter(!is.na(Bakta_KO), Bakta_KO != "NA") %>%
              group_by(Bakta_KO) %>%
              dplyr::summarise(Gene_Number = n(), .groups = "drop") %>%
              arrange(desc(Gene_Number))
            
            gtp %>%
              slice(1:8) %>% 
              plotly::plot_ly(x = ~Gene_Number,
                              y = ~Bakta_KO,
                              color= ~Bakta_KO,
                              colors = RColorBrewer::brewer.pal(12, "Set3"),
                              type = "bar",
                              orientation = "h"
              )%>%
              plotly::layout(title = "Category Distribution",
                             xaxis = list(title = "Count"),
                             yaxis = list(title = "Category", categoryorder = "total ascending"), 
                             margin = list(l = 150, r = 30, t = 50, b = 120), # Add margins to center the plot
                             legend = list(
                               orientation = "h",
                               x = 0.5,                # Center the legend horizontally
                               y = -0.2,               # Place legend below the plot
                               xanchor = "center",     # Align to the center
                               yanchor = "top",
                               tracegroupgap = 5       # Add small spacing between legend items
                             ),
                             showlegend = FALSE,
                             autosize = TRUE          # Auto-adjust plot size
              )
          }
          )
        }
      })
      
    }
  )
}

#Taxonomic filter option

createTaxonomyFilterUI <- function(ns, phylo, Psource, target, selected = character(0)) {
  selectizeInput(session$ns(target), "Subset Taxonomy by : (Optional)",
                 choices = unique(phylo %>% select(all_of(Psource)) %>% unique() %>% pull),
                 options = list(
                   placeholder = 'Please select an option below',
                   onInitialize = I('function() { this.setValue(""); }')
                 ),
                 selected = selected,
                 multiple = TRUE)
}

