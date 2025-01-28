cat(file=stderr(),"Loading Modules\n")

## Annotation Browser per genome page
genelistserv<-function(ida, wbdb, column_names, phylo,utable,nrUTable){
  moduleServer(
    ida,
    function(input, output, session) {
      annotation=reactive(as.vector(unlist(column_names)[str_detect(unlist(column_names), pattern = input$AnnotDB)]))
      IDfilter=reactive(paste0(input$AnnotDB,"_ID"))
      
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
      
      # Empty tibble for the other functions
      tt=reactiveValues(x=tibble(
        WBM_geneID = character(),  
        Genome = character(),      
        Bakta_ID = character(),   
        Contig_name = character(), 
        Bakta_product = character()
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
                                         "AND ",sym(IDfilter())," != ''")) %>% 
                distinct(!!sym(IDfilter()), .keep_all = TRUE)
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
        
        checkboxGroupInput(session$ns("columns"), "",
                           choices = c("All", column_names),
                           selected = default_selection)
      })
      
      #Table for the UI
      output$data <- renderReactable({
        dt<-Rtable()
        if(nrow(dt)>0){
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
                        pagePreviousLabel = "Previous page",
                        pageNextLabel = "Next page")
                      )
            }
          }else{
            reactable(dt %>% mutate(Genome=""),
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
      ns=session$ns
      output$column_selector <- renderUI({
        default_selection <- unique(c("Genome", "WBM_geneID",  "Bakta_ID", "gapseq_ID", "IMG_ID", "PATRIC_ID", "Prokka_ID", "Contig_name","Bakta_product","IMG_product","PATRIC_product","Prokka_product"))
        checkboxGroupInput(session$ns("Dcolumns"), "",
                           choices = c("All", column_names),
                           selected = default_selection)
      })
      
      #column selection
      annotation=reactive(as.vector(column_names[str_detect(column_names, pattern = AnotTrack())]))

      #Taxonomy Filter Functions
      output$TL.second <- renderUI({
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
      ns <- session$ns
      #Taxonomy Filter
      output$TL.second <- renderUI({
        createTaxonomyFilterUI(ns,phylo,input$TL,"TL2")
      })
      
      #Get the annotation names based on which database is selected
      annotation=reactive(as.character(unlist(column_names))[str_detect(as.character(unlist(column_names)), pattern = input$anot)])
      
      output$custom_groups_ui <- renderUI({
        req(input$custom_group_count)  # Ensure the number of groups is defined
        group_count <- as.integer(input$custom_group_count)  # Convert input to integer
        
        if (group_count <= 0) return(NULL)  # If no groups are selected, return NULL
        
        # Generate UI for the specified number of groups
        # Divide the space equally among the groups
        group_width <- floor(12 / group_count)  # Each group gets an equal width column
        
        # Generate UI for the specified number of groups
        group_ui <- lapply(1:group_count, function(i) {
          column(
            width = group_width,
            div(
              h4(paste("Group", i)),
              selectizeInput(
                ns(paste0("custom_genomes_group_", i)),
                "Select Specific Genomes:",
                choices = ugenome,
                options = list(placeholder = 'Select genomes for this group'),
                multiple = TRUE
              ),
              selectizeInput(
                ns(paste0("custom_taxonomic_groups_", i)),
                "Select Taxonomic Groups:",
                choices = colnames(phylo)[-c(1, 2, 8)],
                options = list(placeholder = 'Select taxonomic groups for this group')
              ),
              # Placeholder for dynamically rendered taxonomy filter UI
              uiOutput(ns(paste0("custom_taxonomy_ui_", i)))
            )
          )
        })
        
        # Wrap the UI in a fluidRow
        do.call(fluidRow, group_ui)
      })
      
      # Dynamically render the taxonomy filter UI for each group
      observe({
        req(input$custom_group_count)  
        group_count <- as.integer(input$custom_group_count)  
        
        for (i in 1:group_count) {
          local({
            group_idx <- i  # Capture the current value of `i`
            
            output[[paste0("custom_taxonomy_ui_", group_idx)]] <- renderUI({
              req(input[[paste0("custom_taxonomic_groups_", group_idx)]])  # Ensure input exists
              
              createTaxonomyFilterUI(ns,phylo,
                Psource = input[[paste0("custom_taxonomic_groups_", group_idx)]],
                target = paste0("custom_taxonomic_groups_TL", group_idx))
            })
          })
        }
      })
      
      # Reactive to collect custom group selections
      custom_groups <- reactive({
        req(input$custom_group_count)
        group_count <- input$custom_group_count
        
        lapply(1:group_count, function(i) {
          list(
            genomes = input[[paste0("custom_genomes_group_", i)]],
            taxonomic_groups = input[[paste0("custom_taxonomic_groups_TL", i)]],
            group=paste("Group",i)
          )
        })
      })
      
      # Example: Display custom group selections in the UI
      output$custom_group_output <- renderPrint({
        custom_groups()
      })
      
      #Genome selection
      gensel <- reactive({
        if (input$selection_mode == "taxonomy") {
          if (is_empty(input$TL)) {
            phylo %>%
              filter(ID %in% input$genome) %>%
              mutate(Group = "Taxonomy") %>%
              select(Genome = ID, Group)
          } else {
            phylo %>%
              filter(ID %in% input$genome | get(input$TL) %in% input$TL2) %>%
              select(Genome = ID, Group = !!sym(input$TL))
          }
        } else if (input$selection_mode == "custom") {
          custom_selections <- custom_groups()
          if (length(custom_selections) == 0) return(data.frame(Genome = character(), Group = character()))
          
          genome_data <- do.call(rbind, lapply(seq_along(custom_selections), function(i) {
            group <- custom_selections[[i]]
            parent_taxa=input[[paste0("custom_taxonomic_groups_", i)]]
            genomes <- unique(c(
              group$genomes,
              phylo %>%
                filter(!!sym(parent_taxa) %in% group$taxonomic_groups) %>%
                pull(ID)
            ))
            tibble(Genome = genomes, Group = paste("Group", i))
          }))
          return(genome_data)
        }
      })
      
      #What Kegg is selected
      koname=reactive(paste0(input$anot,"_KO"))
      
      Rtable <- reactive({
        selected_column <- last_selected()$column
        selected_value <- last_selected()$value
        
        dbquery <- paste0(
          "SELECT WBM_geneID, Genome, ",
          paste0(annotation(), collapse = ", "),
          " FROM wb WHERE Genome IN ('", paste0(gensel()$Genome, collapse = "','"), "') ",
          "AND ", paste0(input$anot, "_ID"), " != ''"
        )
        
        base_table <- dbGetQuery(wbdb, dbquery) %>%
          mutate(
            KO = ifelse(is.na(get(koname())), "No Annotations", get(koname())),
            KO = strsplit(KO, "\\|")
          ) %>%
          unnest(KO) %>%
          separate_rows(Bakta_KO, sep = "\\|") %>%
          group_by(Genome, KO) %>%
          dplyr::summarise(kegcount = n(), .groups = "drop") %>%
          left_join(kegg, relationship = "many-to-many",by = "KO") %>% 
          left_join(gensel(),by="Genome")
        
        # Apply filtering if necessary
        if (!is.null(selected_column) && !is.null(selected_value)) {
          base_table <- base_table %>%
            filter(get(selected_column) %in% selected_value)
        }
        as.data.frame(base_table)
      })
      
      # Reactive variable for the aggregated dataframe
      aggregatedData <- reactive({
        if (is_empty(gensel()$Genome)) {
          return(NULL)
        } else {
          # Filter and aggregate data
          selected_column <- if (!is.null(last_selected()$column)) {
            LETTERS[which(LETTERS == as.character(last_selected()$column), arr.ind = TRUE) + 1]
          }
          klvl <- sym(selected_column %||% "A")  # Default to "A" if no selection
          
          Rtable() %>%
            group_by(Genome, !!klvl, Group) %>%
            dplyr::summarise(kegcount = sum(kegcount), .groups = "drop") %>%  
            filter(!is.na(!!klvl)) %>% 
            left_join(phylo, by = c("Genome" = "ID"))
        }
      })
      
      #Section to select a kegg category

      # Populate the first dropdown with unique values from column A
      updateSelectizeInput(session, "kolevel_A", choices = unique(kegg$A), server = TRUE)
      
      # Dynamically generate dropdown for Levels B to G
      output$dropdown_B <- renderUI({
        req(input$kolevel_A)  # Ensure the previous column is selected
        filtered_B <- kegg %>%
          filter(A %in% input$kolevel_A) %>%
          pull(B) %>%
          unique()
        selectizeInput(session$ns("kolevel_B"), "Select Sub-categories:", choices = filtered_B, multiple = TRUE)
      })
      
      output$dropdown_C <- renderUI({
        req(input$kolevel_B)  
        filtered_C <- kegg %>%
          filter(B %in% input$kolevel_B) %>%
          pull(C) %>%
          unique()
        selectizeInput(session$ns("kolevel_C"), "", choices = filtered_C, multiple = TRUE)
      })
      
      output$dropdown_D <- renderUI({
        req(input$kolevel_C)  
        filtered_D <- kegg %>%
          filter(C %in% input$kolevel_C) %>%
          pull(D) %>%
          unique()
        selectizeInput(session$ns("kolevel_D"), "", choices = filtered_D, multiple = TRUE)
      })
      
      output$dropdown_E <- renderUI({
        req(input$kolevel_D) 
        filtered_E <- kegg %>%
          filter(D %in% input$kolevel_D) %>%
          pull(E) %>%
          unique()
        selectizeInput(session$ns("kolevel_E"), "Select Sub-categories", choices = filtered_E, multiple = TRUE)
      })
      
      output$dropdown_F <- renderUI({
        req(input$kolevel_E)  
        filtered_F <- kegg %>%
          filter(E %in% input$kolevel_E) %>%
          pull(F) %>%
          unique()
        selectizeInput(session$ns("kolevel_F"), "", choices = filtered_F, multiple = TRUE)
      })
      
      output$dropdown_G <- renderUI({
        req(input$kolevel_F)  
        filtered_G <- kegg %>%
          filter(F %in% input$kolevel_F) %>%
          pull(G) %>%
          unique()
        selectizeInput(session$ns("kolevel_G"), "", choices = filtered_G, multiple = TRUE)
      })
      
      # Output the last selected child and its column name
      last_selected <- reactive({
        # List all kolevel inputs in order of hierarchy
        levels <- list(
          G = input$kolevel_G,
          F = input$kolevel_F,
          E = input$kolevel_E,
          D = input$kolevel_D,
          C = input$kolevel_C,
          B = input$kolevel_B,
          A = input$kolevel_A
        )
        
        # Find the last non-null and non-empty level
        for (level in names(levels)) {
          if (!is.null(levels[[level]]) && length(levels[[level]]) > 0) {
            return(list(column = level, value = levels[[level]]))
          }
        }
        
        # If none are selected, return NULL
        return(list(column = NULL, value = NULL))
      })
      
      #Create stackbar plot
      StackBarData<-reactive({withProgress(message = "Creating Stack Barplot...", value = 0, {
        p.data <- aggregatedData()  
        if (is.null(p.data)) {
          p <- ggplot() +
            theme_void() +
            geom_text(aes(0, 0, label = "Please select a genome")) +
            xlab(NULL)
        } else {
          # Generate legend to wrap
          selected_column <- if (!is.null(last_selected()$column)) {
            LETTERS[which(LETTERS == as.character(last_selected()$column), arr.ind = TRUE) + 1]
          }
          pl <- p.data %>%
            select(!!sym(selected_column%||% "A")) %>% 
            pull()
          
          # Plot
          p.data %>%
            mutate(legend = str_wrap(pl, width = 20)) %>% 
            ggplot(aes(
              x = Genome,
              y = kegcount,
              fill = legend,
              text = paste(
                "Genome:", Genome,
                "<br>Kegg Count:", kegcount,
                "<br>Level:", legend
              )
            )) +
            geom_bar(alpha = 1, stat = "identity", position = "fill") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90)) +
            facet_grid(~Group, scales = "free", space = "free_y") +
            guides(fill = guide_legend(ncol = 1)) +
            ylab("Percent") +
            scale_y_continuous(labels = scales::percent_format(scale = 100))
        }
      })
      })

      #Principal component analysis
      pcoaplot=reactive({
        withProgress(message = "Creating PCoA Plot...", value = 0, {
        if(length(unique(gensel()$Genome))<3){
          p2=ggplot() +
            theme_void() +
            geom_text(aes(0,0,label="Please select at least 2 groups/genomes or more")) +
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
            left_join(phylo,by="ID") %>% 
            left_join(gensel(), by=c("ID"="Genome"))
          Keig1<- round(Pres2$eig[1]/sum(Pres2$eig)*100,2)
          Keig2<- round(Pres2$eig[2]/sum(Pres2$eig)*100,2)
          head(ptbl)
          colourCount=length(unique(Kplot$Genus))
          
          p2=ggplot(Kplot, aes(x=V1,
                               y=V2,
                               col=Group,
                               text = paste(
                                 "Genome:", ID,
                                 "Group:",Group
                               ))) +
            geom_point()+
            theme_bw()+
            xlab(paste("Dimension 1", Keig1, "%",sep=" "))+
            ylab(paste("Dimension 2", Keig2, "%",sep=" "))+
            ggtitle("Principal Coordinates Analysis of Kegg predictions")+
            scale_color_manual(values = ggpubr::get_palette(palette="aaas",colourCount), name="Group")
        }
      })})
      #Phylogenetic tree display
      grp <- reactive({
        genome_list <- gensel()$Genome
        if (is.null(genome_list) || length(genome_list) == 0) {
          return(list(not.selected = character(), selected = character()))
        }
        list(
          not.selected = tibtree$label[!tibtree$label %in% genome_list],
          selected = tibtree$label[tibtree$label %in% genome_list]
        )
      })
      treeplot = reactive({
        validate(
          need(!is.null(grp()), "Group data is missing."),
          need(length(grp()$selected) > 0, "No genomes selected.")
        )
        groupOTU(p_tree, grp(), 'Species') +
          aes(color = Species) +
          geom_tiplab(size = 2) +
          theme(legend.position = "none") +
          scale_color_manual(values = c("orangered1", "gray80"), breaks = c("selected", "not.selected"))
      })
      
      #Output for the Interactive plotly for the plots above
      output$StackBarData <- plotly::renderPlotly({plotly::ggplotly(StackBarData(), tooltip = "text")})
      output$pcoa <- plotly::renderPlotly({plotly::ggplotly(pcoaplot(), tooltip = "text")})
      output$tree <- renderPlot({treeplot()})
      
      #Download Buttons
      output$downloadRawData <- downloadHandler(
        filename = function() {
          paste("WormBiome-Comparator-Raw-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write_csv(Rtable() %>% 
                      select(Genome,KO,kegcount) %>%
                      unique(), file)
        }
      )
      output$downloadAggData <- downloadHandler(
        filename = function() {
          paste("WormBiome-Comparator-Aggregated-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write_csv(aggregatedData(), file)
        }
      )
      fullplot= reactive({
        ggpubr::ggarrange(
          StackBarData(), 
          ggpubr::ggarrange(
            pcoaplot(), treeplot(),
            labels = c("", "C"),
            ncol = 2, nrow = 1),
          labels = c("A", "B"),
          ncol = 1, nrow = 2
        )
      }) 
      output$downloadPics <- downloadHandler(
        filename = function() {
          paste("Combined_Plot_", Sys.Date(), ".pdf", sep = "")
          },
        content = function(file) {
          ggsave(file, 
                 plot = fullplot(),
                 device = "pdf",
                 width = 12, height = 8, dpi = 300)
      }
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
        if (nrow(utable$x)>0){
          if ("All" %in% input$columns) {
            UCTable=utable$x
          } else {
            UCTable=utable$x %>% select(all_of(input$columns))
          }
          reactable(UCTable,
                    searchable = TRUE,
                    filterable = TRUE,
                    selection="multiple",
                    minRows = if(nrow(utable$x)<20){nrow(utable$x)}else{20},
                    defaultPageSize = 20,
                    language = reactableLang(
                      searchPlaceholder = "Search...",
                      noData = "No data available.",
                      pageInfo = "{rowStart} to {rowEnd} of {rows} entries",
                      pagePrevious = "\u276e",
                      pageNext = "\u276f",
                      pagePreviousLabel = "Previous page",
                      pageNextLabel = "Next page"))
        }else{
          reactable(
            tibble(Message = "Select Genes"),
            searchable = FALSE,
            filterable = FALSE,
            pagination = FALSE,
            defaultPageSize = 1,
            class = "empty-message"
          )
        }
      })
      
      output$UCcolumn_selector <- renderUI({
        # Create a vector of column names that start with the selected Annotation database
        selected_variable <- input$UCAnnotDB
        default_columns <- grep(paste0("^", selected_variable), ImpColumn, value = TRUE)

        # Include these columns in the default selection along with some fixed columns
        default_selection <- unique(c("Genome", "WBM_geneID", default_columns))
        
        checkboxGroupInput(session$ns("columns"), "",
                           choices = c("All", column_names),
                           selected = default_selection)
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
      
      TaxSel=reactive(input$GCTaxLevel)
      GCTaxSel=reactive(input$GCgpby)
      
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
          tsel=TaxSel()
          TaxoSummary <- utable$x %>% 
            left_join(phylo, by = c("Genome" = "ID")) %>%
            select(Genome, Taxon = !!sym(tsel)) %>%  
            unique() %>% 
            group_by(Taxon) %>%  
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
            tsel=TaxSel()
            utable$x %>% 
              left_join(phylo, by = c("Genome" = "ID")) %>%
              select(Genome, Taxon = !!sym(tsel)) %>%
              unique() %>% 
              group_by(Taxon) %>% 
              dplyr::summarise(Genome_Number = n(), .groups = "drop") %>%
              arrange(desc(Genome_Number)) %>% 
              slice(1:10) %>% 
              plotly::plot_ly(
                x = ~Genome_Number,
                y = ~Taxon,
                color=~Taxon,
                colors = RColorBrewer::brewer.pal(12, "Set3"),
                type = "bar", orientation = "h"
              )%>%
              plotly::layout(title = paste(tsel,"Distribution"),
                             xaxis = list(title = "Count"),
                             yaxis = list(title = tsel, categoryorder = "total ascending"), 
                             margin = list(l = 150, r = 30, t = 50, b = 120), 
                             legend = list(
                               orientation = "h",
                               x = 0.5,                
                               y = -0.2,               
                               xanchor = "center",     
                               yanchor = "top",
                               tracegroupgap = 5       
                             ),
                             showlegend = FALSE,
                             autosize = TRUE        
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
          gcg=input$GCgpby
          CatSummary <- utable$x %>% 
            dplyr::rename(GCGroup = !!sym(gcg)) %>%  # Dynamically rename the column
            separate_rows(GCGroup, sep = "\\|") %>%  # Separate rows by delimiter
            filter(!is.na(GCGroup), GCGroup != "NA") %>%  # Filter out NA or "NA" strings
            group_by(GCGroup) %>%  # Group by GCGroup
            dplyr::summarise(Gene_Number = n(), .groups = "drop") %>%  # Summarize counts
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
            gcg=input$GCgpby
            gtp=utable$x %>% 
              dplyr::rename(GCGroup = !!sym(gcg)) %>%
              separate_rows(GCGroup, sep = "\\|") %>%
              filter(!is.na(GCGroup), GCGroup != "NA") %>%
              group_by(GCGroup) %>%
              dplyr::summarise(Gene_Number = n(), .groups = "drop") %>%
              arrange(desc(Gene_Number))
            
            gtp %>%
              slice(1:8) %>% 
              plotly::plot_ly(x = ~Gene_Number,
                              y = ~GCGroup,
                              color= ~GCGroup,
                              colors = RColorBrewer::brewer.pal(12, "Set3"),
                              type = "bar",
                              orientation = "h"
              )%>%
              plotly::layout(title = "Category Distribution",
                             xaxis = list(title = "Count"),
                             yaxis = list(title = "Category", categoryorder = "total ascending"), 
                             margin = list(l = 150, r = 30, t = 50, b = 120), 
                             legend = list(
                               orientation = "h",
                               x = 0.5,                
                               y = -0.2,               
                               xanchor = "center",     
                               yanchor = "top",
                               tracegroupgap = 5       
                             ),
                             showlegend = FALSE,
                             autosize = TRUE          
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
  # Validate inputs
  req(ns, phylo, Psource, target)
  
  # Create the UI
  selectizeInput(
    ns(target),  # Use the namespace function
    "Subset Taxonomy by : (Optional)",
    choices = unique(phylo %>% select(all_of(Psource)) %>% pull()),
    options = list(
      placeholder = 'Please select an option below',
      onInitialize = I('function() { this.setValue(""); }')
    ),
    selected = selected,
    multiple = TRUE
  )
}



