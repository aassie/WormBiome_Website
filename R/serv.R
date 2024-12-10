genelistserv<-function(ida, wbdb, column_names, phylo,utable,nrUTable){
  moduleServer(
    ida,
    function(input, output, session) {
      annotation=reactive(as.vector(unlist(column_names)[str_detect(unlist(column_names), pattern = input$AnnotDB)]))
      gensel<-reactive(phylo %>% 
                         filter(ID %in% input$genome|Genus %in% input$genus) %>% 
                         select(ID) %>% pull())
      
      # Create the search result table
      Rtable<-reactive({
        tt<-dbGetQuery(wbdb,paste0("SELECT *",
                                   " FROM wb WHERE Genome IN ('", paste0(gensel(), collapse = "','"),"') ",
                                   "AND ",paste0(input$AnnotDB,"_ID")," != ''"))
        return(tt)
        })
      
      output$column_selector <- renderUI({
        # Create a vector of column names that start with the selected Annotation database
        selected_variable <- input[[NS(ida, "AnnotDB")]]
        default_columns <- grep(paste0("^", selected_variable), annotation(), value = TRUE)
        
        # Include these columns in the default selection along with some fixed columns
        default_selection <- unique(c("Genome", "WBM_geneID", default_columns))
        
        checkboxGroupInput(NS(ida, "columns"), "Select columns to display:",
                           choices = c("All", as.vector(unlist(column_names))),
                           selected = default_selection)
      })
      
      #Table for the UI
      output$data <- renderReactable({
        dt<-Rtable()
        #print(input$columns)
        if ("All" %in% input$columns) {
          reactable(dt,
                    searchable = TRUE,
                    filterable = TRUE,
                    selection="multiple",
                    minRows = 20,
                    defaultPageSize = 20)
        } else {
          reactable(dt %>% select(input$columns),
                    searchable = TRUE,
                    filterable = TRUE,
                    selection="multiple",
                    minRows = 20,
                    defaultPageSize = 20)
        }
        })
      
      #nrUTable <- reactiveValues(nrow = 0L)
      uSelect<-eventReactive(input$selectGene, {
        selected <- getReactableState("data", "selected")
        req(selected)
        Rtable()[selected,colnames(Rtable())=="WBM_geneID"]
      },
      ignoreNULL = FALSE)
      observeEvent(input$selectGene,{
        #print(uSelect())
        sl<-dbGetQuery(wbdb,paste0("SELECT * FROM wb WHERE WBM_geneID IN ('",
                                   paste0(uSelect(), collapse = "','"),
                                   "')"))
        utable$x<-unique(rbind(utable$x,sl))
      })
      observeEvent(input$selectGene, {
        nrUTable$nrow=nrow(utable$x)
      })
      observeEvent(input$resetGene, {
        utable$x <-NULL
        utable$x <- tibble()
        nrUTable$nrow=nrow(utable$x)
      })
      output$uCartLabel<-renderText({
        paste0("Selected Genes (",nrUTable$nrow,")")})
      output$downloadAData <- downloadHandler(
        filename = function() {
          paste("WormBiome-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write_csv(Rtable(), file)
        }
      )}
  )
}

userGeneCartserv<-function(ida, utable, wbdb, phylo, nrUTable){
  moduleServer(
    ida,
    function(input, output, session) {
      output$userGeneCart <- renderReactable({
        reactable(utable$x,
                  searchable = TRUE,
                  filterable = TRUE,
                  selection="multiple",
                  minRows = 20,
                  defaultPageSize = 20)
      })
      output$uCartDescription<-renderText({
        paste0("Your cart currently contain ",nrow(utable$x)," gene",ifelse(nrow(utable$x)>1,"s","")," from ",length(unique(utable$x$Genome))," genome", ifelse(length(unique(utable$x$Genome))>1,"s",""))})
      
      observeEvent(input$resetGene, {
        utable$x <-NULL
        utable$x <- tibble()
        nrUTable$nrow=nrow(utable$x)
      })
      output$uCartLabel<-renderText({
        paste0("Selected Genes (",nrUTable$nrow,")")})
      output$downloadAData <- downloadHandler(
        filename = function() {
          paste("WormBiome-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write_csv(STable(), file)
        }
      )
    }
  )
}

genseSearchServ<-function(ida,wbdb, column_names,phylo,utable,ugenome,nrUTable){
  moduleServer(
    ida,
    function(input, output, session) {
      output$column_selector <- renderUI({
        default_selection <- unique(c("Genome", "WBM_geneID",  "Bakta_ID", "gapseq_ID", "IMG_ID", "PATRIC_ID", "Prokka_ID", "Contig_name","Bakta_product","IMG_product","PATRIC_product","Prokka_product"))
        checkboxGroupInput(NS(ida, "Dcolumns"), "",
                           choices = c("All", as.vector(unlist(column_names))),
                           selected = default_selection)
      })
      
      #column selection
      annotation=reactive(as.vector(column_names[str_detect(column_names, pattern = AnotTrack())]))

      #Taxonomy Filter Functions
      output$TL.second <- renderUI({
        ns <- session$ns
        selectizeInput(ns("TL2"), "Subset Taxonomy by : (Optional)",
                       choices =   unique(phylo %>% select(input$TL) %>% unique() %>% pull),
                       options = list(
                         placeholder = 'Please select an option below',
                         onInitialize = I('function() { this.setValue(""); }')
                       ),
                       selected = character(0),
                       multiple=T)
      })
      
      GenomeList=reactive(if(any(input$Sgenome %in% "All")){ugenome}else{input$Sgenome})
      Cfilter=reactive(if(any(input$ColumnFilter %in% "All")){column_names}else{input$ColumnFilter})
      
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
        
        filter_clause <- if (is.null(Cfilter()) || length(Cfilter()) == 0) {
          paste0("CONCAT_WS('', ", paste(input$Dcolumns, collapse = ", "), ") LIKE '%", qsearch(), "%'")
        } else {
          paste0(Cfilter(), " LIKE '%", qsearch(), "%'")
        }
        
        query <- paste0(
          "SELECT WBM_geneID, Genome, ", paste(input$Dcolumns, collapse = ", "),
          " FROM wb WHERE ", filter_clause, 
          " AND Genome IN ('", paste(gensel(), collapse = "','"), "')"
        )
        
        print(query)  # Debug query
        
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
      #nrUTable <- reactiveValues(nrow = 0L)
      uSelect<-eventReactive(input$selectGene, {
        selected <- getReactableState("STable", "selected")
        req(selected)
        pull(Sdata()[selected,colnames(Sdata())=="WBM_geneID"])
      },
      ignoreNULL = FALSE)
      observeEvent(input$selectGene,{
        sl<-dbGetQuery(wbdb,paste0("SELECT * FROM wb WHERE WBM_geneID LIKE CONCAT('%','",
                                   uSelect(),
                                   "','%')"))
        utable$x<-unique(rbind(utable$x,sl))
      })
      observeEvent(input$selectGene, {
        nrUTable$nrow=nrow(utable$x)
      })
      observeEvent(input$resetGene, {
        utable$x <-NULL
        utable$x <- tibble()
        nrUTable$nrow=nrow(utable$x)
      })
      output$uCartLabel<-renderText({
        paste0("Selected Genes (",nrUTable$nrow,")")})
      output$downloadAData <- downloadHandler(
        filename = function() {
          paste("WormBiome-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write_csv(STable(), file)
        }
      )
    })
    }

comparatorserv<-function(ida,wbdb, column_names, kegg, phylo,p_tree,getPal=getPalette,tibtree){
  moduleServer(
    ida,
    function(input, output, session) {
      #Kegg filter
      output$secondSelection <- renderUI({
        ns <- session$ns
        selectizeInput(ns("filter2"), "Filter Kegg by:",
                       choices =   unique(kegg %>% select(input$filter) %>% unique() %>% pull),
                       options = list(
                         placeholder = 'Please select an option below',
                         onInitialize = I('function() { this.setValue(""); }')
                       ),
                       selected = character(0),
                       multiple=T,)
      })
      #Taxonomy Filter
      output$TL.second <- renderUI({
        ns <- session$ns
        selectizeInput(ns("TL2"), "Filter Taxonomy by:",
                       choices =   unique(phylo %>% select(input$TL) %>% unique() %>% pull),
                       options = list(
                         placeholder = 'Please select an option below',
                         onInitialize = I('function() { this.setValue(""); }')
                       ),
                       selected = character(0),
                       multiple=T)
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
          dplyr::summarise(kegcount = n()) %>%
          ungroup() %>% 
          left_join(kegg, relationship = "many-to-many")
        
        return(as.data.frame(tt)) 
        })

      Rtable<-reactive(
        if(is_empty(input$filter2)){
          Ptable()
        }else{
          Ptable() %>% 
            filter(get(input$filter) %in% input$filter2)
        }
      )
      
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
            dplyr::summarise(kegcount = sum(kegcount)) %>% 
            ungroup() %>% 
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
            left_join(phylo)
          Keig1<- round(Pres2$eig[1]/sum(Pres2$eig)*100,2)
          Keig2<- round(Pres2$eig[2]/sum(Pres2$eig)*100,2)
          head(ptbl)
          colourCount=length(unique(Kplot$Genus))
          
          p2=ggplot(Kplot, aes(x=V1,
                               y=V2,
                               col=Genus,
                               text = paste(
                                 "Genome:", Genome,
                                 "Genus:",Genus
                               ))) +
            geom_point()+
            theme_bw()+
            xlab(paste("Dimension 1", Keig1, "%",sep=" "))+
            ylab(paste("Dimension 2", Keig2, "%",sep=" "))+
            ggtitle("Principal Coordinates Analysis of Kegg predictions")+
            scale_color_manual(values = getPal(colourCount), name="Genus")
        }
        plotly::ggplotly(p2)
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

