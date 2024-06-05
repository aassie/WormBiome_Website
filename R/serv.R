genelistserv<-function(ida, data, phylo,utable,nrUTable){
  moduleServer(
    ida,
    function(input, output, session) {
      wbdb <- dbConnect(
        RMariaDB::MariaDB(),
        host = "127.0.0.1",
        port = '3306',
        user = "wormreader",
        Sys.getenv("KEY1"),
        dbname = "wormbiome"
      )
      dataCname=as.vector(dbGetQuery(wbdb, "SELECT `COLUMN_NAME` 
        FROM `INFORMATION_SCHEMA`.`COLUMNS` 
        WHERE `TABLE_SCHEMA`='wormbiome'
        AND `TABLE_NAME`='wb'"))
      dbDisconnect(wbdb)
      annotation=reactive(as.vector(unlist(dataCname[str_detect(dataCname, pattern = input$variable)])))
      gensel<-reactive(phylo %>% 
                         filter(ID %in% input$genome|Genus %in% input$genus) %>% 
                         select(ID) %>% pull())
      
      # Create the search result table
      Rtable<-reactive({
        wbdb <- dbConnect(
          RMariaDB::MariaDB(),
          host = "127.0.0.1",
          port = '3306',
          user = "wormreader",
          Sys.getenv("KEY1"),
          dbname = "wormbiome"
        )
        print(paste0("SELECT WB_geneID, Genome, ",paste0(annotation(), collapse = ", "),
                     " FROM wb WHERE Genome IN ('", paste0(gensel(), collapse = "','"),"') ",
                     "AND ",paste0(input$variable,"_ID")," != ''"))
        tt<-dbGetQuery(wbdb,paste0("SELECT WB_geneID, Genome, ",paste0(annotation(), collapse = ", "),
                                              " FROM wb WHERE Genome IN ('", paste0(gensel(), collapse = "','"),"') ",
                                              "AND ",paste0(input$variable,"_ID")," != ''"))
        dbDisconnect(wbdb)
        return(tt)
        })
      #Table for the UI
      output$data <- renderReactable({
        reactable(Rtable(),
                  searchable = TRUE,
                  filterable = TRUE,
                  selection="multiple",
                  minRows = 20,
                  defaultPageSize = 20)
      })
      #nrUTable <- reactiveValues(nrow = 0L)
      uSelect<-eventReactive(input$selectGene, {
        selected <- getReactableState("data", "selected")
        req(selected)
        Rtable()[selected,colnames(Rtable())=="WB_geneID"]
      },
      ignoreNULL = FALSE)
      observeEvent(input$selectGene,{
        print(uSelect())
        wbdb <- dbConnect(
          RMariaDB::MariaDB(),
          host = "127.0.0.1",
          port = '3306',
          user = "wormreader",
          Sys.getenv("KEY1"),
          dbname = "wormbiome"
        )
        sl<-dbGetQuery(wbdb,paste0("SELECT * FROM wb WHERE WB_geneID IN ('",
                                   paste0(uSelect(), collapse = "','"),
                                   "')"))
        dbDisconnect(wbdb)
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

userGeneCartserv<-function(ida, utable,data, phylo,nrUTable){
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

genseSearchServ<-function(ida, data, phylo,utable,ugenome,nrUTable){
  moduleServer(
    ida,
    function(input, output, session) {
      wbdb <- dbConnect(
        RMariaDB::MariaDB(),
        host = "127.0.0.1",
        port = '3306',
        user = "wormreader",
        Sys.getenv("KEY1"),
        dbname = "wormbiome"
      )
      dataCname=unlist(dbGetQuery(wbdb, "SELECT `COLUMN_NAME` 
        FROM `INFORMATION_SCHEMA`.`COLUMNS` 
        WHERE `TABLE_SCHEMA`='wormbiome'
        AND `TABLE_NAME`='wb'"))
      dbDisconnect(wbdb)
      #column selection
      annotation=reactive(as.vector(dataCname[str_detect(dataCname, pattern = AnotTrack())]))

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
      output$searchSecond <- renderUI({
        ns <- session$ns
        selectizeInput(ns("filter2"), "Search in:",
                       choices =   annotation(),
                       options = list(
                         placeholder = 'Please select an option below',
                         onInitialize = I('function() { this.setValue(""); }')
                       ),
                       selected = character(0),
                       multiple=T,)
      })
      
      qsearch<-eventReactive(input$actionSearch,{
        input$geneSearch
      }, ignoreNULL = FALSE,
      )
      AnotTrack=eventReactive(input$actionSearch,{
        input$Svariable
      }, ignoreNULL = FALSE,
      )
      Fil2=eventReactive(input$actionSearch,{
        input$filter2
      }, ignoreNULL = FALSE,
      )
      #Some statistics
      output$uSearchDesc<-renderText({
        paste0("Your search includes ",nrow(Sdata())," gene",ifelse(nrow(Sdata())>1,"s","")," from ",length(unique(Sdata()$Genome))," genome", ifelse(length(unique(Sdata()$Genome))>1,"s",""))
      })
      Sdata<-reactive({
        #print(annotation())
        print(paste("activated for:", qsearch()))
        if(qsearch()!="Search"){
          if(length(Fil2())==0){
            wbdb <- dbConnect(
              RMariaDB::MariaDB(),
              host = "127.0.0.1",
              port = '3306',
              user = "wormreader",
              Sys.getenv("KEY1"),
              dbname = "wormbiome"
            )
          st<-dbGetQuery(wbdb,paste0("SELECT WB_geneID, Genome, ",
                                     paste0(annotation(), collapse = ", "),
                                     " FROM wb WHERE CONCAT_WS('',", 
                                     paste0(annotation(), collapse = ",") ,
                                     ") LIKE CONCAT('%','",
                                     qsearch(),
                                     "','%') AND Genome IN ('", 
                                     paste0(gensel(), collapse = "','"),"')"))
          dbDisconnect(wbdb)
          return(st)
          } 
          else {
            wbdb <- dbConnect(
              RMariaDB::MariaDB(),
              host = "127.0.0.1",
              port = '3306',
              user = "wormreader",
              Sys.getenv("KEY1"),
              dbname = "wormbiome"
            )
            st<-dbGetQuery(wbdb,paste0("SELECT WB_geneID, Genome, ",
                                       paste0(annotation(), collapse = ", "),
                                       " FROM wb WHERE ", 
                                       input$filter2 ,
                                       " LIKE CONCAT('%','",
                                       qsearch(),
                                       "','%') AND Genome IN ('", 
                                       paste0(gensel(), collapse = "','"),"')"))
            
            dbDisconnect(wbdb)
            return(st)
          }
        } else{
          data.frame(Example="")
        }
        })
      
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
        pull(Sdata()[selected,colnames(Sdata())=="WB_geneID"])
      },
      ignoreNULL = FALSE)
      observeEvent(input$selectGene,{
        wbdb <- dbConnect(
          RMariaDB::MariaDB(),
          host = "127.0.0.1",
          port = '3306',
          user = "wormreader",
          Sys.getenv("KEY1"),
          dbname = "wormbiome"
        )
        sl<-dbGetQuery(wbdb,paste0("SELECT * FROM wb WHERE WB_geneID LIKE CONCAT('%','",
                                   uSelect(),
                                   "','%')"))
        dbDisconnect(wbdb)
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

comparatorserv<-function(ida, data, kegg, phylo,p_tree,getPal=getPalette,tibtree){
  moduleServer(
    ida,
    function(input, output, session) {
      #Get database column names
      wbdb <- dbConnect(
        RMariaDB::MariaDB(),
        host = "127.0.0.1",
        port = '3306',
        user = "wormreader",
        Sys.getenv("KEY1"),
        dbname = "wormbiome"
      )
      dataCname=unlist(dbGetQuery(wbdb, "SELECT `COLUMN_NAME` 
        FROM `INFORMATION_SCHEMA`.`COLUMNS` 
        WHERE `TABLE_SCHEMA`='wormbiome'
        AND `TABLE_NAME`='wb'"))
      dbDisconnect(wbdb)
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
      annotation=reactive(as.vector(dataCname[str_detect(dataCname, pattern = input$anot)]))
      #Genera selection
      gensel<-reactive(if(is_empty(input$TL)){
        phylo %>% 
          filter(ID %in% input$genome)%>% select(ID) %>% pull()
      }else{
        phylo %>% 
          filter(ID %in% input$genome|get(input$TL) %in% input$TL2)%>% select(ID) %>% pull()
      })
      
      koname=reactive(paste0(input$anot,"_KO"))
      
      Ptable<-reactive({
        dbquery= paste0("SELECT WB_geneID, Genome, ",
                        paste0(annotation(), collapse = ", "),
                        " FROM wb WHERE Genome IN ('", paste0(gensel(), collapse = "','"),"') ",
                        "AND ",paste0(input$anot,"_ID")," != ''")
        #print(paste0("dbGetQuery(",data," ",dbquery,")"))
        wbdb <- dbConnect(
          RMariaDB::MariaDB(),
          host = "127.0.0.1",
          port = '3306',
          user = "wormreader",
          Sys.getenv("KEY1"),
          dbname = "wormbiome"
        )
        tt<-dbGetQuery(wbdb,dbquery) %>%
          mutate(KO=ifelse(is.na(get(koname())),
                           "No Annotations",
                           get(koname())),
                 KO=strsplit(KO,"\\|")) %>%
          unnest(KO) %>%
          group_by(Genome,KO) %>% 
          dplyr::summarise(kegcount=n()) %>%
          ungroup() %>% 
          left_join(kegg, relationship = "many-to-many")
        dbDisconnect(wbdb)
        return(tt)
      })

      Rtable<-reactive(
        if(is_empty(input$filter2)){
          Ptable()
        }else{
          Ptable() %>% 
            filter(get(input$filter) %in% input$filter2)
        }
      )
      output$data <- renderPlot({
        if(is_empty(gensel())){
          ggplot() +
            theme_void() +
            geom_text(aes(0,0,label="Please select a genome")) +
            xlab(NULL)
        }else{
          klvl=sym(input$kolevel)
          Rtable()%>% 
            left_join(phylo, by=c("Genome"="ID")) %>% 
            ggplot(aes(x=Genome,y=kegcount,fill=!!klvl))+
            geom_bar(alpha=1,stat="identity", position="fill")+
            theme_minimal()+
            theme(axis.text.x = element_text(angle=90))+
            facet_wrap(vars(get(input$TL)), scales = "free")+
            guides(fill=guide_legend(ncol=1))+
            ylab("Percent")
        }},
        execOnResize = TRUE)
      output$pcoa <- renderPlot(
        if(length(gensel())<2){
          ggplot() +
            theme_void() +
            geom_text(aes(0,0,label="Please select two genome or more")) +
            xlab(NULL)
        }else{
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
          
          ggplot(Kplot, aes(x=V1, y=V2, col=Genus)) +
            geom_point()+
            theme_bw()+
            xlab(paste("Dimension 1", Keig1, "%",sep=" "))+
            ylab(paste("Dimension 2", Keig2, "%",sep=" "))+
            ggtitle("Principal Coordinates Analysis of Kegg predictions")+
            scale_color_manual(values = getPal(colourCount), name="Genus")
        }
      )
      
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

