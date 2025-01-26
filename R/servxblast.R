print("Loading Blast module")

#unique id creator
#taken from https://samfirke.com/2018/08/22/generating-unique-ids-using-r/

create_unique_ids <- function(n, seed_no = sample(1:100000, 1), char_len = 5){
  set.seed(seed_no)
  pool <- c(letters, LETTERS, 0:9)

  res <- character(n) # pre-allocating vector is much faster than growing it
  for(i in seq(n)){
    this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
    while(this_res %in% res){ # if there was a duplicate, redo
      this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
    }
    res[i] <- this_res
  }
  res
}

blastServer <-  function(ida, custom_db,wbdb,phylo){
  moduleServer(
    ida,
    function(input, output, session){
      ns <- session$ns
      
      # Reactive value to store the custom database ID
      temp_buid <- reactiveVal(NULL)
      
      # Generate UI for taxonomy filtering
      output$bsecond <- renderUI({
        createTaxonomyFilterUI(ns, phylo, input$btaxa, "BTL2")
      })
      
      # Blast processing and database creation
      blastresults <- eventReactive(input$blast, {
        withProgress(message = "Blasting...", value = 0, {
          # Gather input and set up temp file
          query <- input$query
          tmp <- tempfile(fileext = ".fa")
          
          # Determine the database to use
          if (input$bdb == "All") {
            db <- paste0(srvloc,"blast/All.ref.fa")
          } else {
            # Get genome of interest for custom blast DB
            bgensel <- if (is_empty(input$btaxa)) {
              phylo %>%
                filter(ID %in% input$bgenome) %>%
                select(ID) %>%
                pull()
            } else {
              phylo %>%
                filter(ID %in% input$bgenome | get(input$btaxa) %in% input$BTL2) %>%
                select(ID) %>%
                pull()
            }
            
            # Create unique ID for temp BLAST database
            buid <- create_unique_ids(1)
            temp_buid(buid)
            
            # Create temporary BLAST database
            temp_dir <- paste0(srvloc,"/tmp/", buid)
            system(paste0("mkdir -p ", temp_dir))
            system(paste0("cat ", paste0(srvloc,"fasta/", bgensel, ".fna", collapse = " "), " > ", temp_dir, "/", buid, ".fna"))
            system(paste0("makeblastdb -in ", temp_dir, "/", buid, ".fna -input_type fasta -title ", buid, " -dbtype nucl -out ", temp_dir, "/", buid))
            db <- paste0(temp_dir, "/", buid)
          }
          
          # Format the query for BLAST
          if (startsWith(query, ">")) {
            writeLines(query, tmp)
          } else {
            writeLines(paste0(">Query\n", query), tmp)
          }
          
          # Run BLAST command
          blast_cmd <- paste0(input$program, " -query ", tmp, " -db ", db, " -evalue ", input$eval, " -outfmt 5 -max_hsps 1 -max_target_seqs 10")
          bdata <- tryCatch({
            system(blast_cmd, intern = TRUE)
          }, error = function(e) {
            stop("BLAST failed: ", e$message)
          })
          
          return(bdata)
        })
      })
      
      # Observe BLAST results to trigger cleanup
      observeEvent(blastresults(), {
        buid <- temp_buid()
        if (!is.null(buid)) {
          temp_dir <- paste0(srvloc,"tmp/", buid)
          system(paste0("rm -rf ", temp_dir))
          temp_buid(NULL) # Reset buid
          cat("Temporary BLAST database removed:", temp_dir, "\n")
        }
      })

    #Now to parse the results...
    parsedresults <- reactive({
    req(blastresults())
      #print(head(blastresults()))
      blastxml=xmlParse(paste(blastresults(), collapse = "\n"))
      xmltop = xmlRoot(blastxml)

      #the first chunk is for multi-fastas
      results <- xpathApply(xmltop, '//Iteration',function(row){
        query_ID <- getNodeSet(row, 'Iteration_hits//Hit//Hit_id') %>% sapply(., xmlValue)
        ID <- getNodeSet(row, 'Iteration_hits//Hit//Hit_def') %>% sapply(., xmlValue)
        hit_length <- getNodeSet(row, 'Iteration_hits//Hit//Hit_len') %>% sapply(., xmlValue)
        bitscore <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_bit-score') %>% sapply(., xmlValue)
        eval <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_evalue') %>% sapply(., xmlValue)
        
        top <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_qseq') %>% sapply(., xmlValue)
        mid <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_midline') %>% sapply(., xmlValue)
        bottom <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_hseq') %>% sapply(., xmlValue)
        
        start <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_hit-from') %>% sapply(., xmlValue)
        stop <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_hit-to') %>% sapply(., xmlValue)
        
        
        cbind(query_ID,ID,hit_length,bitscore,eval,top,mid,bottom,start,stop)
      })
      #this ensures that NAs get added for no hits
      results <-  rbind.fill(lapply(results,function(y){as.data.frame((y),stringsAsFactors=FALSE)}))
      return(results)
    })

    #makes the datatable
    output$blastResults <- renderDataTable({
      if (is.null(parsedresults())) {
        tibble(Message = "No BLAST results available")
      } else {
        blasttable=parsedresults()[,1:5]
        colnames(blasttable)<-c("Query ID","Contig Hit ID", "Length", "Bit Score", "e-value")
        return(blasttable)
      }
    }, selection = "single")

    #this chunk gets the alignment information from a clicked row
    output$clicked <- renderTable({
      req(input$blastResults_rows_selected)
      clicked = input$blastResults_rows_selected
      tableout<- data.frame(parsedresults()[clicked,1:5])
      tableout <- t(tableout)
      names(tableout) <- c("")
      rownames(tableout) <- c("Query ID","Contig Hit ID", "Length", "Bit Score", "e-value")
      colnames(tableout) <- NULL
      data.frame(tableout)
    },rownames =T,colnames =F)

    #this chunk makes the alignments for clicked rows
    output$alignment <- renderText({
      req(input$blastResults_rows_selected)
      clicked = input$blastResults_rows_selected

      #split the alignments every 50 characters to get a "wrapped look"
      align=parsedresults() %>% select(top,mid,bottom)
      splits <- strsplit(gsub("(.{100})", "\\1,", align[clicked,1:3]),",")

      #paste them together with returns '\n' on the breaks
      split_out <- lapply(1:length(splits[[1]]),function(i){
        rbind(paste0("Q-",splits[[1]][i],"\n"),paste0("M-",splits[[2]][i],"\n"),paste0("H-",splits[[3]][i],"\n"))
      })
      unlist(split_out)
    })
    
    blast_data<-reactive({
      req(parsedresults())
      
      blastdt=parsedresults() %>% 
        select(ID,start,stop) %>% 
        mutate(start=as.numeric(start),
               stop=as.numeric(stop),
               orientation=ifelse(start<stop,"forward","reverse"),
               start.tmp=ifelse(orientation=="reverse",stop,start),
               stop.tmp=ifelse(orientation=="reverse",start,stop)) %>% 
        select(!c(start,stop)) %>% 
        dplyr::rename(start=start.tmp,stop=stop.tmp) %>% 
        select(ID,start,stop,orientation) %>% 
        as_tibble()
      
      return(blastdt)
    })
    
    blast_data_context<-reactive({
      req(blast_data())
     ts<-dbGetQuery(wbdb,paste0("SELECT *",
                                 " FROM wb WHERE Contig_name IN ('", paste0(gsub("_","_contig_",unique(blast_data()$ID)), collapse = "','"),"') ",
                                 "AND ",paste0("Bakta","_ID")," != ''")) %>% 
        mutate(Contig_name=gsub("_contig_","_",Contig_name))
      return(ts)
    })
    
    blast_plot_click<-reactive({
      req(input$blastResults_rows_selected)
      
      clicked = input$blastResults_rows_selected
      window_Size<-input$Blast_window_Size
      
      contextdata=blast_data_context() %>% 
        mutate(tmp=Bakta_start,
               tmp2=Bakta_end,
               stop=ifelse(tmp2>tmp,Bakta_end,Bakta_start),
               start=ifelse(tmp<tmp2,tmp,tmp2)) %>% 
        select(!c(tmp,tmp2))
      
      max_contig_length=max(contextdata %>% filter(Contig_name==pull(blast_data()[clicked,1])) %>% select(stop) %>% pull())
      start_check=if((pull(blast_data()[clicked,2])-window_Size)>0){pull(blast_data()[clicked,2])-window_Size}else{0}
      stop_check=if((pull(blast_data()[clicked,3])+window_Size)<max_contig_length){pull(blast_data()[clicked,3])+window_Size}else{max_contig_length}
      
      tmp=contextdata %>% 
        select(Contig_name,start,stop,Bakta_product,Bakta_type,Bakta_strand) %>% 
        unique() %>% 
        filter(Contig_name==pull(blast_data()[clicked,1])) %>% 
        filter(start>start_check) %>% 
        filter(stop<stop_check) %>% 
        mutate(orientation=ifelse(Bakta_strand=="+","forward","reverse"),
               Atype="Annotation") %>% 
        select(!Bakta_strand) %>% 
        dplyr::rename(ID=Contig_name,product=Bakta_product,type=Bakta_type)  %>% 
        arrange(start)
      
      tt=tmp %>% 
        rbind(blast_data()[clicked,] %>% 
                filter(ID==blast_data()[clicked,1]) %>% 
                mutate(product="BLAST result",type="BLAST",Atype="Blast_Result")) %>% 
        mutate(direction=ifelse(orientation=="forward",1,0))

      return(tt)
      })
    
    output$blast_plot=renderPlot({
      if (is.null(input$blastResults_rows_selected)) {
        ggplot()+
          geom_text(data=tibble(x=0,y=0,label="Select alignment"),
                    aes(x=x,y=y,label=label))+theme_void()
      }else{
        withProgress(message = "Rendering plot...", value = 0, {
          req(blast_plot_click())

          window_Size<-input$Blast_window_Size
          clicked = input$blastResults_rows_selected

          contextdata=blast_data_context() %>% 
            mutate(tmp=as.numeric(Bakta_start),
                   tmp2=as.numeric(Bakta_end),
                   stop=ifelse(tmp2>tmp,Bakta_end,Bakta_start),
                   start=ifelse(tmp<tmp2,tmp,tmp2)) %>% 
            select(!c(tmp,tmp2))
          
          max_contig_length=max(contextdata %>% filter(Contig_name==pull(blast_data()[clicked,1])) %>% select(stop) %>% pull())
          start_check=if((pull(blast_data()[clicked,2])-window_Size)>0){pull(blast_data()[clicked,2])-window_Size}else{0}
          stop_check=if((pull(blast_data()[clicked,3])+window_Size)<max_contig_length){pull(blast_data()[clicked,3])+window_Size}else{max_contig_length}
          
          blast_plot_click() %>% 
            ggplot(aes(xmin = start, xmax = stop, y = Atype, fill = type, forward=direction)) +
            geom_segment(x=start_check,xend=stop_check,y="Annotation",yend="Annotation")+
            gggenes::geom_gene_arrow()+
            theme_minimal()+
            scale_y_discrete(expand = c(1, 10), limits=c("Annotation","Blast_Result")) + #y label spacing to have blast result on top
            theme(
              axis.ticks.y = element_blank(),                
              axis.line.y = element_blank(),
              legend.position = "bottom",
              axis.text.y = element_blank(),
              panel.grid = element_blank()
            )+
            geom_text(data = blast_plot_click() %>% filter(Atype=="Annotation") %>% mutate(product=str_wrap(URLdecode(product), width=40,whitespace_only = FALSE)),aes(x = stop - ((stop-start)/2), y = -0.5, label = product, angle=90,hjust = 1, vjust=0.5), size=4)+
            geom_text(data = blast_plot_click() %>% filter(Atype!="Annotation"),aes(x = stop - ((stop-start)/2), y = 0.2, label = product, angle=90,hjust = -1),col="salmon")+
            labs(y="",x="Contig Position")
          })
    }})
    #Download buttons
    output$downloadBlastData <- downloadHandler(
      filename = function() {
        paste("WormBiome-Blast-Raw-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write_csv(parsedresults(),file)
      }
    )
    output$downloadBlastContext <- downloadHandler(
      filename = function() {
        paste("WormBiome-Blast-Context-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write_csv(blast_plot_click(),file)
      }
    )
    }
  )
}
