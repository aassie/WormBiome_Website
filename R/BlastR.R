require(XML)
library(plyr)
library(dplyr)
library(DT)

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

blastServer <-  function(ida, custom_db,wb,phylo){
  moduleServer(
    ida,
    function(input, output, session){
      ns <- session$ns
      output$bsecond <- renderUI({
        selectizeInput(ns("BTL2"), "Filter Taxonomy by:",
                       choices =   unique(phylo %>% select(input$btaxa) %>% unique() %>% pull),
                       options = list(
                         placeholder = 'Please select an option below',
                         onInitialize = I('function() { this.setValue(""); }')
                       ),multiple=T)
      })
      output$Blast_outputValue <- renderText({
        paste("Window size:", input$Blast_window_Size)
      })
      #The great idea of a custom blast db creator
      blastresults <- eventReactive(input$blast, {
        withProgress(message = "Blasting...", value = 0, {
        #gather input and set up temp file
        query <- input$query
        tmp <- tempfile(fileext = ".fa")
        #if else chooses the right database
        if (input$bdb == "All"){
          db <- c("./blast/All.ref.fa")
          remote <- c("")
          }else{
            #get the genome of interest
            bgensel<-reactive(if(is_empty(input$btaxa)){
              phylo %>%
                filter(ID %in% input$bgenome)%>% select(ID) %>% pull()
              }else{
                phylo %>%
                  filter(ID %in% input$bgenome|get(input$btaxa) %in% input$BTL2)%>% select(ID) %>% pull()
                })
            buid<-create_unique_ids(1)
            #create temporary blastdb
            system(paste0("mkdir tmp/",buid))
            system(paste0("cat ", paste0("./fasta/",bgensel(),".fna", collapse = " ")," > ./tmp/",buid,"/", buid,".fna", collapse = " "))
            system(paste0("/blast/bin/makeblastdb -in ", "./tmp/",buid,"/", buid,".fna -input_type fasta -title ",buid, " -dbtype nucl -out ./tmp/",buid,"/",buid))
            db <- paste0("./tmp/",buid,"/",buid)
            }
        #this makes sure the fasta is formatted properly
        if (startsWith(query, ">")){
          writeLines(query, tmp)
          } else {
            writeLines(paste0(">Query\n",query), tmp)
          }
        blast_cmd=paste0("/Users/m3thyl/miniforge3/envs/ncbihack/bin/",input$program," -query ",tmp," -db ",db," -evalue ",input$eval," -outfmt 5 -max_hsps 1 -max_target_seqs 10 ")
        #calls the blast
        #Can add a remote variable at the end of the paste0 to get a nr db for example
        bdata <- tryCatch({
          system(blast_cmd, intern = TRUE)
          }, error = function(e) {
            stop("BLAST failed: ", e$message)
          })
        
        return(bdata)
        })}, ignoreNULL= T)
        
      #clear tmp db files
        observeEvent(input$blast, {
          if (input$bdb != "All" && exists("buid")) {
            system(paste0("rm -rf ./tmp/", buid))
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
        query_ID <- getNodeSet(row, 'Iteration_query-def') %>% sapply(., xmlValue)
        hit_IDs <- getNodeSet(row, 'Iteration_hits//Hit//Hit_id') %>% sapply(., xmlValue)
        hit_length <- getNodeSet(row, 'Iteration_hits//Hit//Hit_len') %>% sapply(., xmlValue)
        bitscore <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_bit-score') %>% sapply(., xmlValue)
        eval <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_evalue') %>% sapply(., xmlValue)
        cbind(query_ID,hit_IDs,hit_length,bitscore,eval)
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
        parsedresults()
      }
    }, selection = "single")

    #this chunk gets the alignemnt information from a clicked row
    output$clicked <- renderTable({
      req(input$blastResults_rows_selected)
      blastxml=xmlParse(paste(blastresults(), collapse = "\n"))
      xmltop = xmlRoot(blastxml)
      clicked = input$blastResults_rows_selected
      tableout<- data.frame(parsedresults()[clicked,])

      tableout <- t(tableout)
      names(tableout) <- c("")
      rownames(tableout) <- c("Query ID","Hit ID", "Length", "Bit Score", "e-value")
      colnames(tableout) <- NULL
      data.frame(tableout)
    },rownames =T,colnames =F)

    #this chunk makes the alignments for clicked rows
    output$alignment <- renderText({
      #print("I am running")
      req(input$blastResults_rows_selected)
      blastxml=xmlParse(paste(blastresults(), collapse = "\n"))
      xmltop = xmlRoot(blastxml)

      clicked = input$blastResults_rows_selected

      #loop over the xml to get the alignments
      align <- xpathApply(xmltop, '//Iteration',function(row){
        top <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_qseq') %>% sapply(., xmlValue)
        mid <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_midline') %>% sapply(., xmlValue)
        bottom <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_hseq') %>% sapply(., xmlValue)
        rbind(top,mid,bottom)
      })

      #split the alignments every 50 carachters to get a "wrapped look"
      alignx <- do.call("cbind", align)
      splits <- strsplit(gsub("(.{50})", "\\1,", alignx[1:3,clicked]),",")

      #paste them together with returns '\n' on the breaks
      split_out <- lapply(1:length(splits[[1]]),function(i){
        rbind(paste0("Q-",splits[[1]][i],"\n"),paste0("M-",splits[[2]][i],"\n"),paste0("H-",splits[[3]][i],"\n"))
      })
      unlist(split_out)
    })
    
    blast_data<-reactive({
      req(blastresults())
      blastxml=xmlParse(paste(blastresults(), collapse = "\n"))
      xmltop = xmlRoot(blastxml)
      
      extractXML <- xpathApply(xmltop, '//Iteration',function(row){
        ID <- getNodeSet(row, 'Iteration_hits//Hit/Hit_id') %>% sapply(., xmlValue)
        start <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_hit-from') %>% sapply(., xmlValue)
        stop <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_hit-to') %>% sapply(., xmlValue)
        rbind(ID,start,stop)
      })
        
      blastdt=reduce(extractXML, as.data.frame) %>%
        t() %>% 
        as_tibble() %>% 
        mutate(start=as.numeric(start),
               stop=as.numeric(stop),
               orientation=ifelse(start<stop,"forward","reverse"),
               tmp=ifelse(orientation=="reverse",start,stop),
               tmp2=ifelse(orientation=="reverse",stop,start),
               start=tmp2,
               stop=tmp) %>% 
        select(!c(tmp,tmp2))

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
      
      max_contig_length=max(blast_data_context() %>% filter(Contig_name==pull(blast_data()[clicked,1])) %>% select(Bakta_end) %>% pull())
      start_check=if(blast_data()[clicked,2]-window_Size>0){pull(blast_data()[clicked,2]-window_Size)}else{0}
      stop_check=if(blast_data()[clicked,3]+window_Size<max_contig_length){pull(blast_data()[clicked,3]+window_Size)}else{max_contig_length}
      

      tmp=blast_data_context() %>% 
        select(Contig_name,Bakta_start,Bakta_end,Bakta_product,Bakta_type,Bakta_strand) %>% 
        unique() %>% 
        filter(Contig_name==pull(blast_data()[clicked,1])) %>% 
        filter(Bakta_start>start_check) %>% 
        filter(Bakta_end<stop_check) %>% 
        mutate(orientation=ifelse(Bakta_strand=="+","forward","reverse"),
               Atype="Annotation") %>% 
        select(!Bakta_strand) %>% 
        dplyr::rename(ID=Contig_name,start=Bakta_start,stop=Bakta_end,product=Bakta_product,type=Bakta_type)  %>% 
        arrange(start)
      
      tt=tmp %>% 
        rbind(blast_data()[clicked,] %>% 
                filter(ID==blast_data()[clicked,1]) %>% 
                mutate(product="BLAST result",type="BLAST",Atype="Blast_Result")) %>% 
        mutate(direction=ifelse(orientation=="forward",1,0))

      return(tt)
      })
    
    output$blast_plot=renderPlot({withProgress(message = "Rendering plot...", value = 0, {
      if(is.null(input$blastResults_rows_selected)){
        ggplot()+
          geom_text(data=tibble(x=0,y=0,label="Select alignment"),
                    aes(x=x,y=y,label=label))+theme_void()
      }else{
        window_Size<-input$Blast_window_Size
        clicked = input$blastResults_rows_selected
        
        max_contig_length=max(blast_data_context() %>% filter(Contig_name==pull(blast_data()[clicked,1])) %>% select(Bakta_end) %>% pull())
        start_check=if(pull(blast_data()[clicked,2])-window_Size>0){pull(blast_data()[clicked,2])-window_Size}else{0}
        stop_check=if(pull(blast_data()[clicked,3])+window_Size<max_contig_length){pull(blast_data()[clicked,3])+window_Size}else{max_contig_length}
        
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
        }
    })})
    }
  )
}
