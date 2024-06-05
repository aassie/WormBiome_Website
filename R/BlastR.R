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

      #The great idea of a custom blast db creator
      blastresults <- eventReactive(input$blast, {
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

      #calls the blast
        #Can add a remote variable at the end of the paste0 to get a nr db for example
      bdata <- system(paste0("/blast/bin/",input$program," -query ",tmp," -db ",db," -evalue ",input$eval," -outfmt 5 -max_hsps 1 -max_target_seqs 10 "), intern = T)
      xmlParse(bdata)
    }, ignoreNULL= T)

      #clear tmp db files
      reactive(if (input$bdb != "All"&exists("buid")){
        system(paste0("rm -rf ./tmp/",buid))
      })

    #Now to parse the results...
    parsedresults <- reactive({
      if (is.null(blastresults())){}
      else {
        xmltop = xmlRoot(blastresults())

        #the first chunk is for multi-fastas
        results <- xpathApply(blastresults(), '//Iteration',function(row){
          query_ID <- getNodeSet(row, 'Iteration_query-def') %>% sapply(., xmlValue)
          hit_IDs <- getNodeSet(row, 'Iteration_hits//Hit//Hit_id') %>% sapply(., xmlValue)
          hit_length <- getNodeSet(row, 'Iteration_hits//Hit//Hit_len') %>% sapply(., xmlValue)
          bitscore <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_bit-score') %>% sapply(., xmlValue)
          eval <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_evalue') %>% sapply(., xmlValue)
          cbind(query_ID,hit_IDs,hit_length,bitscore,eval)
        })
        #this ensures that NAs get added for no hits
        results <-  rbind.fill(lapply(results,function(y){as.data.frame((y),stringsAsFactors=FALSE)}))
      }
    })

    #makes the datatable
    output$blastResults <- renderDataTable({
      if (is.null(blastresults())){
      } else {
        parsedresults()
      }
    }, selection="single")

    #this chunk gets the alignemnt information from a clicked row
    output$clicked <- renderTable({
      if(is.null(input$blastResults_rows_selected)){}
      else{
        xmltop = xmlRoot(blastresults())
        clicked = input$blastResults_rows_selected
        tableout<- data.frame(parsedresults()[clicked,])

        tableout <- t(tableout)
        names(tableout) <- c("")
        rownames(tableout) <- c("Query ID","Hit ID", "Length", "Bit Score", "e-value")
        colnames(tableout) <- NULL
        data.frame(tableout)
      }
    },rownames =T,colnames =F)

    #this chunk makes the alignments for clicked rows
    output$alignment <- renderText({
      print("I am running")
      if(is.null(input$blastResults_rows_selected)){}
      else{
        xmltop = xmlRoot(blastresults())

        clicked = input$blastResults_rows_selected

        #loop over the xml to get the alignments
        align <- xpathApply(blastresults(), '//Iteration',function(row){
          top <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_qseq') %>% sapply(., xmlValue)
          mid <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_midline') %>% sapply(., xmlValue)
          bottom <- getNodeSet(row, 'Iteration_hits//Hit//Hit_hsps//Hsp//Hsp_hseq') %>% sapply(., xmlValue)
          rbind(top,mid,bottom)
        })

        #split the alignments every 40 carachters to get a "wrapped look"
        alignx <- do.call("cbind", align)
        splits <- strsplit(gsub("(.{40})", "\\1,", alignx[1:3,clicked]),",")

        #paste them together with returns '\n' on the breaks
        split_out <- lapply(1:length(splits[[1]]),function(i){
          rbind(paste0("Q-",splits[[1]][i],"\n"),paste0("M-",splits[[2]][i],"\n"),paste0("H-",splits[[3]][i],"\n"))
        })
        unlist(split_out)
      }
    })
    }
  )
}
