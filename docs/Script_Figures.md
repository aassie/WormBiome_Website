# Code for the home page alluvial plot

```{R}
#Load the data
phylo<- WORMBIOME PHYLOGENY DATA
Wb<- CONSENSUS WORMBIOME DATA

#Overly complicated color coding system

Red=c("#ff999b","#ff8082","#ff6668","#ff4d4f","#ff3336","#ff1a1d","#FF0004","#e60004","#cc0003","#b30003","#990002","#800002","#660002")
RedOrange=c(rev(c("#ff691a",	"#ff7933",	"#ff8a4d",	"#ff9b66",	"#ffac80")),"#FF5800","#e64f00",	"#cc4600",	"#b33e00",	"#993500",	"#802c00")
Orange=c(rev(c("#ffb21a","#ffba33","#ffc34d","#ffcb66","#ffd480")),"#FFA900","#e69800","#cc8700","#b37600","#996500","#805500")
Yellow=c(rev(c("#ffea1a","#ffed33","#ffef4d","#fff166","#fff480")),"#FFE800","#e6d100","#ccba00","#b3a200","#998b00","#807400")
green=c(rev(c("#e1ff1a","#e5ff33","#e8ff4d","#ebff66","#efff80")),"#DEFF00","#c8e600","#b2cc00","#9bb300","#859900","#6f8000")
green2=c(rev(c("#21ff1a","#39ff33","#52ff4d","#6bff66","#84ff80")),"#08FF00","#07e600","#06cc00","#06b300","#059900","#048000")
Teal=c(rev(c("#1affaf","#33ffb8","#4dffc1","#66ffca","#80ffd3")),"#00FFA6","#00e695","#00cc85","#00b374","#009964","#008053")
Blue1=c(rev(c("#1afffc","#33fffd","#4dfffd","#66fffd","#80fffe")),"#00FFFC","#00e6e3","#00ccca","#00b3b0","#009997","#00807e")
Blue2=c(rev(c("#1ab9ff","#33c1ff","#4dc8ff","#66d0ff","#80d8ff")),"#00B1FF","#009fe6","#008ecc","#007cb3","#006a99","#005980")
Blue3=c(rev(c("#1a72ff","#3381ff","#4d91ff","#66a1ff","#80b1ff")),"#0062FF","#0058e6","#004ecc","#0045b3","#003b99","#003180")
BluePur=c(rev(c("#551aff","#6833ff","#7b4dff","#8e66ff","#a180ff")),"#4200FF","#3b00e6","#3500cc","#2e00b3","#280099","#210080")
Purple=c(rev(c("#a01aff","#aa33ff","#b54dff","#bf66ff","#ca80ff")),"#9500FF","#8600e6","#7700cc","#6800b3","#590099","#4b0080")
Pink=c(rev(c("#ea1aff","#ed33ff","#ef4dff","#f166ff","#f480ff")),"#E800FF","#d100e6","#ba00cc","#a200b3","#8b0099","#740080")
Pink2=c(rev(c("#ff1a9b","#ff33a6","#ff4db1","#ff66bc","#ff80c8")),"#FF0090","#e60082","#cc0073","#b30065","#990056","#800048")

Class.color<-as.character(c(green[7],
                            Yellow[4],
                            Red[6],
                            green2[10],
                            Blue2[4],
                            Pink[6],
                            Purple[6]
))

names(Class.color)<-c("Alphaproteobacteria",
                      "Betaproteobacteria",
                      "Gammaproteobacteria",
                      "Bacilli",
                      "Actinomycetes",
                      "Flavobacteriia",
                      "Sphingobacteriia")

Order.color<-as.character(c(Orange[2],
                            green2[2],
                            Yellow[4],
                            green[5],
                            BluePur[6],
                            RedOrange[10],
                            Red[4],
                            Pink[6],
                            green[7],
                            "#317873",
                            green2[8],
                            Blue2[4],
                            Orange[10],
                            Blue1[4],
                            Orange[6],
                            "#7FFF00",
                            green[9],
                            Purple[6],
                            green[11],
                            "gray40",
                            RedOrange[8])
)

names(Order.color)<-c("Aeromonadales",
                      "Bacillales",
                      "Burkholderiales",
                      "Caulobacterales",
                      "Chitinophagales",
                      "Chromatiales",
                      "Enterobacterales",
                      "Flavobacteriales",
                      "Hyphomicrobiales",
                      "Kitasatosporales",
                      "Lactobacillales",
                      "Micrococcales",
                      "Moraxellales",
                      "Mycobacteriales",
                      "Pseudomonadales",
                      "Rhodobacterales",
                      "Rhodospirillales",
                      "Sphingobacteriales",
                      "Sphingomonadales",
                      "Unclassified",
                      "Xanthomonadales")

bphylo2<- tibble()
for (i in 1:length(unique(phylo$ID))){
  tmp2<-phylo %>% 
    filter(ID %in% str_split(unique(phylo$ID)[i],",")[[1]]) %>% 
    select(Class,Order,Genus) %>%
    unique()
  bphylo2<-rbind(bphylo2, tibble(Genome=unique(phylo$ID)[i],Class=tmp2$Class,Order=tmp2$Order,Genus=tmp2$Genus))
}
bphylo2<-bphylo2 %>% filter(!is.na(Class))

new.color<-tibble(Order=c("Aeromonadales",
                          "Bacillales",
                          "Burkholderiales",
                          "Caulobacterales",
                          "Chitinophagales",
                          "Chromatiales",
                          "Enterobacterales",
                          "Flavobacteriales",
                          "Hyphomicrobiales",
                          "Kitasatosporales",
                          "Lactobacillales",
                          "Micrococcales",
                          "Moraxellales",
                          "Mycobacteriales",
                          "Pseudomonadales",
                          "Rhodobacterales",
                          "Rhodospirillales",
                          "Sphingobacteriales",
                          "Sphingomonadales",
                          "Unclassified",
                          "Xanthomonadales"),
                  color=c(list(Orange[2]),
                          list(green2[c(2,4,6,8,10,11)]),
                          list(Yellow[c(2,1,4,6,8,10)]),
                          list(Yellow[11]),
                          list(BluePur[6]),
                          list(RedOrange[10]),
                          list(c(Red,Pink2)),
                          list(Pink[c(2,4,6)]),
                          list(green),
                          list("#317873"),
                          list(green2[c(1,3,5,7)]),
                          list(c(Blue2,Blue3)),
                          list(Orange[10]),
                          list(Blue1[c(2,4,6,8)]),
                          list(Orange[6]),
                          list("#7FFF00"),
                          list(Teal[c(2,4,6,8,10)]),
                          list(Purple[6]),
                          list(green[11]),
                          list("gray40"),
                          list(RedOrange[c(4,6,8)])),
)

GenomeCol2<-vector()
a=0
for(i in 1:(length(unique(bphylo2$Order)))){
  tmp  = bphylo2 %>% select(!Genome)%>% filter(Order==unique(bphylo2$Order)[i]) %>% select(Genus) %>% unique()
  for(j in 1:nrow(tmp)){
    tmpc = new.color %>% filter(Order==unique(bphylo2$Order)[i]) %>% select(color) %>% pull() %>% unlist()
    GenomeCol2[pull(tmp[j,1])] = tmpc[j]
  }
}

GenomeCol2["Missing"]="gray70"


GenomeCol2b<-GenomeCol2

# Generate base data

Newlist<-c("BH3","BIGb0102","BIGb0106","BIGb0112","BIGb0116","BIGb0117","BIGb0119","BIGb0124","BIGb0125","BIGb0132","BIGb0135","BIGb0138","BIGb0145","BIGb0149","BIGb0152","BIGb0156","BIGb0163","BIGb0164","BIGb0165","BIGb0176","BIGb0188","BIGb0189","BIGb0203","BIGb0204","BIGb0206","BIGb0215","BIGb0219","BIGb0220","BIGb0222","BIGb0227","BIGb0232","BIGb0234","BIGb0236","BIGb0239","BIGb0267","BIGb0270","BIGb0273","BIGb0277","BIGb0278","BIGb0281","BIGb0359","BIGb0381","BIGb0383","BIGb0389","BIGb0399","BIGb0404","BIGb0405","BIGb0408","BIGb0428","BIGb0435","BIGb0445","BIGb0450","BIGb0470","BIGb0473","BIGb0477","BIGb0494","BIGb0523","BIGb0525","BIGb0552","BIGb0558","BIGb0603","JUb101","JUb102","JUb104","JUb11","JUb111","JUb115","JUb117","JUb119","JUb134","JUb18","JUb20","JUb21","JUb23","JUb26","JUb28","JUb34","JUb39","JUb42","JUb44","JUb45","JUb52","JUb53","JUb54","JUb56","JUb58","JUb65","JUb66","JUb7","JUb78","JUb8","JUb83","JUb85","JUb87","JUb89","JUb90","JUb91","JUb96","BIGb0186","BIGb0367","BIGb0407","BIGb0471","BIGb0506","BIGb0611")

tb<-phylo %>% 
  dplyr::mutate(Sequencing=ifelse(ID %in% c(unique(Wb$Genome),"BIGb0186"),"Yes","No"),
         Sequenced=ifelse(Sequencing=="Yes","Old","No"),
         Sequenced=ifelse(Sequencing=="Yes"&ID %in% Newlist,"New",Sequenced),
         Sequenced=ifelse(ID %in% c("JUb44","JUb66"),"Old",Sequenced))


# Define custom node order (already done)
custom.order <- tb %>%
  filter(Sequenced != "No") %>%
  select(!Sequenced) %>% 
  arrange( match(Class, c("Alphaproteobacteria",
                    "Betaproteobacteria",
                    "Gammaproteobacteria",
                    "Bacilli",
                    "Actinomycetia",
                    "Flavobacteria",
                    "Sphingobacteria")),Order,Genus) %>%
  ggsankey::make_long( Class, Order, Genus) %>%
  select(node) %>%
  unique() %>%
  pull()

# Prepare nodes with proper factor levels
plotall.value <- tb %>%
  filter(Sequenced != "No") %>%
  select(-c(Name, Sequencing, Sequenced)) %>%
  arrange( Class, Family, Order, Genus, ID) %>%
  unique() %>%
  pivot_longer(!ID) %>%
  group_by(value) %>%
  dplyr::mutate(count = n()) %>%
  ungroup() %>%
  dplyr::rename(node = value, value = count) %>%
  select(!ID) %>% 
  unique() %>%
  arrange(match(node, custom.order)) %>%
  dplyr::mutate(
    node = factor(node, levels = custom.order),
    label = row_number() - 1
  )

# Ensure `plotall.sourcetarg` uses consistent levels
plotall.sourcetarg <- tb %>% 
  filter(Sequenced!="No") %>%
  arrange(Class, Family,Order, Genus) %>% 
  ggsankey::make_long(Class,Order,Genus)  %>% 
  unique() %>% 
  filter(!is.na(next_node)) %>%
  left_join(plotall.value %>% select(node, label) %>% dplyr::rename(source = label)) %>%
  left_join(plotall.value %>% select(node, label) %>% dplyr::rename(next_node = node, target = label)) %>%
  left_join(plotall.value %>% select(node, value) %>% dplyr::rename(next_node = node)) %>%
  arrange(source) %>%
  dplyr::mutate(node = factor(node, levels = custom.order)) %>%
  filter(!is.na(target))

# Ensure color mapping matches the node order
plotall.color <- as.data.frame(c(Class.color, Order.color, GenomeCol2b)) %>%
  dplyr::rename_with(~ "color", .cols = 1) %>%
  rownames_to_column("node") %>%
  arrange(match(node, custom.order)) %>%
  dplyr::mutate(node = factor(node, levels = custom.order)) %>%
  filter(!is.na(node))

# Sankey Plot
alluv=plotly::plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = plotall.value$node, # Match custom order
    color = plotall.color$color,
    pad = 5,
    thickness = 40,
    line = list(
      color = "black",
      width = 0
    ),
    hoverinfo = "none"
  ),
  
  link = list(
    source = plotall.sourcetarg$source,
    target = plotall.sourcetarg$target,
    value = plotall.sourcetarg$value
  )
) %>%
  plotly::layout(
    title = "WormBiome Genome Taxonomic Distribution",
    font = list(
      size = 10
    )
  )

write_rds(alluv, OUTPUTFOLDER)
```