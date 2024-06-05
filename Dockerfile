FROM rocker/shiny-verse:4.2.2
ARG GAC

RUN apt-get update -y  && \
  apt-get upgrade -y &&\
  apt-get install -y apt-transport-https ca-certificates gnupg curl

RUN echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] http://packages.cloud.google.com/apt cloud-sdk main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list && curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key --keyring /usr/share/keyrings/cloud.google.gpg  add - && apt-get update -y && apt-get install google-cloud-cli -y


RUN rm -rf /srv/shiny-server/*
RUN R -q -e  "install.packages(c('labdsv','RColorBrewer','vegan','reactable','plyr','treeio','XML','DT','BiocManager','shinyjs','feather'),dependencies=TRUE, repos='http://cran.rstudio.com/')"\
  R -e "BiocManager::install(ask=FALSE)" \
  R -e "BiocManager::install('ggtree')"
WORKDIR /srv/shiny-server/
COPY ./website/ ./

WORKDIR /
RUN wget ftp://ftp.ncbi.nlm.nih.gov/blast/executables/blast+/2.13.0/ncbi-blast-2.13.0+-x64-linux.tar.gz &&\
  tar zxvf ncbi-blast-2.13.0+-x64-linux.tar.gz &&\
  mv ncbi-blast-2.13.0+ blast &&\
  rm ncbi-blast-2.13.0+-x64-linux.tar.gz
ENV PATH=/blast/bin:${PATH}

WORKDIR /srv/shiny-server/
COPY ${GAC} tmp.json
RUN  gcloud auth activate-service-account worm-reader@wormbiome-377822.iam.gserviceaccount.com \
   --key-file=tmp.json \
   --project=wormbiome-377822 &&\
  rm tmp.json
RUN gcloud storage cp -r gs://wormbiome_data/ ./ &&\
  mv wormbiome_data/* ./ &&\
  rm -rf wormbiome_data
