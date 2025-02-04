# Website Documentation

## Microbial Genome list

The **Microbial Genome** tab provides metadata related to bacterial strains in the Wormbiome database. This includes: 
- Taxonomic assignment of bacterial strains.
- Links to public repositories for genomic data. 
- Sampling information for each strain. 
- Details of the laboratory that isolated the strain.

## Wormbiome annotation process.

Our genomic annotations are derived from four tools: [Bakta](https://bakta.computational.bio/), [Prokka](https://github.com/tseemann/prokka), [BV-BRC](https://www.bv-brc.org/) (formerly PATRIC), and [IMG](https://img.jgi.doe.gov/cgi-bin/mer/main.cgi). 

The Wormbiome databse is created with the following key steps:
1. Annotations are combined into a multi-track database. 
2. Overlapping Annotations (80% overlap) from different pipelines are merged into a single master feature. 
3. Merged features retain tool-specific details and are assigned unique WormBiome IDs (e.g., `WBM_BH3_0000012`). 

A pan-genomic analysis is performed on the Bakta-based gene predictions with Anvio to identify clusters of annotated and hypothetical genes with similar sequences. Using the DIAMOND software, the Anvio pan-genomic pipeline starts by calculating the similarity between all predicted protein amino acid sequences in the GenBank files generated by our Bakta annotation pipeline. Then, the pipeline resolves gene clusters using the BLAST results via an MCL algorithm after discarding weak hits from the search results using an minbit heuristic (for more details see the Anvio website). Because our pan-genomic comparison includes distantly related microbial genomes, we used a loose MCL inflation parameter (2) to generate clusters not overly specific to microbial taxa.

## Available data

Our database currently regroups 87 different types of information listed as follows:

| Column Name             | Description                                                                                    |
|-------------------------|------------------------------------------------------------------------------------------------|
| WBM_geneID              | Unique Gene ID specific to the Wormbiome database                                               |
| Genome                  | Genome Strain ID                                                                                |
| gene_cluster_id         | Pangenome cluster identifier                                                                    |
| Contig_name             | Common contig name                                                                              |
| contig_Bakta            | Contig-level identifier assigned by Bakta                                                       |
| contig_gapseq           | Contig-level identifier assigned by GapSeq                                                      |
| contig_IMG              | Contig-level identifier assigned by IMG                                                         |
| contig_PATRIC           | Contig-level identifier assigned by PATRIC                                                      |
| contig_Prokka           | Contig-level identifier assigned by Prokka                                                      |
| Bakta_start             | Start position of gene prediction by Bakta                                                      |
| Bakta_end               | Stop position of gene prediction by Bakta                                                       |
| Bakta_frame             | Reading frame of gene prediction by Bakta                                                       |
| Bakta_strand            | Bakta gene prediction DNA strand (+/-)                                                          |
| Bakta_type              | Type of gene feature (e.g., CDS, tRNA)                                                          |
| Bakta_Gene              | Gene symbol assigned by Bakta                                                                   |
| Bakta_product           | Functional annotation of the gene product from Bakta                                            |
| Bakta_BlastRules        | NCBI [BlastRules](https://ftp.ncbi.nih.gov/pub/blastrules/) used by Bakta for gene annotation   |
| Bakta_Cazy              | Custom [CAZy](https://www.cazy.org/) annotation based on Bakta gene calling.                    |
| Bakta_COG               | Cluster of Orthologous Groups (COG) annotation from Bakta                                       |
| Bakta_EC                | Enzyme Commission (EC) number annotations                                                       |
| Bakta_GO                | Gene Ontology (GO) annotations                                                                  |
| Bakta_ID                | Bakta-generated unique identifier for the gene                                                  |
| Bakta_IS                | Insertion sequence (IS) element annotations from Bakta                                          |
| Bakta_KO                | Custom KEGG Orthology (KO) annotation based on Bakta gene calling                               |
| Bakta_NCBIFam           | [NCBI Protein family](https://www.ncbi.nlm.nih.gov/protfam) classification for the gene product |
| Bakta_NCBIProtein       | NCBI protein accession number associated with the gene                                          |
| Bakta_PFAM              | Protein family (PFAM) annotation                                                                |
| Bakta_RefSeq            | RefSeq accession for the gene product                                                           |
| Bakta_RFAM              | RNA family (RFAM) annotation                                                                    |
| Bakta_score             | Confidence score assigned to Bakta predictions                                                  |
| Bakta_SO                | Sequence Ontology term for the feature                                                          |
| Bakta_UniParc           | UniParc identifier for the protein                                                              |
| Bakta_UniRef            | UniRef identifier for the protein cluster                                                       |
| Bakta_VFDB              | Virulence Factor Database (VFDB) annotation                                                     |
| gapseq_start            | Start position of gene prediction by GapSeq                                                     |
| gapseq_end              | End position of gene prediction by GapSeq                                                       |
| gapseq_strand           | DNA strand for GapSeq prediction (+/-)                                                          |
| gapseq_frame            | Reading frame of gene prediction by GapSeq                                                      |
| gapseq_type             | Type of Feature predicted by GapSeq                                                             |
| gapseq_ID               | Unique identifier for GapSeq-predicted gene                                                     |
| gapseq_BiocycRxn        | BioCyc reaction identifier from GapSeq annotation                                               |
| gapseq_SeedID           | SEED subsystem identifier for GapSeq prediction                                                 |
| gapseq_substances       | Predicted metabolic substances associated with GapSeq gene                                      |
| gapseq_tc               | Transport classification (TCDB) for GapSeq gene                                                 |
| IMG_ID                  | Unique identifier for IMG-predicted gene                                                        |
| IMG_start               | Start position of gene prediction by IMG                                                        |
| IMG_end                 | End position of gene prediction by IMG                                                          |
| IMG_strand              | DNA strand for IMG prediction (+/-)                                                             |
| IMG_product             | Functional annotation of the gene product from IMG                                              |
| IMG_frame               | Reading frame of gene prediction by IMG                                                         |
| IMG_type                | Type of Feature predicted by IMG                                                                |
| IMG_cog                 | Cluster of Orthologous Groups (COG) annotation from IMG                                         |
| IMG_ko                  | KEGG Orthology (KO) annotation from IMG                                                         |
| IMG_pfam                | Protein family (PFAM) annotation from IMG                                                       |
| IMG_score               | Confidence score assigned to IMG predictions                                                    |
| IMG_signalp             | Signal peptide prediction by IMG                                                                |
| IMG_smart               | SMART domain annotation from IMG                                                                |
| IMG_superfam            | Superfamily annotation from IMG                                                                 |
| IMG_tigrfam             | TIGRFAM annotation from IMG                                                                     |
| IMG_tmhmm               | Transmembrane helix prediction by IMG                                                           |
| PATRIC_start            | Start position of gene prediction by PATRIC                                                     |
| PATRIC_end              | End position of gene prediction by PATRIC                                                       |
| PATRIC_strand           | DNA strand for PATRIC prediction (+/-)                                                          |
| PATRIC_frame            | Reading frame of gene prediction by PATRIC                                                      |
| PATRIC_ID               | Unique identifier for PATRIC-predicted gene                                                     |
| PATRIC_type             | Type of Feature predicted by PATRIC                                                             |
| PATRIC_product          | Functional annotation of the gene product from PATRIC                                           |
| PATRIC_class            | Classification of gene product by PATRIC                                                        |
| PATRIC_pathID           | Pathway identifier from PATRIC                                                                  |
| PATRIC_Pathway          | Pathway annotation from PATRIC                                                                  |
| PATRIC_score            | Confidence score assigned to PATRIC predictions                                                 |
| PATRIC_SPclassification | Subsystem classification from PATRIC                                                            |
| PATRIC_SPproperty       | Subsystem property from PATRIC                                                                  |
| PATRIC_subclass         | Subclass annotation from PATRIC                                                                 |
| PATRIC_subsystem        | Subsystem annotation from PATRIC                                                                |
| PATRIC_superclass       | Superclass annotation from PATRIC                                                               |
| Prokka_start            | Start position of gene prediction by Prokka                                                     |
| Prokka_end              | End position of gene prediction by Prokka                                                       |
| Prokka_strand           | DNA strand for Prokka prediction (+/-)                                                          |
| Prokka_frame            | Reading frame of gene prediction by Prokka                                                      |
| Prokka_ID               | Unique identifier for Prokka-predicted gene                                                     |
| Prokka_type             | Type of Feature predicted by Prokka                                                             |
| Prokka_gene             | Gene symbol assigned by Prokka                                                                  |
| Prokka_product          | Functional annotation of the gene product from Prokka                                           |
| Prokka_COG              | Cluster of Orthologous Groups (COG) annotation from Prokka                                      | 
| Prokka_EC_number        | Enzyme Commission (EC) number annotations from Prokka                                           |
| Prokka_KO               | KEGG Orthology (KO) annotation from Prokka                                                      |
| Prokka_score            | Confidence score assigned to Prokka predictions                                                 |

# Wormbiome Download information

All the data used for the Wormbiome database is publicly available.
**The Raw Data:**
- Genome assemblies are available on [NCBI](https://www.ncbi.nlm.nih.gov/). 
- IMG and PATRIC assemblies can be accessed on their respective websites.

**The Wormbiome Processed Data:** Custom annotation tables are publicly available on Zenodo: 
- [Consensus Database Entry](https://zenodo.org/records/10689575): Compiled consensus table of curated annotations. 
- Individual strain entries with all associated files generated by the different annotation pipelines.

Individual links to the different public repositories are available on the Microbial Genome tab. 

## Gene Search

The **Gene Search** tab allows text-based queries on the Wormbiome database.

### How to Use:
1. Enter a query in the search box and hit "Search."
   ![Search Example](.image1.png)
2. Narrow your search:
   - By Taxonomy: Select taxonomic level (e.g., *Genus*) and specific taxa (e.g., *Ochrobactrum*).
   - By Genome: Filter by strain ID.
   - By Column: Search specific fields like gene name or KEGG annotations.
  ![Filter Example](.image2.png)
Genes of interest can then be saved to a User Cart by ticking the gene in the list and clicking "Select Gene."

## Annotation Browser

The annotation browser allows the user to list all the genes specific to a bacteria or specific taxonomic group.

The annotation database option displays a default set of columns specific to the different annotation pipelines used for the Wormbiome database. Users can choose which column to display using the scrolling list on the left side panel.

Similarly to the Gene search option, users can either list all the genes of a specific bacterial strain or all the genes associated with specific microbial taxa.

## Tools

We offer two interactive tools to browse and analyze the genomes available in the Wormbiome database:
An annotation comparison tool called Compare Feature 
A custom blast search tool. 

### Compare Feature

The annotation comparison tool allows some simple comparisons between taxonomic groups or one to four custom groups.

### Blast

Users can perform sequence-based queries on the Wormbiome database to look for specific sequences of interest. 

## User Cart

The **User Cart** stores genes selected from the Gene Search or Annotation Browser.
The tab has three different displays.
**Overview Table**: Display the selected genes with their associated information. Users can choose which column to display
**Taxonomic Overview**: Visualize the distribution of selected genes across genomes.
**Annotation Summary**: Display the count of specific associated annotations (e.g., KEGG, CAZy, PFAM).

There are two export options:
- Download raw or displayed data.
- Export generated visualizations.

# Tutorials

## BiomProfiler 

:warning: This tool was designed to work for experiments where synthetic communities of sequenced strains were used and profiled with Amplicon sequencing. 

To predict defined community functional profiles, we build Biomprofiler, a bioinformatic pipeline that converts 16S rRNA amplicon-based community composition to a gene abundance profile. The ASV table prediction and reference sequence fasta file, together with the experiment metadata file, are then analyzed with BiomProfiler to predict the genetic potential profile of the different microbial communities. To use this pipeline, the user needs to provide a reference community for the pipeline to look for the bacteria in the amplicon dataset. 

![IMAGE](.Biomprofiler.png)

The BiomProfiler pipeline begins by creating a blast reference nucleotide database using a user-provided list of bacteria of the defined synthetic community used in the experiment. A nucleotide blast (Blastn) is used to query the amplicon sequence variant (ASV) sequences against the nucleotide database and return matches with a 99% minimum identity. Samples not present in the metadata file or with less than 1000 sequences were flagged and removed. Similarly, ASVs with no count were also flagged and removed. The pipeline parses the blast results and associates each ASV with specific genomes. Bacteria in the reference list with identical sequences are merged into the same "Genome unit" and processed as one. Reference bacteria with no 100% blast hits are then attributed to the closest blast result within a 99% similarity limit. If an ASV does not match any reference sequence, it is flagged as potential contamination.

The tutorial below explain how to transform a 16S rRNA amplicon project into a Genomic Potential prediction table.



## Metagenomic comparisons

This tutorial covers a pangenomic approach to compare closely related genomes you are interested in. The code below explain how to compre closely related *Enterobacteriaceae* genomes that have different phenotype when fed to *C. elegans*
This approach is based on the An open-source, community-driven analysis and visualization platform for microbial 'omics or [Anvio](https://anvio.org/).

### Before running

#### :warning: Disclaimer :warning:

There is no need to reinvent the wheel, the official Anvio tutorial [here](https://merenlab.org/2016/11/08/pangenomics-v2/) and [here](https://merenlab.org/2018/12/01/combining-annotation-sources-for-pan/) as well as the work from [Mike Lee](https://anvio.org/people/AstrobioMike/) are excellent and detailed resources on how to perform this type of pangenomic comparison. Check the link for step by step general tutorials.

#### Installing softwares

Head to this [page](https://anvio.org/install/) for anvio installation guide.

And we'll need a side set of script from [Mike Lee](https://merenlab.org/2018/12/01/combining-annotation-sources-for-pan/):

You can instal it with the code below
```{shell}
conda create -y -n bit -c conda-forge -c bioconda -c defaults -c astrobiomike bit
```


#### Preparing run

We run the Pangenomic analysis with our Bacta annotated genomes.

First place the name of genome of interest in a `Gene.list.txt` file

Our genomes:

```{text}
BH3
MYb71
MYb15
MYb49
MYb68
JUb45
JUb46
BIGb0125
```

Our genbank files are not compatible with the default anvio pipeline and we need to hack our way in. Thankfully, Mike Lee did all the job for us with his `bit` environment.

All Prokka gbk file are copied in the `GBK` folder and we create a `clean` folder for having the genbank corrected files stored in.

```{shell}
for i in $(cat Gene.list.txt);
    do cp /PATH/$i GBK/;
done
mkdir clean
```

In brief the script rename Bacta genbank contig name to unique contig per genomes

```{shell}
conda activate bit
for i in $(ls -d GBK/*);
  do a=$(echo $i | cut -f2 -d"/" |gsed "s/gbff\///g; s/\.gbff//g");
  echo $i $a;
  bit-genbank-locus-clean-slate -i $i -o clean/$a.gbk -w $a;
done
cat clean/*gbk> all_refs.gbff
conda deactivate
````

## Running Anvio

Anvio is a very comprehensive pipeline with many modules. We'll barrel through the pipeline here, for more information go to the Anvio website.

```{R}
conda activate anvio-8
```
Import all the information we have from our genbank files into compatible file format

```{R}
anvi-script-process-genbank -i all_refs.gbff --output-gene-calls all_refs_gene_calls.tsv --output-functions all_refs_functions.tsv --output-fasta all_refs.fa --include-locus-tags-as-functions
```
Create a contig database, with all the gene call information
```{R}
anvi-gen-contigs-database -f all_refs.fa -o contigs.db -n Wormbiome --external-gene-calls all_refs_gene_calls.tsv  --split-length -1 --num-threads 12
anvi-import-functions -c contigs.db -i all_refs_functions.tsv
```
Look for unique copy marker genes:
```
anvi-run-hmms -c contigs.db --num-threads 12
```

Create a collection file
```{R}
cd clean
for genome in $(ls *gbk | cut -f1 -d ".");
  do   grep "LOCUS" "$genome".gbk | tr -s " " "\t" | cut -f2 > "$genome"_contigs.tmp;
    for contig in $(cat "$genome"_contigs.tmp);
       do     echo "$genome";
     done > "$genome"_name.tmp;
   paste "$genome"_contigs.tmp "$genome"_name.tmp > "$genome"_for_cat.tmp;
done
cat *_for_cat.tmp > collection.tsv && rm *.tmp
cd ..
mv  clean/collection.tsv ./
anvi-profile -c contigs.db -o profile -S profile --blank-profile --min-contig-length 0 --skip-hierarchical-clustering
anvi-import-collection collection.tsv -c contigs.db -p profile/PROFILE.db -C Wormbiome --contigs-mode
```
Get the genome database and file ready:
```{R}
echo -e "name\tbin_id\tcollection_id\tprofile_db_path\tcontigs_db_path" > header.tmp

cut -f2 collection.tsv | uniq > name_and_bin_id.tmp
for i in $(cat name_and_bin_id.tmp); do echo "Wormbiome"; done > collection_id.tmp
for i in $(cat name_and_bin_id.tmp); do echo "$PWD/profile/PROFILE.db"; done > profile_db_path.tmp
for i in $(cat name_and_bin_id.tmp); do echo "$PWD/contigs.db"; done > contigs_db_path.tmp
paste name_and_bin_id.tmp name_and_bin_id.tmp collection_id.tmp profile_db_path.tmp contigs_db_path.tmp > body.tmp
cat header.tmp body.tmp > internal_genomes.tsv && rm *.tmp

anvi-gen-genomes-storage -i internal_genomes.tsv -o wormbiome-GENOMES.db --gene-caller NCBI_PGAP

```
Finally run the pangenome comparison
```{R}
anvi-pan-genome -g wormbiome-GENOMES.db \
               -n wormbiome-mcl \
               --num-threads 12 --mcl-inflation 10 
```
Get everything exported for downstream analysis
```{R}
anvi-script-add-default-collection -c contigs.db -p wormbiome-mcl/wormbiome-mcl-PAN.db -C Wormbiome
#Export gene clusters
anvi-summarize -p wormbiome-mcl/wormbiome-mcl-PAN.db -g wormbiome-GENOMES.db -C Wormbiome -o Project_summary
#Export gene names
anvi-export-gene-calls -c contigs.db --gene-caller NCBI_PGAP -o gene_call.txt --skip-sequence-reporting
```