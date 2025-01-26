-- MySQL dump 10.13  Distrib 8.3.0, for macos14.2 (arm64)
--
-- Host: localhost    Database: wormbiome
-- ------------------------------------------------------
-- Server version	8.3.0

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!50503 SET NAMES utf8mb4 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `wb`
--

DROP TABLE IF EXISTS `wb`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `wb` (
  `Genome` text,
  `WBM_geneID` text,
  `Bakta_ID` text,
  `gapseq_ID` text,
  `IMG_ID` bigint DEFAULT NULL,
  `PATRIC_ID` text,
  `Prokka_ID` text,
  `Contig_name` text,
  `Bakta_end` int DEFAULT NULL,
  `Bakta_start` int DEFAULT NULL,
  `contig_Bakta` text,
  `gapseq_end` int DEFAULT NULL,
  `gapseq_start` int DEFAULT NULL,
  `contig_gapseq` text,
  `IMG_end` int DEFAULT NULL,
  `IMG_start` int DEFAULT NULL,
  `contig_IMG` text,
  `PATRIC_end` int DEFAULT NULL,
  `PATRIC_start` int DEFAULT NULL,
  `contig_PATRIC` text,
  `Prokka_end` int DEFAULT NULL,
  `Prokka_start` int DEFAULT NULL,
  `contig_Prokka` text,
  `Bakta_strand` text,
  `Bakta_score` text,
  `Bakta_type` text,
  `Bakta_frame` int DEFAULT NULL,
  `Bakta_product` text,
  `Bakta_Name` text,
  `Bakta_Gene` text,
  `Bakta_COG` text,
  `Bakta_SO` text,
  `Bakta_UniRef` text,
  `Bakta_RefSeq` text,
  `Bakta_UniParc` text,
  `Bakta_GO` text,
  `Bakta_EC` text,
  `Bakta_RFAM` text,
  `Bakta_NCBIFam` text,
  `Bakta_BlastRules` text,
  `Bakta_PFAM` text,
  `Bakta_VFDB` text,
  `Bakta_KO` text,
  `Bakta_Cazy` text,
  `gapseq_strand` text,
  `gapseq_type` text,
  `gapseq_frame` text,
  `gapseq_BiocycRxn` text,
  `gapseq_SeedID` text,
  `gapseq_substances` text,
  `gapseq_tc` text,
  `IMG_strand` text,
  `IMG_score` text,
  `IMG_type` text,
  `IMG_frame` int DEFAULT NULL,
  `IMG_product` text,
  `IMG_cog` text,
  `IMG_ko` text,
  `IMG_pfam` text,
  `IMG_signalp` text,
  `IMG_smart` text,
  `IMG_superfam` text,
  `IMG_tigrfam` text,
  `IMG_tmhmm` text,
  `PATRIC_strand` text,
  `PATRIC_score` text,
  `PATRIC_type` text,
  `PATRIC_frame` text,
  `PATRIC_superclass` text,
  `PATRIC_class` text,
  `PATRIC_subclass` text,
  `PATRIC_subsystem` text,
  `PATRIC_Pathway` text,
  `PATRIC_pathID` text,
  `PATRIC_SPproperty` text,
  `PATRIC_SPclassification` text,
  `Prokka_strand` text,
  `Prokka_score` text,
  `Prokka_type` text,
  `Prokka_frame` text,
  `Prokka_product` text,
  `Prokka_gene` text,
  `Prokka_EC_number` text,
  `Prokka_COG` text,
  `Prokka_KO` text,
  `Bakta_NCBIProtein` text,
  `gene_cluster_id` text,
  `PATRIC_product` text
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
/*!40101 SET character_set_client = @saved_cs_client */;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2025-01-22 16:30:15
