## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
library(VariantAnnotation) 

## ----eval=FALSE---------------------------------------------------------------
# if (!require("BiocManager"))
#     install.packages("BiocManager")
# BiocManager::install("VariantAnnotation")

## ----create_snv---------------------------------------------------------------
# Create a GRanges object for position
pos <- GRanges(seqnames = "chr1", ranges = IRanges(start = 123456, width = 1), strand = "+")

#create an snv
snv <- SNV(
  variantID = "rs123",
  position = pos,
  refAllele = "A",
  altAllele = "T",
  geneSymbol = "BRCA1",
  geneType = "protein-coding",
  effect = "missense",
  clinicalRelevance = "Pathogenic",
  ensemblID = "ENST00000357654",
  hgvsp = "p.Arg175His"
)

snv

## ----create_dnv---------------------------------------------------------------
# Create a GRanges object for position
pos2 <- GRanges(seqnames = "chr2", ranges = IRanges(start = 200, width = 2))

#create a DNV
dnv <- DNV(
  variantID = "dnv1",
  position = pos2,
  refAllele = "AG",
  altAllele = "TC",
  geneSymbol = "TP53",
  geneType = "protein-coding",
  effect = "nonsense",
  clinicalRelevance = "Likely pathogenic",
  ensemblID = "ENST00000269305",
  hgvsp = "p.Trp53*"
)

dnv

## ----accessors----------------------------------------------------------------
# Get alternative allele
altAllele(snv)

# Get amino acid position
aamutation(snv)

# Get variant effect
effect(snv)

#Get aa mutation position
aamutation(snv)

## ----setAllele----------------------------------------------------------------
# Create a GRanges object for position
pos <- GRanges(seqnames = "chr1", ranges = IRanges(start = 123456, width = 1), strand = "+")

#create a variant
snv <- SNV(
  variantID = "rs123",
  position = pos,
  refAllele = "A",
  altAllele = "T",
  geneSymbol = "BRCA1",
  geneType = "protein-coding",
  effect = "missense",
  clinicalRelevance = "Pathogenic",
  ensemblID = "ENST00000357654",
  hgvsp = "p.Arg175His"
)

#setting a new allele
variant <- altAlleleSet(snv, "G")
variant

## ----create_insertion---------------------------------------------------------
# Create a GRanges object for a range
pos_ins <- GRanges(seqnames = "chr17",
                   ranges = IRanges(start = 41276045, end = 41276045),
                   strand = "+")
#crate an insertion
ins <- Insertion(
  variantID = "COSM476",
  position = pos_ins,
  altAllele = "AG",
  geneSymbol = "BRCA1",
  geneType = "protein-coding",
  effect = "frameshift_variant",
  clinicalRelevance = "Pathogenic",
  ensemblID = "ENST00000357654",
  hgvsp = "p.S1615fs"
)

ins

## ----create_deletion----------------------------------------------------------
# Create a GRanges object for a range
pos_del <- GRanges("chr5", IRanges(500, 502))

#create a deletion
del <- Deletion(
  variantID = "del1",
  position = pos_del,
  refAllele = "TGC",
  geneSymbol = "PTEN",
  geneType = "protein-coding",
  effect = "frameshift",
  clinicalRelevance = "Pathogenic",
  ensemblID = "ENST00000371953",
  hgvsp = "p.Asp117fs"
)

del

## ----session-info-------------------------------------------------------------
sessionInfo()

