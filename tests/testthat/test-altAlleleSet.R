test_that("altAlleleSet setter updates the alternative allele correctly", {
  
  pos <- GRanges(seqnames = "chr1",ranges = IRanges(start = 123456, width = 1),strand = "+")
  
  snv <- new("SNV",
             variantID = "rs123",
             position = pos,
             refAllele = "A",
             altAllele = "T",
             geneSymbol = "BRCA1",
             geneType = "protein-coding",
             effect = "missense",
             clinicalRelevance = "Pathogenic",
             ensemblID = "ENST00000357654",
             hgvsp = "p.Arg175His")
  snv_new <- altAlleleSet(snv, "G")
  expect_equal(altAllele(snv_new), "G")
})