test_that("aamutation returns correct amino acid position", {
  pos <- GenomicRanges::GRanges("chr1", IRanges::IRanges(123456, width = 1), strand = "+")
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
  expect_equal(aamutation(snv), "175")
})