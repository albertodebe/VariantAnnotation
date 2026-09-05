test_that("SNV object stores all slots correctly", {
  pos <- GenomicRanges::GRanges(seqnames = "chr1", ranges = IRanges::IRanges(start = 123456, width = 1), strand = "+")
  
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
  
  expect_equal(snv@variantID, "rs123")
  expect_equal(snv@position, pos)
  expect_equal(snv@refAllele, "A")
  expect_equal(snv@altAllele, "T")
  expect_equal(snv@geneSymbol, "BRCA1")
  expect_equal(snv@geneType, "protein-coding")
  expect_equal(snv@effect, "missense")
  expect_equal(snv@clinicalRelevance, "Pathogenic")
  expect_equal(snv@ensemblID, "ENST00000357654")
  expect_equal(snv@hgvsp, "p.Arg175His")
})