test_that("DNV object stores all slots correctly", {
  pos2 <- GenomicRanges::GRanges(seqnames = "chr2", ranges = IRanges::IRanges(start = 200, width = 2))
  
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
  
  expect_equal(dnv@variantID, "dnv1")
  expect_equal(dnv@position, pos2)
  expect_equal(dnv@refAllele, "AG")
  expect_equal(dnv@altAllele, "TC")
  expect_equal(dnv@geneSymbol, "TP53")
  expect_equal(dnv@geneType, "protein-coding")
  expect_equal(dnv@effect, "nonsense")
  expect_equal(dnv@clinicalRelevance, "Likely pathogenic")
  expect_equal(dnv@ensemblID, "ENST00000269305")
  expect_equal(dnv@hgvsp, "p.Trp53*")
})