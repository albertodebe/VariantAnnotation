test_that("Deletion object stores all slots correctly", {
  pos_del <- GenomicRanges::GRanges("chr5", IRanges::IRanges(500, 502))
  
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
  
  expect_equal(del@variantID, "del1")
  expect_equal(del@position, pos_del)
  expect_equal(del@refAllele, "TGC")
  expect_equal(del@altAllele, "-") 
  expect_equal(del@geneSymbol, "PTEN")
  expect_equal(del@geneType, "protein-coding")
  expect_equal(del@effect, "frameshift")
  expect_equal(del@clinicalRelevance, "Pathogenic")
  expect_equal(del@ensemblID, "ENST00000371953")
  expect_equal(del@hgvsp, "p.Asp117fs")
})