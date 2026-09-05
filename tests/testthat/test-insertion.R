test_that("Insertion object stores all slots correctly", {
  pos_ins <- GenomicRanges::GRanges(seqnames = "chr17",
                                    ranges = IRanges::IRanges(start = 41276045, end = 41276045),
                                    strand = "+")
  
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
  
  expect_equal(ins@variantID, "COSM476")
  expect_equal(ins@position, pos_ins)
  expect_equal(ins@altAllele, "AG")
  expect_equal(ins@geneSymbol, "BRCA1")
  expect_equal(ins@geneType, "protein-coding")
  expect_equal(ins@effect, "frameshift_variant")
  expect_equal(ins@clinicalRelevance, "Pathogenic")
  expect_equal(ins@ensemblID, "ENST00000357654")
  expect_equal(ins@hgvsp, "p.S1615fs")
})