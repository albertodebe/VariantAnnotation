# ==============================================================================
# S4 Class for Genetic Variants
# ==============================================================================

#' @title Virtual general class for all variant types
#' @description this s4 class is the parent of all other mut classes.
#' @slot variantID Unique identifier for the variant 
#' @slot position GRanges object with genomic position
#' @slot refAllele Reference allele sequence
#' @slot altAllele Alternate allele sequence
#' @slot geneSymbol Gene symbol affected 
#' @slot geneType Type of gene 
#' @slot effect Variant effect ("missense", "nonsense", etc.)
#' @slot clinicalRelevance Clinical significance from ClinVar/COSMIC
#' @slot ensemblID Ensembl transcript ID
#' @slot hgvsp HGVSp protein notation 
#' @exportClass Variant
setClass("Variant",
         slots = list(
           variantID = "character",
           position = "GRanges",
           refAllele = "character",
           altAllele = "character",
           geneSymbol = "character",
           geneType = "character",
           effect = "character",
           clinicalRelevance = "character",
           ensemblID = "character",
           hgvsp = "character"
         ),
         contains = "VIRTUAL") #so it cannot be directly initiated

###   Classes for variant types   ###

#' @title Single Nucleotide Variant (SNV) class
#' @description Class for single nucleotide substitutions (e.g., A>T)
#' @exportClass SNV
setClass("SNV", contains = "Variant")

#' @title Double Nucleotide Variant (DNV) class
#' @description Class for dinucleotide substitutions (e.g., AG>TC)
#' @exportClass DNV
setClass("DNV", contains = "Variant")

#' @title Other Nucleotide Variant (ONV) class
#' @description Class for complex nucleotide changes
#' @slot variantType Type of variant
#' @exportClass ONV
setClass("ONV", 
         slots = list(
           variantType = "character"
         ),
         contains = "Variant")

#' @title Insertion class
#' @description Class for insertion mutations
#' @exportClass Insertion
setClass("Insertion", contains = "Variant")

#' @title Deletion class
#' @description Class for deletion mutations
#' @slot deletedSeq The deleted sequence
#' @exportClass Deletion
setClass("Deletion",
         slots = list(
           deletedSeq = "character"
         ),
         contains = "Variant")

###   Constructor Functions   ####

#' @title SNV constructor
#' @description Creates an SNV (single nucletide variant) object
#' @param variantID Variant identifier
#' @param position GRanges object with position
#' @param refAllele Reference allele (single nucleotide)
#' @param altAllele Alternate allele (single nucleotide)
#' @param ... Additional Variant slots
#' @return SNV object
#' @export
SNV <- function(variantID, position, refAllele, altAllele, ...) {
  if (nchar(refAllele) != 1 || nchar(altAllele) != 1) {
    stop("SNV must be a single nucleotide")
  }
  new("SNV",
      variantID = variantID,
      position = position,
      refAllele = refAllele,
      altAllele = altAllele,
      ...)
}

#' @title DNV constructor
#' @description Creates a DNV (dinucleotide variant) object
#' @param variantID Variant identifier
#' @param position GRanges object (must span 2 adjacent nucleotides)
#' @param refAllele Reference dinucleotide
#' @param altAllele Alternate dinucleotide
#' @param ... Additional Variant slots
#' @return DNV object
#' @export
DNV <- function(variantID, position, refAllele, altAllele, ...) {
  if (width(position) != 2 || nchar(refAllele) != 2 || nchar(altAllele) != 2) {
    stop("DNV must be two nucleotides, lenght of refAllele or altAllele does not match")
  }
  new("DNV",
      variantID = variantID,
      position = position,
      refAllele = refAllele,
      altAllele = altAllele,
      ...)
}

#' @title Insertion constructor
#' @description Creates an Insertion object
#' @param variantID Variant identifier
#' @param position GRanges object where region is added
#' @param altAllele Inserted sequence
#' @param ... Additional Variant slots
#' @return Insertion object
#' @export
Insertion <- function(variantID, position, altAllele, ...) {
  new("Insertion",
      variantID = variantID,
      position = position,
      refAllele = "-",
      altAllele = altAllele,
      ...)
}

#' @title Deletion constructor
#' @description Creates a Deletion object
#' @param variantID Variant identifier
#' @param position GRanges object where region is deleted
#' @param refAllele Reference sequence being deleted
#' @param ... Additional Variant slots
#' @return Deletion object
#' @export
Deletion <- function(variantID, position, refAllele, ...) {
  if (width(position) != nchar(refAllele)) {
    stop("Deletion position range must match length of refAllele")
  }
  new("Deletion",
      variantID = variantID,
      position = position,
      refAllele = refAllele,
      altAllele = "-",
      deletedSeq = refAllele,
      ...)
}


###   Methods   ###


#'  Get or set alternative allele
#'
#' @param x A Variant object
#' @param value A character string representing the new alternative allele (for setter)
#' @return The alternative allele (getter) or updated object (setter)
#' @export
setGeneric("altAllele", function(x) standardGeneric("altAllele"))

#' @rdname altAllele
#' @export
setMethod("altAllele", "Variant", function(x) x@altAllele)

#' @rdname altAllele
#' @export
setGeneric("altAlleleSet", function(x, value) standardGeneric("altAlleleSet"))

#' @rdname altAllele
#' @export
setMethod("altAlleleSet", "Variant", function(x, value) {
  x@altAllele <- value
  x
})


#  get allele effect

#' @title Get variant effect
#' @param x Variant object
#' @return Variant effect (e.g., "missense")
#' @export
setGeneric("effect", function(x) standardGeneric("effect"))

#' @describeIn effect Get variant effect for any Variant
#' @export
setMethod("effect", "Variant", function(x) x@effect)

###   Method for AA Mutation    ###

#' @title Extract aa mutation position
#' @description Gets the affected amino acid position from HGVSp notation
#' @param variant Variant object
#' @return Amino acid position as character (or "-" if non-coding)
#' @export
setGeneric("aamutation", function(variant) standardGeneric("aamutation"))

#' @describeIn aamutation Default implementation for Variant objects
#' @export
setMethod("aamutation", "Variant", function(variant) {
  hgvsp <- variant@hgvsp
  
  # in case of empty seq
  if (is.na(hgvsp) || hgvsp == "" || hgvsp == "p.=") return("-")
  
  # Match pattern "p." followed by letters (including '*') and digits
  match <- regmatches(hgvsp, regexpr("p\\.[A-Za-z*]+[0-9]+", hgvsp))
  if (length(match) == 0 || match == "") return("-")
  
  # Remove the "p." prefix
  without_prefix <- sub("^p\\.", "", match)
  
  # Extract the position number (digits)
  pos <- regmatches(without_prefix, regexpr("[0-9]+", without_prefix))
  if (length(pos) == 0 || pos == "") return("-")
  
  return(pos)
})

