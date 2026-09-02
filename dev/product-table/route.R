# The route a dosage form describes.
#
# The consistency check groups products by ATC and route. ATC alone does not
# work: G03CA03 is estradiol, and it spans five categories because the route
# differs. Measured on the 2026 delivery, 13 of 40 ATC codes carry more than
# one category, and most of those splits are correct.
#
# The map is deliberately coarse. It answers "did the drug enter the same way",
# not "was the package identical".

lmed_route_of_form <- function(lform) {
  f <- tolower(as.character(lform))
  return(data.table::fcase(
    grepl("intrauterint", f), "iud",
    grepl("implantat", f), "implant",
    grepl("injektion", f), "injection",
    grepl("vaginal|vagitorium", f), "vaginal",
    grepl("plåster|transdermal|^gel", f), "transdermal",
    grepl("bucka", f), "buccal",
    grepl("tablett|kapsel|kapslar|dragerad", f), "oral",
    default = "other"
  ))
}
