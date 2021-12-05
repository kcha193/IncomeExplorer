logging::logReset()
logging::addHandler(logging::writeToFile, file = "test_IncomeExplorer.log")

output_dir <- './output/'
if (dir.exists(output_dir)) {
  unlink("output", recursive = TRUE)
}
dir.create(output_dir)

REQUIRED_OUTPUTS <- list(
  "gross_wage1",  "hours1",  "gross_wage1_annual",  "gross_wage2" ,  "wage2_tax",
  "wage2_ACC_levy",  "net_wage2",  "net_benefit2",  "gross_benefit2",  "IETC_abated1",
  "IETC_abated2",  "net_benefit1",  "IWTC_unabated",  "gross_benefit1",
  "gross_benefit_and_wage1",  "net_benefit_and_wage1",  "wage1_tax",  "wage1_ACC_levy",
  "net_wage1",  "FTC_unabated",  "MFTC",  "AbateAmount",  "FTC_abated",  "IWTC_abated",
  "BestStart_Universal",  "BestStart_Abated",  "BestStart_Total",  "WinterEnergy",
  "AS_Amount",  "Net_Income",  "EMTR",  "Replacement_Rate" ,  "Participation_Tax_Rate",
  "Equivalised_Income",  "BHC_Depth",  "AHC_Net_Income",  "AHC_Equivalised_Income",
  "AHC_Depth",  "BHC_Unequiv_Poverty_Line",  "AHC_Unequiv_Poverty_Line",  "Eq_Factor"
)

is_subset <- function(small_list, big_list){
  intersection <- intersect(small_list, big_list)
  return(length(small_list) == length(intersection))
}

test_against_ref <- function(emtr_output, ref_file) {
  ref_output <- fread(ref_file)
  for (variable in REQUIRED_OUTPUTS) {
    testthat::expect_equal(emtr_output[[variable]], ref_output[[variable]], label = variable,
                           tolerance = 1e-3)
  }
}


test_datatables <- function(emtr_output, ref_output, columns) {
  browser()
  for (variable in columns) {
    testthat::expect_equal(emtr_output[[variable]],
                           ref_output[[variable]],
                           label = variable,
                           tolerance = 1e-3)
  }
}

has_required_output_names <- function(emtr_output){
  testthat::expect_true(is_subset(REQUIRED_OUTPUTS, names(emtr_output)))
}
