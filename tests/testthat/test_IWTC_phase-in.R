PARAMETERS_FILE = 'input/IncomeExplorer_TY22.xlsx'
PARAMETERS <- parameters_from_file(PARAMETERS_FILE)

# test phase in single
test_that(
  'Test phase in of IWTC, for single parents ', 
  {
    test_parameters <- PARAMETERS
    test_parameters$FamilyAssistance_IWTC_Eligibility <- 2
    test_parameters$FamilyAssistance_IWTC_PhaseIn_Single <- matrix(c(0,200*52.2,0,-.5), ncol=2)
    test_parameters$FamilyAssistance_FTC_Rates_FirstChild <- 0
    test_parameters$FamilyAssistance_FTC_Rates_SubsequentChild <- 0
    test_parameters$FamilyAssistance_IWTC_Rates_UpTo3Children <- 20*52
    test_parameters$FamilyAssistance_Abatement_AbatementScale <- matrix(c(0,1000*365/7,0,.5), ncol=2)
    #
    test_parameters$FamilyAssistance_MFTC_Rates_MinimumIncome <- 0
    
    emtr_output <- emtr(
      test_parameters, Partnered = F, wage1_hourly = 20,
      Children_ages = c(0, 1, 10), gross_wage2 = 0, hours2 = 0,
      AS_Accommodation_Costs = 600, AS_Accommodation_Rent = T, AS_Area = 1L
    )
    has_required_output_names(emtr_output)
    
    
    # 20 IWTC where gross_wage1>phase in and 
    # gross_wage1+gross_wage2+gross_benefit1+gross_benefit2 < ftc abatement
    expect_true(
      emtr_output[gross_wage1>=240 & 
                    gross_wage1+gross_wage2+gross_benefit1+gross_benefit2<=1000,
                  all(IWTC_abated==20)])
    # 0 before and after start/end of abatement
    expect_true(
      emtr_output[gross_wage1<200 & 
                    gross_wage1+gross_wage2+gross_benefit1+gross_benefit2>1040,
                  all(IWTC_abated==0)])
    # check two points on the slope
    expect_true(emtr_output[gross_wage1==220, 
                            IWTC_abated==10])
    expect_true(emtr_output[gross_wage1+gross_wage2+gross_benefit1+gross_benefit2==1020, 
                            IWTC_abated==10])
  }
)

# test phase in couple
test_that(
  'Test phase in of IWTC, for single parents ', 
  {
    test_parameters <- PARAMETERS
    test_parameters$FamilyAssistance_IWTC_Eligibility <- 2
    test_parameters$FamilyAssistance_IWTC_PhaseIn_Couple <- matrix(c(0,200*52.2,0,-.5), ncol=2)
    test_parameters$FamilyAssistance_FTC_Rates_FirstChild <- 0
    test_parameters$FamilyAssistance_FTC_Rates_SubsequentChild <- 0
    test_parameters$FamilyAssistance_IWTC_Rates_UpTo3Children <- 20*52
    test_parameters$FamilyAssistance_Abatement_AbatementScale <- matrix(c(0,1000*365/7,0,.5), ncol=2)
    #
    test_parameters$FamilyAssistance_MFTC_Rates_MinimumIncome <- 0
    
    emtr_output <- emtr(
      test_parameters, Partnered = T, wage1_hourly = 20,
      Children_ages = c(0, 1, 10), gross_wage2 = 20*10, hours2 = 10,
      AS_Accommodation_Costs = 600, AS_Accommodation_Rent = T, AS_Area = 1L
    )
    has_required_output_names(emtr_output)
    
    # 20 IWTC where gross_wage1+gross_wage2>phase in and 
    # gross_wage1+gross_wage2+gross_benefit1+gross_benefit2 < ftc abatement
    expect_true(
      emtr_output[gross_wage1+gross_wage2>=240 & 
                    gross_wage1+gross_wage2+gross_benefit1+gross_benefit2<=1000,
                  all(IWTC_abated==20)])
    # 0 before and after start/end of abatement
    expect_true(
      emtr_output[gross_wage1+gross_wage2<200 & 
                    gross_wage1+gross_wage2+gross_benefit1+gross_benefit2>1040,
                  all(IWTC_abated==0)])
    # check two points on the slope
    expect_true(emtr_output[gross_wage1+gross_wage2==220, 
                            IWTC_abated==10])
    expect_true(emtr_output[gross_wage1+gross_wage2+gross_benefit1+gross_benefit2==1020, 
                            IWTC_abated==10])
  }
)