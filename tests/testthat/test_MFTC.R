PARAMETERS_FILE = 'input/IncomeExplorer_TY22.xlsx'
PARAMETERS <- parameters_from_file(PARAMETERS_FILE)

test_that(
  'Hours test for MFTC, if working at least FullTimeWorkingHours per week', 
  {
    
    test_parameters <- PARAMETERS
    test_parameters$FamilyAssistance_IWTC_Eligibility <- 0
    test_parameters$FamilyAssistance_IWTC_ToBeneficiaries <- 1
    
    emtr_output <- emtr(
      test_parameters, Partnered = F, wage1_hourly = 18.50,
      Children_ages = c(0, 1, 10), gross_wage2 = 0, hours2 = 0,
      AS_Accommodation_Costs = 600, AS_Accommodation_Rent = T, AS_Area = 1L
    )
    has_required_output_names(emtr_output)
    
    #Expect MFTC is zero if not working full time
    expect_true(
      emtr_output[hours1 < 
                    test_parameters$FamilyAssistance_FullTimeWorkingHours_Single,
                  all(MFTC == 0)])
    
    #Expect MFTC to be greater than zero is hours work meet the full time work hours
    expect_true(
      emtr_output[hours1 > 
                    test_parameters$FamilyAssistance_FullTimeWorkingHours_Single,
                  MFTC[1] > 0])
    
    #Expect IWTC_abated is zero if not working full time
    expect_true(
      emtr_output[hours1 < 
                    test_parameters$FamilyAssistance_FullTimeWorkingHours_Single,
                  all(IWTC_abated == 0)])
    
    #Expect IWTC_abated to be non-zero, equal to the up-to-three-children rate
    expect_true(
      emtr_output[hours1 > 
                    test_parameters$FamilyAssistance_FullTimeWorkingHours_Single,
                  all(IWTC_abated == 
                        test_parameters$FamilyAssistance_IWTC_Rates_UpTo3Children/52)])
    
    
    # Using the Income test option based on TAR 236
    test_parameters$FamilyAssistance_IWTC_Eligibility <- 1
    test_parameters$FamilyAssistance_IWTC_ToBeneficiaries <- 0
    test_parameters$FamilyAssistance_IWTC_IncomeThreshold_Single <- 20*52.2
    
    emtr_output <- emtr(
      test_parameters, Partnered = F, wage1_hourly = 18.50,
      Children_ages = c(0, 1, 10), gross_wage2 = 0, hours2 = 0,
      AS_Accommodation_Costs = 600, AS_Accommodation_Rent = T, AS_Area = 1L
    )
    has_required_output_names(emtr_output)
    
    #Expect MFTC is zero if not working full time
    expect_true(
      emtr_output[hours1 < 
                    test_parameters$FamilyAssistance_FullTimeWorkingHours_Single,
                  all(MFTC == 0)])
    
    #Expect MFTC to be greater than zero is hours work meet the full time work hours
    expect_true(
      emtr_output[hours1 > 
                    test_parameters$FamilyAssistance_FullTimeWorkingHours_Single,
                  MFTC[1] > 0])
    
    
    # Now the net income contains IWTC instead of benefit if work greater
    # than IWTC_Income Threshold, even not working full time
    expect_true(
      emtr_output[
        gross_wage1 >= 
          test_parameters$FamilyAssistance_IWTC_IncomeThreshold_Single/52.2 ,
        all(IWTC_abated == 
              test_parameters$FamilyAssistance_IWTC_Rates_UpTo3Children/52)])
    
    
    # Thus, the benefit has became zero, if work greater
    # than IWTC_Income Threshold, even not working full time.
    expect_true(
      emtr_output[
        gross_wage1 >= 
          test_parameters$FamilyAssistance_IWTC_IncomeThreshold_Single/52.2 & 
          hours1 < test_parameters$FamilyAssistance_FullTimeWorkingHours_Single,
        all(net_benefit1 == 0)])
    
    # Check the opposite from above 
    expect_true(
      emtr_output[
        gross_wage1 < test_parameters$FamilyAssistance_IWTC_IncomeThreshold_Single/52.2,
        all(net_benefit1 > 0)
      ]
    )
    
    
  }
)