PARAMETERS_FILE = 'input/IncomeExplorer_TY22.xlsx'
PARAMETERS <- parameters_from_file(PARAMETERS_FILE)

PARAMETERS_TAR210_FILE = 'input/IncomeExplorer_TY22_WFF_order.xlsx'
PARAMETERS_TAR210 <- parameters_from_file(PARAMETERS_TAR210_FILE)

test_that(
  'Test FamilyAssistance_Abatement_Order', 
  {
    test_parameters <- PARAMETERS
    
    # Status Quo (Default): FTC abated first then IWTC
    emtr_output_SQ <- emtr(
      test_parameters, Partnered = F, wage1_hourly = 18.50,
      Children_ages = c(0, 1, 10), gross_wage2 = 0, hours2 = 0,
      AS_Accommodation_Costs = 600, AS_Accommodation_Rent = T, AS_Area = 1L
    )
    
    # Reform: IWTC abated first then FTC
    test_parameters <- PARAMETERS_TAR210
    
    emtr_output_reform <- emtr(
      test_parameters, Partnered = F, wage1_hourly = 18.50,
      Children_ages = c(0, 1, 10), gross_wage2 = 0, hours2 = 0,
      AS_Accommodation_Costs = 600, AS_Accommodation_Rent = T, AS_Area = 1L
    )
    
    has_required_output_names(emtr_output_SQ)
    has_required_output_names(emtr_output_reform)
    
    # Net wage should still be the same between reform and SQ
    expect_equal( emtr_output_reform$net_wage1, emtr_output_SQ$net_wage1)
    # Is FTC in the reform ever higher than in the SQ
    expect_true( any(emtr_output_reform$FTC_abated > emtr_output_SQ$FTC_abated))
    # Is IWTC in the reform ever less than in the SQ
    expect_true( any(emtr_output_reform$IWTC_abated <  emtr_output_SQ$IWTC_abated))
    
    # Net Income should still be the same between reform and SQ
    expect_equal(emtr_output_reform$Net_Income, emtr_output_SQ$Net_Income) 
    
  }
)