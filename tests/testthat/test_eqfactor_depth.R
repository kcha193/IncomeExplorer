PARAMETERS_FILE = "input/IncomeExplorer_TY22.xlsx"
PARAMETERS <- parameters_from_file(PARAMETERS_FILE)

test_that(
  'Test if modified OECD, depth and equivalised income calculated properly', 
  {
    
    test_parameters <- PARAMETERS
    
    # Single Family with no children
    emtr_output <- emtr(
      test_parameters,
      # Family parameters
      Partnered = FALSE,
      wage1_hourly = 20,
      Children_ages = c(),
      gross_wage2 = 0,
      hours2 = 0,
      AS_Accommodation_Costs = 400,
      AS_Accommodation_Rent = TRUE,
      AS_Area = 1L,
      # Presentation parameters
      max_wage = 1900,
      steps_per_dollar = 1L,
      weeks_in_year = 52,
      MFTC_WEP_scaling = NULL,
      pov_thresholds = 0.5,
      bhc_median = 43000,
      ahc_median = 33100
    )
    
    expect_equal(emtr_output$Eq_Factor, rep(1, nrow(emtr_output)))
    expect_equal(emtr_output$Net_Income,emtr_output$Equivalised_Income)
    
    expect_equal(emtr_output$AHC_Net_Income, (emtr_output$AHC_Equivalised_Income * emtr_output$Eq_Factor))
    expect_equal(emtr_output$Net_Income, (emtr_output$Equivalised_Income * emtr_output$Eq_Factor))
    
    expect_equal(emtr_output$AHC_Depth, (emtr_output$AHC_Unequiv_Poverty_Line/52 - pmax(0, emtr_output$AHC_Net_Income)))
    expect_equal(emtr_output$BHC_Depth, (emtr_output$BHC_Unequiv_Poverty_Line/52 - pmax(0,emtr_output$Net_Income)))
    
    # Single family with one child under 14
    emtr_output <- emtr(
      test_parameters,
      # Family parameters
      Partnered = FALSE,
      wage1_hourly = 20,
      Children_ages = c(0),
      gross_wage2 = 0,
      hours2 = 0,
      AS_Accommodation_Costs = 400,
      AS_Accommodation_Rent = TRUE,
      AS_Area = 1L,
      # Presentation parameters
      max_wage = 1900,
      steps_per_dollar = 1L,
      weeks_in_year = 52,
      MFTC_WEP_scaling = NULL,
      pov_thresholds = 0.5,
      bhc_median = 43000,
      ahc_median = 33100
    )
    
    expect_equal(emtr_output$Eq_Factor, rep(1.3, nrow(emtr_output)))
    expect_true(emtr_output[, all(Net_Income >  Equivalised_Income)])
    
    expect_equal(emtr_output$AHC_Net_Income, (emtr_output$AHC_Equivalised_Income * emtr_output$Eq_Factor))
    expect_equal(emtr_output$Net_Income, (emtr_output$Equivalised_Income * emtr_output$Eq_Factor))
    
    expect_equal(emtr_output$AHC_Depth, (emtr_output$AHC_Unequiv_Poverty_Line/52 - pmax(0, emtr_output$AHC_Net_Income)))
    expect_equal(emtr_output$BHC_Depth, (emtr_output$BHC_Unequiv_Poverty_Line/52 - pmax(0,emtr_output$Net_Income)))
    
    # Single family with one child over 14 
    emtr_output <- emtr(
      test_parameters,
      # Family parameters
      Partnered = FALSE,
      wage1_hourly = 20,
      Children_ages = c(15),
      gross_wage2 = 0,
      hours2 = 0,
      AS_Accommodation_Costs = 400,
      AS_Accommodation_Rent = TRUE,
      AS_Area = 1L,
      # Presentation parameters
      max_wage = 1900,
      steps_per_dollar = 1L,
      weeks_in_year = 52,
      MFTC_WEP_scaling = NULL,
      pov_thresholds = 0.5,
      bhc_median = 43000,
      ahc_median = 33100
    )
    
    expect_equal(emtr_output$Eq_Factor, rep(1.5, nrow(emtr_output)))
    expect_true(emtr_output[, all(Net_Income >  Equivalised_Income)])
    
    expect_equal(emtr_output$AHC_Net_Income, (emtr_output$AHC_Equivalised_Income * emtr_output$Eq_Factor))
    expect_equal(emtr_output$Net_Income, (emtr_output$Equivalised_Income * emtr_output$Eq_Factor))
    
    expect_equal(emtr_output$AHC_Depth, (emtr_output$AHC_Unequiv_Poverty_Line/52 - pmax(0, emtr_output$AHC_Net_Income)))
    expect_equal(emtr_output$BHC_Depth, (emtr_output$BHC_Unequiv_Poverty_Line/52 - pmax(0,emtr_output$Net_Income)))
    
    # Single family with more than one child
    emtr_output <- emtr(
      test_parameters,
      # Family parameters
      Partnered = FALSE,
      wage1_hourly = 20,
      Children_ages = c(5,15),
      gross_wage2 = 0,
      hours2 = 0,
      AS_Accommodation_Costs = 400,
      AS_Accommodation_Rent = TRUE,
      AS_Area = 1L,
      # Presentation parameters
      max_wage = 1900,
      steps_per_dollar = 1L,
      weeks_in_year = 52,
      MFTC_WEP_scaling = NULL,
      pov_thresholds = 0.5,
      bhc_median = 43000,
      ahc_median = 33100
    )
    
    expect_equal(emtr_output$Eq_Factor, rep(1.8, nrow(emtr_output)))
    expect_true(emtr_output[, all(Net_Income >  Equivalised_Income)])
    
    expect_equal(emtr_output$AHC_Net_Income, (emtr_output$AHC_Equivalised_Income * emtr_output$Eq_Factor))
    expect_equal(emtr_output$Net_Income, (emtr_output$Equivalised_Income * emtr_output$Eq_Factor))
    
    expect_equal(emtr_output$AHC_Depth, (emtr_output$AHC_Unequiv_Poverty_Line/52 - pmax(0, emtr_output$AHC_Net_Income)))
    expect_equal(emtr_output$BHC_Depth, (emtr_output$BHC_Unequiv_Poverty_Line/52 - pmax(0,emtr_output$Net_Income)))
    
    # couple family with no child
    emtr_output <- emtr(
      test_parameters,
      # Family parameters
      Partnered = TRUE,
      wage1_hourly = 20,
      Children_ages = c(),
      gross_wage2 = 0,
      hours2 = 0,
      AS_Accommodation_Costs = 400,
      AS_Accommodation_Rent = TRUE,
      AS_Area = 1L,
      # Presentation parameters
      max_wage = 1900,
      steps_per_dollar = 1L,
      weeks_in_year = 52,
      MFTC_WEP_scaling = NULL,
      pov_thresholds = 0.5,
      bhc_median = 43000,
      ahc_median = 33100
    )

    expect_equal(emtr_output$Eq_Factor, rep(1.5, nrow(emtr_output)))
    expect_true(emtr_output[, all(Net_Income >  Equivalised_Income)])
    
    expect_equal(emtr_output$AHC_Net_Income, (emtr_output$AHC_Equivalised_Income * emtr_output$Eq_Factor))
    expect_equal(emtr_output$Net_Income, (emtr_output$Equivalised_Income * emtr_output$Eq_Factor))
    
    expect_equal(emtr_output$AHC_Depth, (emtr_output$AHC_Unequiv_Poverty_Line/52 - pmax(0, emtr_output$AHC_Net_Income)))
    expect_equal(emtr_output$BHC_Depth, (emtr_output$BHC_Unequiv_Poverty_Line/52 - pmax(0,emtr_output$Net_Income)))
    
    # couple family with one child under 14
    emtr_output <- emtr(
      test_parameters,
      # Family parameters
      Partnered = TRUE,
      wage1_hourly = 20,
      Children_ages = c(3),
      gross_wage2 = 0,
      hours2 = 0,
      AS_Accommodation_Costs = 400,
      AS_Accommodation_Rent = TRUE,
      AS_Area = 1L,
      # Presentation parameters
      max_wage = 1900,
      steps_per_dollar = 1L,
      weeks_in_year = 52,
      MFTC_WEP_scaling = NULL,
      pov_thresholds = 0.5,
      bhc_median = 43000,
      ahc_median = 33100
    )
    
    expect_true(emtr_output[, all(Eq_Factor > 1.5)])
    expect_true(emtr_output[, all(Net_Income >  Equivalised_Income)])
    
    expect_equal(emtr_output$AHC_Net_Income, (emtr_output$AHC_Equivalised_Income * emtr_output$Eq_Factor))
    expect_equal(emtr_output$Net_Income, (emtr_output$Equivalised_Income * emtr_output$Eq_Factor))
    
    expect_equal(emtr_output$AHC_Depth, (emtr_output$AHC_Unequiv_Poverty_Line/52 - pmax(0, emtr_output$AHC_Net_Income)))
    expect_equal(emtr_output$BHC_Depth, (emtr_output$BHC_Unequiv_Poverty_Line/52 - pmax(0,emtr_output$Net_Income)))
    
    # couple family with one child over 14
    emtr_output <- emtr(
      test_parameters,
      # Family parameters
      Partnered = TRUE,
      wage1_hourly = 20,
      Children_ages = c(15),
      gross_wage2 = 0,
      hours2 = 0,
      AS_Accommodation_Costs = 400,
      AS_Accommodation_Rent = TRUE,
      AS_Area = 1L,
      # Presentation parameters
      max_wage = 1900,
      steps_per_dollar = 1L,
      weeks_in_year = 52,
      MFTC_WEP_scaling = NULL,
      pov_thresholds = 0.5,
      bhc_median = 43000,
      ahc_median = 33100
    )
    
    expect_equal(emtr_output$Eq_Factor, rep(2, nrow(emtr_output)))
    expect_true(emtr_output[, all(Net_Income >  Equivalised_Income)])
    
    expect_equal(emtr_output$AHC_Net_Income, (emtr_output$AHC_Equivalised_Income * emtr_output$Eq_Factor))
    expect_equal(emtr_output$Net_Income, (emtr_output$Equivalised_Income * emtr_output$Eq_Factor))
    
    expect_equal(emtr_output$AHC_Depth, (emtr_output$AHC_Unequiv_Poverty_Line/52 - pmax(0, emtr_output$AHC_Net_Income)))
    expect_equal(emtr_output$BHC_Depth, (emtr_output$BHC_Unequiv_Poverty_Line/52 - pmax(0,emtr_output$Net_Income)))
    
    # couple family with more than one child
    emtr_output <- emtr(
      test_parameters,
      # Family parameters
      Partnered = TRUE,
      wage1_hourly = 20,
      Children_ages = c(3,15),
      gross_wage2 = 0,
      hours2 = 0,
      AS_Accommodation_Costs = 400,
      AS_Accommodation_Rent = TRUE,
      AS_Area = 1L,
      # Presentation parameters
      max_wage = 1900,
      steps_per_dollar = 1L,
      weeks_in_year = 52,
      MFTC_WEP_scaling = NULL,
      pov_thresholds = 0.5,
      bhc_median = 43000,
      ahc_median = 33100
    )
    
    expect_true(emtr_output[, all(Eq_Factor > 2)])
    expect_true(emtr_output[, all(Net_Income >  Equivalised_Income)])
    
    expect_equal(emtr_output$AHC_Net_Income, (emtr_output$AHC_Equivalised_Income * emtr_output$Eq_Factor))
    expect_equal(emtr_output$Net_Income, (emtr_output$Equivalised_Income * emtr_output$Eq_Factor))
    
    expect_equal(emtr_output$AHC_Depth, (emtr_output$AHC_Unequiv_Poverty_Line/52 - pmax(0, emtr_output$AHC_Net_Income)))
    expect_equal(emtr_output$BHC_Depth, (emtr_output$BHC_Unequiv_Poverty_Line/52 - pmax(0,emtr_output$Net_Income)))
    
  })

