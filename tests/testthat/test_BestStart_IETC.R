PARAMETERS_FILE = 'input/IncomeExplorer_TY22.xlsx'
PARAMETERS <- parameters_from_file(PARAMETERS_FILE)

test_that(
  'Test if receiving best start, IETC should be zero, for single family', 
  {
    
    test_parameters <- PARAMETERS
    
    # Single Family with no children
    emtr_output <- emtr(
      test_parameters, Partnered = FALSE, 
      wage1_hourly = 25,
      Children_ages = c(),
      gross_wage2 = 0, 
      hours2 = 0, max_wage = 120000,
      AS_Accommodation_Costs = 0,
      AS_Accommodation_Rent = FALSE,
      AS_Area = 1L
    )
    
    # Expect not to receive BestStart at all
    expect_true(emtr_output[, all(BestStart_Total == 0)])
    
    # Expect to receive IETC
    expect_true(emtr_output[, any(IETC_abated1 > 0)])
    
    # Expect to receive IETC at minimum gross income of IETC at 24000
    expect_equal(emtr_output[gross_wage1_annual > 24000 & 
                               net_benefit1 == 0][1, IETC_abated1], 
                 test_parameters$IETC_PerYear / wks_in_year(2022))
    
    
    emtr_output[IETC_abated1==0 & gross_wage1_annual > 24000][1]
    
    # Note that IETC_abated1 is all abated away when gross_wage1_annual == 48023.57
    
    # Expect partner not to receive IETC all
    expect_true(emtr_output[, all(IETC_abated2 == 0)])
    
    
    # Single family with 0 year old
    emtr_output <- emtr(
      test_parameters, Partnered = FALSE, 
      wage1_hourly = 25,
      Children_ages = c(0),
      gross_wage2 = 0, 
      hours2 = 0, max_wage = 120000,
      AS_Accommodation_Costs = 0,
      AS_Accommodation_Rent = FALSE,
      AS_Area = 1L
    )
    
    # Expect to receive BestStart for all Gross income range
    expect_true(emtr_output[, all(BestStart_Universal > 0)])
    expect_true(emtr_output[, all(BestStart_Total > 0)])
    
    # checks that all best start values are equal to the full rate
    expect_equal(emtr_output[, max(BestStart_Total)], 
                 test_parameters$FamilyAssistance_BestStart_Rates_Age0 / (365/7))
    
    # Expect to not receive IETC at all 
    expect_false(emtr_output[, any(IETC_abated1 > 0)])
    expect_false(emtr_output[, any(IETC_abated2 > 0)])
    
    # Single family with 2 years old
    emtr_output <- emtr(
      test_parameters, Partnered = FALSE, 
      wage1_hourly = 25,
      Children_ages = c(2),
      gross_wage2 = 0, 
      hours2 = 0, max_wage = 120000,
      AS_Accommodation_Costs = 0,
      AS_Accommodation_Rent = FALSE,
      AS_Area = 1L
    )
    
    # Expect BestStart_Universal to be zero, since there are no zero year old child
    expect_true(emtr_output[, all(BestStart_Universal == 0)])
    
    # Expect BestStart to be abated from gross_wage1_annual == 79000
    # Thus, the BestStart is not the same for all Gross income range
    expect_false(emtr_output[, all(BestStart_Total > 0)])
    
    # checks that all best start values up to total gross wage of 79,000 are equal to the full rate
    expect_equal(emtr_output[gross_wage1_annual <= 79000,
                             max(BestStart_Total)],
                 test_parameters$FamilyAssistance_BestStart_Rates_Age1or2 / 
                   (365/7))
    
    # BestStart is starting to abate from gross_wage1_annual == 79000
    expect_true(emtr_output[gross_wage1_annual %between% c(78950, 79100), 
                            abs(diff(BestStart_Total)) > 0 ])
    
    # BestStart is all abated away when gross_wage1_annual > 93858
    expect_true(emtr_output[gross_wage1_annual > 93858, 
                            all(BestStart_Total == 0)])
    
    
    # Expect to not receive IETC at all 
    expect_false(emtr_output[, any(IETC_abated1 > 0)])
    expect_false(emtr_output[, any(IETC_abated2 > 0)])
  })

test_that(
  'Test if receiving best start, IETC should be zero, for Couple family with no kids', 
  {
    
    test_parameters <- PARAMETERS
    
    #Couple family with no kids 
    #Partner has no income 
    emtr_output <- emtr(
      test_parameters, Partnered = TRUE, 
      wage1_hourly = 25,
      Children_ages = c(),
      gross_wage2 = 0, 
      hours2 = 0, max_wage = 120000,
      AS_Accommodation_Costs = 0,
      AS_Accommodation_Rent = FALSE,
      AS_Area = 1L
    )
    
    # Expect not to receive BestStart at all
    expect_true(emtr_output[, all(BestStart_Total == 0)])
    
    # Expect to receive IETC
    expect_true(emtr_output[, any(IETC_abated1 > 0)])
    
    # Expect to receive IETC more than minimum gross income of 24000
    # Make sure at the point when net_benefit1_and_LAP == 0
    expect_equal(emtr_output[gross_wage1_annual > 24000 &
                               net_benefit1 == 0][1, IETC_abated1], 
                 test_parameters$IETC_PerYear / wks_in_year(2022))
    
    # Expect partner not to receive IETC all
    expect_true(emtr_output[, all(IETC_abated2 == 0)])
    
    
    # Couple family with no kids 
    # Having Partner's gross income just over the minimum gross income of  24000
    emtr_output <- emtr(
      test_parameters, Partnered = TRUE, 
      wage1_hourly = 25,
      Children_ages = c(),
      gross_wage2 = 24000/wks_in_year(2022), 
      hours2 = 20, max_wage = 120000,
      AS_Accommodation_Costs = 0,
      AS_Accommodation_Rent = FALSE,
      AS_Area = 1L
    )
    
    # Expect both main distributor and partner to receive IETC
    expect_true(emtr_output[, any(IETC_abated1 > 0)])
    expect_true(emtr_output[, any(IETC_abated2 > 0)])
    
    # Expect not to receive BestStart at all, because they have no kids
    expect_true(emtr_output[, all(BestStart_Total == 0)])
    
  })

PARAMETERS_FILE = 'input/IncomeExplorer_TY22.xlsx'
PARAMETERS <- parameters_from_file(PARAMETERS_FILE)

test_that(
  'Test if receiving best start, IETC should be zero, for Couple family with 0 year old', 
  {
    
    test_parameters <- PARAMETERS   
    
    # Couple family with 0 year old
    # Having Partner's gross income just over the minimum gross income of IETC at 24000
    emtr_output <- emtr(
      test_parameters, Partnered = TRUE, 
      wage1_hourly = 25,
      Children_ages = c(0),
      gross_wage2 = 24000/wks_in_year(2022), 
      hours2 = 20, max_wage = 120000,
      AS_Accommodation_Costs = 0,
      AS_Accommodation_Rent = FALSE,
      AS_Area = 1L
    )
    
    
    # Expect to receive BestStart for all Gross income range
    expect_true(emtr_output[, all(BestStart_Universal > 0)])
    expect_true(emtr_output[, all(BestStart_Total > 0)])
    
    
    # Expect both main distributor and partner to not to receive IETC
    expect_true(emtr_output[, all(IETC_abated1 == 0)])
    expect_true(emtr_output[, all(IETC_abated2 == 0)])
    
  })

PARAMETERS_FILE = 'input/IncomeExplorer_TY22.xlsx'
PARAMETERS <- parameters_from_file(PARAMETERS_FILE)

test_that(
  'Test if receiving best start, IETC should be zero, for Couple family with 2 year old', 
  {
    
    test_parameters <- PARAMETERS
    
    # Couple family with 2 year old
    # Having Partner's gross income just over the minimum gross income of IETC at 24000
    emtr_output <- emtr(
      test_parameters, Partnered = TRUE, 
      wage1_hourly = 25,
      Children_ages = c(2),
      gross_wage2 = 24000/wks_in_year(2022), 
      hours2 = 20, max_wage = 120000,
      AS_Accommodation_Costs = 0,
      AS_Accommodation_Rent = FALSE,
      AS_Area = 1L
    )
    
    # Expect BestStart_Universal to be zero, since there are no zero year old child
    expect_true(emtr_output[, all(BestStart_Universal == 0)])
    
    # Expect BestStart to be abated from gross_wage1_annual == 79000
    # Thus, the BestStart is not the same for all Gross income range
    expect_false(emtr_output[, all(BestStart_Total > 0)])
    
    # checks that all best start values up to total gross wage of 79,000 are equal to the full rate
    expect_equal(emtr_output[gross_wage1_annual <= 79000,
                             max(BestStart_Total)],
                 test_parameters$FamilyAssistance_BestStart_Rates_Age1or2 / 
                   (365/7))
    
    # BestStart is starting to abate from gross_wage1_annual + gross_wage2_annual == 79000
    # i.e. gross_wage1_annual == (79000 - 24000 = ) 55000
    expect_true(emtr_output[gross_wage1_annual %between% c(54950, 55050), 
                            abs(diff(BestStart_Total)) > 0 ])
    
    
    # BestStart is all abated away when gross_wage1_annual + gross_wage2_annual > 93858
    # i.e. gross_wage1_annual > (93858 - 24000 = ) 69858
    expect_true(emtr_output[gross_wage1_annual > 69858, 
                            all(BestStart_Total == 0)])
    
    # Expect the Partner to receive IETC, since their gross_wage2_annual is 
    # greater or equal to 24000
    expect_true(emtr_output[, any(IETC_abated2 > 0)])
    
    # If recieving BestStart_Total, IETC_abated2 should equal to zero
    expect_true(emtr_output[BestStart_Total > 0 ,
                            all(IETC_abated2 == 0)])
    
    # From the test of single family, 
    # Note that IETC_abated1 is all abated away when gross_wage1_annual == 48023.57
    # For this case, when gross_wage1_annual> 48000, BestStart_Total is still greater than zero
    expect_true(emtr_output[gross_wage1_annual> 48000, any(BestStart_Total > 0)])
    
    # Thus, expect IETC_abated1 to be zero for all cases
    expect_true(emtr_output[, all(IETC_abated1 == 0)])
    
    # Couple family with 2 year old
    # Having Partner's gross income double of the minimum gross income of IETC at 48000, 
    # so IETC_abated1 is likely to be greater than zero, unlike the previous cases
    emtr_output <- emtr(
      test_parameters, Partnered = TRUE, 
      wage1_hourly = 25,
      Children_ages = c(2),
      gross_wage2 = 48000/wks_in_year(2022), 
      hours2 = 20, max_wage = 120000,
      AS_Accommodation_Costs = 0,
      AS_Accommodation_Rent = FALSE, AS_Area = 1L
    )
    
    # Expect BestStart_Universal to be zero, since there are no zero year old child
    expect_true(emtr_output[, all(BestStart_Universal == 0)])
    
    # Expect BestStart to be abated from gross_wage1_annual == 79000
    # Thus, the BestStart is not the same for all Gross income range
    expect_false(emtr_output[, all(BestStart_Total > 0)])
    
    # checks that all best start values up to total gross wage of 79,000 are equal to the full rate
    expect_equal(emtr_output[gross_wage1_annual + 48000 <= 79000,
                             max(BestStart_Total)],
                 test_parameters$FamilyAssistance_BestStart_Rates_Age1or2 / 
                   (365/7))
    
    # BestStart is starting to abate from gross_wage1_annual + gross_wage2_annual == 79000
    # i.e. gross_wage1_annual == (79000 - 48000 =) 31000
    expect_true(emtr_output[gross_wage1_annual %between% c(30950, 31050), 
                            abs(diff(BestStart_Total)) > 0 ])
    
    
    # BestStart is starting to abate from gross_wage1_annual + gross_wage2_annual == 93858
    # i.e. gross_wage1_annual == (93858 - 48000 =) 45858
    expect_true(emtr_output[gross_wage1_annual > 45858, 
                            all(BestStart_Total == 0)])
    
    
    # Expect the Partner to receive IETC, since their gross_wage2_annual is 
    # greater and equal to 24000
    expect_true(emtr_output[, any(IETC_abated2 > 0)])
    
    # If recieving BestStart_Total, IETC_abated2 should equal to zero
    expect_true(emtr_output[BestStart_Total > 0,
                            all(IETC_abated2 == 0)])
    
    # From the test of single family, 
    # Note that IETC_abated1 is all abated away when gross_wage1_annual == 48023.57
    # For this case, when gross_wage1_annual> 48000, BestStart_Total is already zero
    expect_true(emtr_output[gross_wage1_annual> 48000, any(BestStart_Total == 0)])
    
    # Thus, expect IETC_abated1 to be greater than zero for some cases
    expect_true(emtr_output[, any(IETC_abated1 > 0)])
    
    # If recieving BestStart_Total, IETC_abated1 should equal to zero
    expect_true(emtr_output[BestStart_Total > 0,
                            all(IETC_abated1 == 0)])
    
  }
)