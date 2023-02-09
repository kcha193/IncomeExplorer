PARAMETERS_FILE <- 'input/IncomeExplorer_TY22.xlsx'
PARAMETERS <- parameters_from_file(PARAMETERS_FILE)

test_that(
  '1.	Single parent, children aged 0, 1, 10, AS area 1, renting', 
  {
    emtr_output <- emtr(
      PARAMETERS, Partnered = F, wage1_hourly = 18.50,
      Children_ages = c(0, 1, 10), gross_wage2 = 0, hours2 = 0,
      AS_Accommodation_Costs = 600, AS_Accommodation_Rent = T, AS_Area = 1L
    )
    has_required_output_names(emtr_output)


    #fwrite(emtr_output, 'ref/emtr_output_1.csv')

    test_against_ref(emtr_output, 'ref/emtr_output_1.csv')
  }
  )

test_that(
  '2.	Couple parent, children aged 2, 15, partner not working, AS area 2, mortgage', 
  {
    emtr_output <- emtr(
      PARAMETERS, Partnered = T, wage1_hourly = 18.50,
      Children_ages = c(2, 15), gross_wage2 = 0, hours2 = 0,
      AS_Accommodation_Costs = 800, AS_Accommodation_Rent = F, AS_Area = 2L
    )
    has_required_output_names(emtr_output)

    #fwrite(emtr_output, 'ref/emtr_output_2.csv')

    test_against_ref(emtr_output, 'ref/emtr_output_2.csv')
  }
  )

test_that(
  '3.	Couple parent, children aged 9, partner working 10 hours, AS area 3, renting',
  {
    emtr_output <- emtr(
      PARAMETERS, Partnered = T, wage1_hourly = 18.50,
      Children_ages = c(9), gross_wage2 = 185, hours2 = 10,
      AS_Accommodation_Costs = 600, AS_Accommodation_Rent = T, AS_Area = 3L
    )
    has_required_output_names(emtr_output)

    #fwrite(emtr_output, 'ref/emtr_output_3.csv')


    test_against_ref(emtr_output, 'ref/emtr_output_3.csv')
  })

test_that(
  '4.	Couple, both working, partner working 20 hours, AS area 4, mortgage',
  {
    emtr_output <- emtr(
      PARAMETERS, Partnered = T, wage1_hourly = 18.50,
      Children_ages = c(), gross_wage2 = 370, hours2 = 20,
      AS_Accommodation_Costs = 500, AS_Accommodation_Rent = F, AS_Area = 4L
    )
    has_required_output_names(emtr_output)

    #fwrite(emtr_output, 'ref/emtr_output_4.csv')

    test_against_ref(emtr_output, 'ref/emtr_output_4.csv')
  })

test_that(
  '5.	Couple, one working, AS area 1, mortgage',
  {
    emtr_output <- emtr(
      PARAMETERS, Partnered = T, wage1_hourly = 18.50,
      Children_ages = c(), gross_wage2 = 0, hours2 = 0,
      AS_Accommodation_Costs = 800, AS_Accommodation_Rent = F, AS_Area = 1L
    )
    has_required_output_names(emtr_output)

    #fwrite(emtr_output, 'ref/emtr_output_5.csv')

    test_against_ref(emtr_output, 'ref/emtr_output_5.csv')
  })

test_that(
  '6.	Single, AS area 2, renting',
  {
    emtr_output <- emtr(
      PARAMETERS, Partnered = F, wage1_hourly = 18.50,
      Children_ages = c(), gross_wage2 = 0, hours2 = 0,
      AS_Accommodation_Costs = 0, AS_Accommodation_Rent = T, AS_Area = 2L
    )
    has_required_output_names(emtr_output)

    #fwrite(emtr_output, 'ref/emtr_output_6.csv')

    test_against_ref(emtr_output, 'ref/emtr_output_6.csv')
  })
