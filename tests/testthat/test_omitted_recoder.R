testthat::test_that("default usage", code = {
	### Testing that omitted_recoder_df works
	# Input
	input <- rlang::set_names(as.data.frame(matrix(c(
		1,0,1,0,1, # All present
		NA,0,1,0,1, # First missing
		NA,NA,1,0,1, # First two missing
		1,0,NA,0,1, # One in middle missing
		1,NA,NA,NA,1, # All in the middle missing
		1,0,1,0,NA, # Last one missing
		1,0,1,NA,NA, # Last two missing
		1,0,NA,NA,NA, # Last three missing
		NA,NA,NA,NA,NA # All missing
	), nrow = 9, byrow = T)), nm=paste0("X", 1:5))
	# What should be the output for item estimation according to Mislevy
	# Skipped=> 0, not_administered=>NA, all_missing=>NA
	y_i <-  rlang::set_names(as.data.frame(matrix(c(
		1,0,1,0,1, # All present
		0,0,1,0,1, # First missing
		0,0,1,0,1, # First two missing
		1,0,0,0,1, # One in middle missing
		1,0,0,0,1, # All in the middle missing
		1,0,1,0,0, # Last one missing
		1,0,1,0,NA, # Last two missing
		1,0,0,NA,NA, # Last three missing
		NA,NA,NA,NA,NA # All missing
	), nrow = 9, byrow = T)), nm=paste0("X", 1:5))
	
	# What should be the output for person estimation according to Mislevy
	# Skipped=> 0, not_administered=>NA, all_missing=>NA
	y_p <- rlang::set_names(as.data.frame(matrix(c(
		1,0,1,0,1, # All present
		0,0,1,0,1, # First missing
		0,0,1,0,1, # First two missing
		1,0,0,0,1, # One in middle missing
		1,0,0,0,1, # All in the middle missing
		1,0,1,0,0, # Last one missing
		1,0,1,0,0, # Last two missing
		1,0,0,0,0, # Last three missing
		0,0,0,0,0 # All missing
	), nrow = 9, byrow = T)), nm=paste0("X", 1:5))
	# Recoding for counting skipped, not_administered, all_missing, etc
	# Skipped=> 99, not_administered=>999, all_missing=>9999
	y_info <- rlang::set_names(as.data.frame(matrix(c(
		1,0,1,0,1, # All present
		99,0,1,0,1, # First missing
		99,99,1,0,1, # First two missing
		1,0,99,0,1, # One in middle missing
		1,99,99,99,1, # All in the middle missing
		1,0,1,0,99, # Last one missing
		1,0,1,99,999, # Last two missing
		1,0,99,999,999, # Last three missing
		9999,9999,9999,9999,9999 # All missing
	), nrow = 9, byrow = T)), nm=paste0("X", 1:5))
	
	
	
	y_i2 <- omitted_recoder_df(input) #Mislevy item estimation
	y_p2 <- omitted_recoder_df(input, skipped = 0L, not_administered = 0L, all_missing = 0L) #Mislevy person param estimation
	y_info2 <- omitted_recoder_df(input, skipped = 99, not_administered = 999, all_missing = 9999)
	testthat::expect_identical(y_i, y_i2)
	testthat::expect_identical(y_p, y_p2)
	testthat::expect_identical(y_info, y_info2)
	
	testthat::expect_error(omitted_recoder_df(input[,4]), regex="Vectors not accepted.") # Should fail
	testthat::expect_identical(omitted_recoder_df(input[,4], accept_vector=T), c(0,0,0,0,0,0,0,NA,NA))
	testthat::expect_warning(identical(omitted_recoder_df(input[,4, drop=F]), input[,4, drop=F]), 
							 regexp = "Unable to recode single-column data.frame without knowing context.") 
})