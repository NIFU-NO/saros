test_that("make_content dispatches to correct method", {
  # Test that make_content calls the correct S3 method
  expect_error(
    make_content("nonexistent_type"),
    "Invalid make_content-type: nonexistent_type"
  )
})

test_that("make_content.default provides helpful error message", {
  # Test the default method error
  expect_error(
    make_content.default("invalid_type"),
    "Invalid make_content-type: invalid_type"
  )

  expect_error(
    make_content.default("another_invalid"),
    "Check that you have loaded the required packages"
  )
})

test_that("get_makeme_types returns available content types", {
  types <- get_makeme_types()

  expect_type(types, "character")
  expect_true(length(types) > 0)

  # Should not include the default method
  expect_false("default" %in% types)

  # Should include known built-in types
  expect_true(any(grepl("cat_", types)))
  expect_true(any(grepl("table_html", types)))
})

test_that("get_makeme_types removes make_content prefix", {
  types <- get_makeme_types()

  # None should start with "make_content."
  expect_false(any(grepl("^make_content\\.", types)))
})

test_that("get_makeme_types includes expected pattern types", {
  types <- get_makeme_types()

  # Check for expected patterns in built-in types
  categorical_types <- types[grepl("^cat_", types)]
  character_types <- types[grepl("^chr_", types)]
  integer_types <- types[grepl("^int_", types)]

  expect_true(length(categorical_types) > 0)
  expect_true(length(character_types) > 0)
  expect_true(length(integer_types) > 0)
})

test_that("make_content function signature is correct", {
  # Check that make_content accepts type and ... arguments
  expect_no_error(formals(make_content))

  formal_args <- names(formals(make_content))
  expect_true("type" %in% formal_args)
  expect_true("..." %in% formal_args)
})

test_that("make_content class assignment works conceptually", {
  # Test that the function signature supports class assignment
  expect_true(is.function(make_content))

  # Test that class assignment doesn't break basic function calls
  expect_error(make_content("nonexistent_type"), "Invalid make_content-type")
})
