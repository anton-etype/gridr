test_that("Run create grid",{
  grid <- createGrid(gridr_example("best.gpkg"), "best", 20, 20, c(-5, -2))
  type <- toString(unique(sf::st_geometry_type(grid)))
  expect_match(type, "POINT", ignore.case = TRUE)
})

test_that("Run get subsamples",{
  grid <- createGrid(gridr_example("best.gpkg"), "best", 20, 20, c(-5, -2))
  sg <- getSubsample(grid)

  expect_true(all(c("dist", "dist_rank") %in% colnames(sg)))

})
