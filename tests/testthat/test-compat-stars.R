test_that("stars argument coercers work", {
  skip_if_not_installed("stars")

  obj <- stars::st_as_stars(sf::st_bbox(c(xmin = 0, xmax = 1, ymin = 0, ymax = 1)))
  expect_error(
    as_qgis_argument(obj),
    "Can't convert 'stars' object"
  )

  tmp_file <- expect_match(
    suppressWarnings(as_qgis_argument(obj, qgis_argument_spec(qgis_type = "layer"))),
    "\\.tif$"
  )
  expect_s3_class(tmp_file, "qgis_tempfile_arg")
  unlink(tmp_file)

  # also check stars_proxy
  obj <- stars::read_stars(
    system.file("longlake/longlake_depth.tif", package = "qgisprocess"),
    proxy = TRUE
  )
  expect_equal(
    normalizePath(as_qgis_argument(obj, qgis_argument_spec(qgis_type = "layer"))),
    normalizePath(system.file("longlake/longlake_depth.tif", package = "qgisprocess"))
  )

  obj <- stars::read_stars(
    system.file("longlake/longlake.tif", package = "qgisprocess"),
    proxy = TRUE
  )
  cat("stars version:", as.character(packageVersion("stars")))
  sf::sf_extSoftVersion()
  cat("obj:\n")
  print(obj)
  expect_equal(
    normalizePath(as_qgis_argument(obj, qgis_argument_spec(qgis_type = "layer"))),
    normalizePath(system.file("longlake/longlake.tif", package = "qgisprocess"))
  )

  expect_warning(
    as_qgis_argument(obj, qgis_argument_spec(qgis_type = "multilayer")),
    "extract the bands"
  )

  # check behaviour in case of band selection
  obj1 <- obj[, , , 2]
  withAutoprint({
    print(obj1)
    cat("class of obj1:")
    print(class(obj1))
    as_qgis_argument(obj1, qgis_argument_spec(qgis_type = "layer"))
    x <- obj1
    dim(x)["band"]
    (file <- unclass(x)[[1]])
    stringr::str_to_lower(tools::file_ext(file))
    dim(stars::read_stars(file, proxy = TRUE))["band"]
  })

  res <- expect_message(
    as_qgis_argument(obj1, qgis_argument_spec(qgis_type = "layer")),
    "Rewriting"
  )
  cat(res, "\n")
  expect_s3_class(res, "qgis_tempfile_arg")
})


test_that("stars result coercers work", {
  skip_if_not_installed("stars")

  expect_s3_class(
    stars::st_as_stars(
      structure(
        system.file("longlake/longlake.tif", package = "qgisprocess"),
        class = "qgis_outputRaster"
      )
    ),
    "stars"
  )

  expect_s3_class(
    stars::st_as_stars(
      structure(
        system.file("longlake/longlake.tif", package = "qgisprocess"),
        class = "qgis_outputLayer"
      )
    ),
    "stars"
  )

  expect_s3_class(
    stars::st_as_stars(
      structure(
        list(
          OUTPUT = structure(
            system.file("longlake/longlake.tif", package = "qgisprocess"),
            class = "qgis_outputRaster"
          )
        ),
        class = "qgis_result"
      )
    ),
    "stars"
  )
})
