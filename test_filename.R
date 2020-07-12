filepath <- system.file("extdata", package = "FARSfun")

expect_that(make_filename(2013:2015),
            equals(c(paste(filepath, "accident_2013.csv.bz2", sep = "/"),
                     paste(filepath, "accident_2014.csv.bz2", sep = "/"),
                     paste(filepath, "accident_2015.csv.bz2", sep = "/"))))
