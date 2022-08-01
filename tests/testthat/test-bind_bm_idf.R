library(dplyr)
library(tibble)
library(rlang)

w <- tibble(
  document = rep(1:2, each = 5),
  word = c(
    "the", "quick", "brown", "fox", "jumped",
    "over", "the", "lazy", "brown", "dog"
  ),
  frequency = c(
    1, 1, 1, 1, 2,
    1, 1, 1, 1, 2
  )
)

test_that("Can calculate BM-IDF", {
  result <- w %>%
    bind_bm_idf(word, document, frequency, threshold = 1)
  result2 <- w %>%
    bind_bm_idf("word", "document", "frequency", threshold = 1)
  expect_equal(result, result2)

  expect_equal(
    select(w, document, word, frequency),
    select(result, document, word, frequency)
  )

  expect_s3_class(result, "tbl_df")
  expect_type(result$bm, "double")
  expect_type(result$idf, "double")
  expect_type(result$bm_idf, "double")

  expect_equal(result$bm, rep(1, 10))
  expect_equal(result$idf[1:4], c(0, log(2), 0, log(2)))
  expect_equal(result$bm_idf, result$bm * result$idf)

  # preserves but ignores groups
  result2 <- w %>%
    group_by(document) %>%
    bind_bm_idf(word, document, frequency)

  expect_equal(length(groups(result2)), 1)
  expect_equal(as.character(groups(result2)[[1]]), "document")
})

test_that("Thresholding works", {
  result <- w %>%
    bind_bm_idf(word, document, frequency, threshold = 2)

  expect_equal(result$bm, c(rep(0, 4), 1, rep(0, 4), 1))
  expect_equal(result$idf[1:4], c(0, log(2), 0, log(2)))
  expect_equal(result$bm_idf, result$bm * result$idf)
})

test_that("BM-IDF works when the document ID is a number", {
  # example thanks to https://github.com/juliasilge/tidytext/issues/31
  my_corpus <- dplyr::tibble(
    id = rep(c(2, 3), each = 3),
    word = c("an", "interesting", "text", "a", "boring", "text"),
    n = c(1, 1, 3, 1, 2, 1)
  )

  bm_idf <- bind_bm_idf(my_corpus, word, id, n)
  expect_false(any(is.na(bm_idf)))
  expect_equal(bm_idf$bm_idf[c(3, 6)], c(0, 0))
})


test_that("tf-idf with tidyeval works", {
  d <- tibble(txt = c(
    "Because I could not stop for Death -",
    "He kindly stopped for me -"
  ))
  termvar <- quo("word")
  documentvar <- quo("document")
  countvar <- quo("frequency")

  result <- w %>%
    bind_bm_idf(!!termvar, !!documentvar, !!countvar)

  expect_equal(
    select(w, document, word, frequency),
    select(result, document, word, frequency)
  )

  expect_s3_class(result, "tbl_df")
  expect_type(result$bm, "double")
  expect_type(result$idf, "double")
  expect_type(result$bm_idf, "double")

  expect_equal(result$bm, rep(1, 10))
  expect_equal(result$idf[1:4], c(0, log(2), 0, log(2)))
  expect_equal(result$bm_idf, result$bm * result$idf)

  result2 <- w %>%
    group_by(document) %>%
    bind_bm_idf(!!termvar, !!documentvar, !!countvar)

  expect_equal(length(groups(result2)), 1)
  expect_equal(as.character(groups(result2)[[1]]), "document")
})
