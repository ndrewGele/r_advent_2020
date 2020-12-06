library(readr)
library(dplyr)
library(stringr)
library(purrr)

# Input
input <- read_table2("Day 2/input.txt", col_names = FALSE) %>%
  rename(password = X3)

# Part 1
minmax <- str_split(input$X1, '-', simplify = TRUE) %>%
  as_tibble() %>%
  rename(min = V1, max = V2)

cleaned <- input %>%
  bind_cols(minmax) %>%
  mutate(
    min = as.numeric(min),
    max = as.numeric(max),
    letter = str_remove(string = X2, pattern = ':')
  )

passtest <- cleaned %>%
  mutate(
    occurences = str_count(string = password, pattern = letter),
    test = occurences >= min & occurences <= max
  ) %>%
  filter(test == TRUE)

# Part 2
positions <- str_split(input$X1, '-', simplify = TRUE) %>%
  as_tibble() %>%
  rename(pos1 = V1, pos2 = V2)

cleaned <- input %>%
  bind_cols(positions) %>%
  mutate(
    pos1 = as.numeric(pos1),
    pos2 = as.numeric(pos2),
    letter = str_remove(string = X2, pattern = ':')
  )

passtest <- cleaned %>%
  mutate(
    locations = purrr::map2(
      .x = password,
      .y = letter,
      .f = function(x,y) {
        res <- str_locate_all(string = x, patter = y)
        res <- unique(unlist(res))
        res
      }
    ),
    test1 = purrr::map2_lgl(
      .x = pos1,
      .y = locations,
      .f = function(x,y) {
        res <- x %in% y
        res
      }
    ),
    test2 = purrr::map2_lgl(
      .x = pos2,
      .y = locations,
      .f = function(x,y) {
        res <- x %in% y
        res
      }
    ),
    testsum = test1 + test2
  ) %>%
  filter(testsum == 1)
