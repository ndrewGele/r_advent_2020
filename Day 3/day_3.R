library(readr)
library(dplyr)
library(stringr)

# Input
input <- read_table2("Day 3/input.txt", col_names = FALSE)

# Single column -> Multi Column
mountain <- str_split(string = input$X1, pattern = '', simplify = TRUE) %>%
  as_tibble() %>%
  mutate_all(.funs = function(x) {
    str_replace_all(string = x, pattern = '\\.', replacement = '0')
  }) %>%
  mutate_all(.funs = function(x) {
    str_replace_all(string = x, pattern = '\\#', replacement = '1')
  }) %>%
  mutate_all(.funs = as.integer)

# Handle the (horizontally) looping mountain
loop_right <- function(x, moves, limit) {
  x <- x + moves
  if(x > limit) x <- x - limit
  x
}

# Increment down mountain
get_next_pos <- function(cur_x, cur_y, down, right, y_limit) {
  x <- cur_x + down
  y <- loop_right(cur_y, right, y_limit)
  return(list(x = x, y = y))
}

# Sled down that mountain
sled_down_mountain <- function(mountain, down, right) {

  # Initialize some values
  x <- 1
  y <- 1
  trees <- 0

  # Traverse
  while(x <= nrow(mountain)) {
    trees <- trees + as.numeric(mountain[x,y])

    next_pos <- get_next_pos(
      cur_x = x,
      cur_y = y,
      down = down,
      right = right,
      y_limit = ncol(mountain)
    )

    x <- next_pos$x
    y <- next_pos$y
  }

  trees

}

# Part 1
sled_down_mountain(mountain = mountain, down = 1, right = 3)

# Part 2
sled_down_mountain(mountain = mountain, down = 1, right = 1) *
  sled_down_mountain(mountain = mountain, down = 1, right = 3) *
  sled_down_mountain(mountain = mountain, down = 1, right = 5) *
  sled_down_mountain(mountain = mountain, down = 1, right = 7) *
  sled_down_mountain(mountain = mountain, down = 2, right = 1)
