library(readr)
library(dplyr)

# Importing data
input <- read_csv("Day 1/input.txt", col_names = FALSE)

# Part 1
combo <- expand.grid(input$X1, input$X1)

result <- combo %>%
  mutate(sum = Var1 + Var2) %>%
  filter(sum == 2020) %>%
  mutate(product = Var1 * Var2)

# Part 2
combo <- expand.grid(input$X1, input$X1, input$X1)

result <- combo %>%
  mutate(sum = Var1 + Var2 + Var3) %>%
  filter(sum == 2020) %>%
  mutate(product = Var1 * Var2 * Var3)
