library(tidyverse)
library(dslabs)

# Explore gapminder

head(gapminder,6)

# Prepare the nested dataframe gap_nested
gap_nested <- gapminder %>% group_by(country) %>% nest()

# Explore gap_nested
head(gap_nested)

# Create the unnested dataframe called gap_unnnested
gap_unnested <- gap_nested %>% unnest()

# Extract the data of Algeria
gap_nested$country[2]
algeria_df <- gap_nested$data[[2]]

# Calculate the minimum of the population vector
min(algeria_df$population, na.rm = TRUE)

# Calculate the maximum of the population vector
max(algeria_df$population, na.rm = TRUE)

# Calculate the mean of the population vector
mean(algeria_df$population, na.rm = TRUE)

#-------------------------------------------------------------------------------
# purrr -> map()
#-------------------------------------------------------------------------------
# Calculate the mean of the population by country
map(.x = gap_nested$data, .f = ~mean(.x$population, na.rm = TRUE ))
