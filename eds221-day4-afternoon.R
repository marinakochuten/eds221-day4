# Let's create a data frame:

# Now let's write a function that automatically switches depth based on
gw_rate <- function(site) {
  
  if(!site %in% c("mountain", "prarie", "desert", "beach")) {
    warning("site not included")
  }
  
  # Stored parameters for 4 different sites
  gw_depths <- data.frame(sitename = c("mountain",
                                     "prarie",
                                     "desert",
                                     "beach"),
                        depth = c(32, 41, 63, 2),
                        slope = c(11.2, 0.4, 0.8, 2.6))

  # Subset for just that site information (creates a 1-row data frame)
  site_select <- filter(gw_depths, sitename == site)

  # Calculate using values from that 1-row data frame
  transport_rate <- 1.4 * site_select$slope + 3.6 * site_select$depth

  # Return the output
  return(transport_rate)
}

gw_rate(site = "beach")


# logistic growth example

# build the minimal function
logistic_growth <- function(N0, K, r, time) {
  Nt <- K / (1 + ((K - N0) / N0) * exp(-r *time))
  return(Nt)
}

# Do the values check out when you test this? 
logistic_growth(N0 = 100, K = 6000, r = 0.27, time = 40)

# Now, let’s explore what this looks like over a whole sequence of times (e.g. from t = 0 to t = 40)

# Create a vector of times:
time_vec <- seq(from = 0, to = 50, by = 0.1)

# Apply the logistic growth function to that vector of times (& store):
pop_1 <- logistic_growth(N0 = 100, K = 6000, r = 0.27, time = time_vec)

# can also do this in a for loop
pop_1_vec <- vector(mode = "numeric", length = length(time_vec))

for (i in seq_along(time_vec)) {
  population <- logistic_growth(N0 = 100, K = 6000, r = 0.27, time = time_vec[i])
  pop_1_vec[i] <- population
}

# Bind together the time_vec and population:
pop_time_1 <- data.frame(time_vec, pop_1_vec)

# plot the data
ggplot(data = pop_time_1, aes(x = time_vec, y = pop_1_vec)) +
  geom_line()


# we may want to apply our function over a range of growth rates

# Create a sequence of growth rate values:
r_seq <- seq(from = 0.2, to = 0.4, by = 0.01)

# Create a for loop that goes through each, apply the logistic growth function for a range of times for each growth rate

# Need to create a MATRIX to store the outputs in:
out_matrix <- matrix(nrow = length(time_vec), ncol = length(r_seq))

# Now, a nested for loop:

for (i in seq_along(r_seq)) {  # outer loop of growth rates
  for (j in seq_along(time_vec)) { # inner loop of time steps
    population <- logistic_growth(N0 = 100, K = 6000, r = r_seq[i], time = time_vec[j])
    out_matrix[j,i] <- population
  } 
}

# Let's wrangle it a little bit 
out_df <- data.frame(out_matrix, time = time_vec)   # Make it a data frame and add time

# Update the column names of out_df, keeping time column name the same
colnames(out_df) <- c(paste0("growth_rate_", r_seq), "time")

# pivot_longer to make it tidy (you'll learn more about this next week)
out_df_long <- out_df |>
  pivot_longer(cols = -time, names_to = "growth_rate", values_to = "population_size")

# Then plot it: 
ggplot(data = out_df_long, aes(x = time, y = population_size)) +
  geom_line(aes(color = growth_rate), show.legend = FALSE) +
  theme_minimal()







