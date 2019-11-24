#intro to writing functions. 

#getting some best practice in, this code shows the arguments of a function
# Note the arguments to median()
args(median)

# Rewrite this function call, following best practices, to make NA values go last.
rank(-gold_medals, na.last = "keep",ties.method = "min")

#suggested function writing
#make a template
#paste in the script
#choose the arguments
#replace specific argument names
#make specific variable names more general
#remove a final assignment

# Your functions, from previous steps - simulate a coin toss
toss_coin <- function() {
  coin_sides <- c("head", "tail")
  sample(coin_sides, 1)
}

# Call your function
toss_coin()

# Update the function to return n coin tosses
toss_coin <- function(n_flips) {
  coin_sides <- c("head", "tail")
  sample(coin_sides, size = n_flips, replace = TRUE)
}

# Generate 10 coin tosses
toss_coin(n_flips = 10)

#though this was an interesting way to assign probability
coin_sides <- c("head", "tail")
n_flips <- 10
p_head <- 0.8
# Define a vector of weights
weights <- c(p_head, 1 - p_head)
# Update so that heads are sampled with prob p_head
sample(coin_sides, n_flips, replace = TRUE, prob = weights)


# Update the function so heads have probability p_head
toss_coin <- function(n_flips, p_head) {
  coin_sides <- c("head", "tail")
  # Define a vector of weights
  weights <- c(p_head, 1 - p_head)
  # Modify the sampling to be weighted
  sample(coin_sides, n_flips, replace = TRUE, prob = weights)
}

# Generate 10 coin tosses
toss_coin(n_flips = 10, p_head = 0.8)


# Run a generalized linear regression, this points out that the formula is first and dataset second,
#meaning this wont work in a pipe scenario
glm(
  # Model no. of visits vs. gender, income, travel
  n_visits ~ gender + income + travel, 
  # Use the snake_river_visits dataset
  data = snake_river_visits, 
  # Make it a Poisson regression
  family = poisson
)

# Write a function to run a Poisson regression
run_poisson_regression <- function(data, formula) {
  glm(formula, data, family = poisson)
}

# From previous step
run_poisson_regression <- function(data, formula) {
  glm(formula, data, family = poisson)
}

# Re-run the Poisson regression, using your function
model <- snake_river_visits %>%
  run_poisson_regression(n_visits ~ gender + income + travel)

# Run this to see the predictions
snake_river_explanatory %>%
  mutate(predicted_n_visits = predict(model, ., type = "response")) %>%
  arrange(desc(predicted_n_visits))


