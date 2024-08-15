# nested for loop examples ----

file_prefix <- c("temp", "ph", "salinity")
file_suffix <- c(1, 2, 3, 4)

for (i in 1:length(file_prefix)) {
  for (j in 1:length(file_suffix)) {
    print(paste0(file_prefix[i], "_", file_suffix[j]))
  }
}

for (i in 1:length(file_suffix)) {
  for (j in 1:length(file_prefix)) {
    print(paste0(file_prefix[j], "_", file_suffix[i]))
  }
}

# ----
 
odds <- c(1, 3, 5)
evens <- c(2, 4, 6, 8)

for (i in seq_along(odds)) {
  for (j in seq_along(evens)) {
    print(odds[i] * evens[j])
  }
}


# making functions ----

birddog_sum <- function(bird, dog) {
  pets <- bird + dog
  return(pets)
}

x <- birddog_sum(bird = 2, dog = 5)

double_it <- function(x) {
  print(2 * x)
}

double_it(x = 4)
double_it(1:4)

exclaim_age <- function(age) {
  print(paste("I am", age, "years old!"))
}

exclaim_age(age = 27)


# print() vs return() ----

find_max <- function(val1, val2) {  
  if (val1 > val2) {
    return(val1)
  } else if (val2 > val1) {
    return(val2)
  }
}

find_max(7, 3)
# return() is always best practice because it stores the output to be used later



# for loop practice ----

quarter_splits <- c(1.0, 1.1, 1.2, 1.1, 1.4, 1.5, 1.6, 1.4)

for (i in seq_along(quarter_splits)) {
  print(quarter_splits[i] + quarter_splits[i + 1])
}

# functions with conditionals ----

animal_age <- function(animal, age) {
  if (animal == "dog" ) {
    print(age * 7)
  } else if (animal == "goat") {
    print(age * 4.7)
  }
}

animal_age(animal = "dog", age = 8)
animal_age(animal = "cow", age = 2)
animal_age(animal = "dog", age = "yellow")


# ----
library(tidyverse)

dog_choice <- data.frame(dog_name = c("Khora",
                                      "Teddy",
                                      "Waffle",
                                      "Banjo"),
                         food = c("everything",
                                  "salmon",
                                  "pancakes",
                                  "chicken"))

dog_menu <- function(name) {
  my_sub <- dog_choice |>
  dplyr::filter(dog_name == name)

print(paste("My name is", my_sub$dog_name, "and I like to eat", my_sub$food))
}

dog_menu("Khora")
dog_menu("Teddy")

# adding error and warning messages into functions ----

animal_age <- function(animal, age) {
  
  if (!animal %in% c("dog", "goat")) {
    stop("Oops! Animal must be a dog or goat")
  }
  
  if (is.numeric(age) == FALSE) {
    stop("The age must be a number")
  }
  
  if (age <= 0) {
    stop("Age must be greater than 0")
  }
  
  if (animal == "dog" ) {
    print(age * 7)
  } else if (animal == "goat") {
    print(age * 4.7)
  }
}

animal_age(animal = "cow", age = 2)
animal_age("dog", "yellow")
animal_age("dog", -2)

# ----

calc_windpower <- function(rho, radius, windspeed) {
  
  if (windspeed > 130) {
    warning("wow, that's some fast wind! are you sure?")
  }
  
  if (rho > 1.225) {
    warning("that aitr density is suspicious. are you sure?")
  }
  
  if (radius < 0) {
    stop("rotor radius must be a positive value (meters).")
  }
  
  print(0.3 * rho * pi * (radius^2) * (windspeed^3))
}


calc_windpower(rho = 1, radius = 10, windspeed = 500)

# ----

























