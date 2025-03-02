# Introduction to R
# Environmental Science Lab EVS 3000L - Spring 2025
# Exercise developed by Ana Y. Y. Meiga and Isaac Coleman

# Objectives:
# 1. Import files to R
# 2. Practice common data cleaning steps using tidyverse
# 3. Practice basic functions and calculations
# 4. Practice basic data visualization in ggplot and mapview

# Install necessary packages ------
# Note that you will need to install any packages only once in your computer. After that, you can either delete the lines of code 14, 15, and 16, or comment using '#' before the text. When installing other packages, you can also use the function 'install.packages' in the console - this way the code will not be saved on your script but will install the package in your computer in the same way.

# install.packages("readxl")
# install.packages("sf")
# install.packages("mapview")
# install.packages("tidyverse")

# After installing the packages, you need to load them to start using. The way you load the packages is using the code 'Library'. It is good practice to always load all the libraries you will use in the beginning of the code. The order that you call the libraries matters! Since many of the packages have the same name of the functions (but work completely different in the background), the functions from the last library you called will be the one working.
# For me personally, since the 'tidyverse' package is the package that has many of the manipulation arguments, I always load 'tidyverse' as the last package.

# load libraries ------
library(readxl) # read excel files
library(sf) # package to deal with GIS systems
install.packages("mapview")
library(mapview) # to visualize maps
library(tidyverse) # one of the most useful packages

# When you load the libraries, you often see the information below:
# library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.4
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

# This tells you that tidyverse is loading the ggplot2, tibble, tidyr, readr, purrr, and dplyr packages. These are considered to be the core of the tidyverse because you’ll use them in almost every analysis. You can also see the conflicts functions. For example, note that the function 'filter' is available in 'dplyr' (is part of tidyverse) and 'stats'. You can either define which function you want your code to be running, or call the name of the package you want to use before the function using the following:

# stats::filter()

# In general, if you are only calling the function a few times, it can be easier to use the approach above. But if you need to use the function many times in your code, it can be annoying. Thus, another way to set the functions from different packages to work after loading the libraries in different order is to use the codes below to indicate the which function you want from each package to be the prevalent. The first word inside the function refers to the set select and filter to come from tidyverse.
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("plot", "ctmm")

# Exercises ------
# Load one file ------
# If you want to load one single file, you can load it using the following code
data <- read_excel("data/group_1_sps_id.xlsx")

# check the data
data

# working with date and time can be very annoying! One need to be very carefully. Note that the date in the column date is correct, however, the dates for the time have completely wrong dates. The time is correct, but the date is wrong. We will learn how to deal with it in a minute.

# Load multiple files -----
data_all <- list.files("data", # define the path of the files
                       pattern = "id.xlsx", # define the pattern of the files you want to read
                       full.names = TRUE) |>  # read full name
  purrr::map_dfr(~ read_excel(.))

# This code will give you an error. This is one of the common errors when different data frames has different data types (character in one file and double in another). To resolve this, you need to ensure that the count column has the same data type across all files before combining them.

# To fix that, let's use a function to read and convert the count column to numeric
read_and_convert <- function(file) {
  data <- read_excel(file)
  data <- data |>
    mutate(count = as.numeric(count))
  return(data)
}

# Using the function we created
data_all <- list.files("data", # define the path of the files
                       pattern = "id.xlsx", # define the pattern of the files you want to read
                       full.names = TRUE) |>  # read full name
  purrr::map_dfr(~ read_and_convert(.)) # use the new function you created

# This code will give you a warning. Warnings is just to call your attention to something, but it is different than an error. An error won't allow you to move forward, while the warning will. In this case, the warning is telling you that your dataset has NAs (empty values). R gives you this warning because some functions will not work with NAs in the columns. We will need to remove them or add some information to it.

# After loading data and manipulating your data, running different functions, it is always good practice to check the data. One function I like to check the data is 'glimpse', because it tells you what kind of data you have. Let's have a look:
glimpse(data_all)

# The glimpse function provides a quick overview of the data frame. This is useful for getting a sense of the structure and contents of the data at a glance. We can also see that we have the NAs in our data
# - It displays the number of rows and columns in the data frame.
# - It shows the names of the columns and their respective data types (e.g., character, numeric, date).
# - It provides a preview of the first few values in each column.


# Basic functions -----
# Now, let's check some basic functions we can do using R.

# Count number of species
data_all |>
  distinct(species) |>
  count()
# Note that this single code give us the total number, but it doesn't tell much.

# Count the number of unique species and return their names along with the counts
data_all |>
  group_by(species) |>
  summarize(count = n()) |>
  arrange(desc(count))

# I WILL ADD MORE BASIC CALCULATIONS HERE
# SHOW HOW TO REMOVE NAs
# BASIC PLOTS
# BIODIVERSITY INDEXES CALCULATIONS

# As sf -----
# Create an sf object
data_all_sf <- st_as_sf(data_all,
                        coords = c("long", "lat"),
                        crs = 4326) # set coordinates system as WGS84

# This code will give you an error because the function doesn't allow you to have NAs in the column.

# Remove NAs values
data_filter <- data_all |>
  filter(!is.na(long) & !is.na(lat)) # filter() remove lines where the columns long and lat has NAs

# Note that our data went from 81 observations to 27 observations! (check objects in the environment on the top right)

# Create an sf object with the cleaned data
data_sf <- st_as_sf(data_filter,
                        coords = c("long", "lat"),
                        crs = 4326) # set coordinates system as WGS84

# Mapview ----
mapview(data_sf)

# We can clear see that some of the coordinates were not correct. This is a common error that happens when taking notes or when passing the data to the excel.

# We can use mapview to show more info other than the location, for example, let's use colors to see different species
mapview(data_sf, zcol = "species")

# Plotting with ggplot ------
ggplot(data_filter, aes(x = long, y = lat, color = species)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Species distribution",
       x = "Long",
       y = "Lat",
       color = "Species")

# REMOVE OUTLIERS AND PLOT AGAIN
# ADD MAP COMPONENT TO THE PLOT

# Save files as unique files ------
data_all |>
  writexl::write_xlsx("data/sps_id_all.xlsx")


# Load tree data -----
tree <- list.files("data", # define the path of the files
                       pattern = "measurement.xlsx", # define the pattern of the files you want to read
                       full.names = TRUE) |>  # read full name
  purrr::map_dfr(~ read_excel(.))

# Save files -----
tree |>
  writexl::write_xlsx("data/tree_measurement_all.xlsx")

