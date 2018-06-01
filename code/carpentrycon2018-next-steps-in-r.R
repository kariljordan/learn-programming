# ---- Load packages ----
library(gapminder)
library(tidyverse)

# ---- Import Data ----
data("gapminder")
gm_car_deaths <- read_tsv("data/gapminder-car-deaths.txt")
gm_cells <- read_tsv("data/gapminder-cell-phones.txt")

# ---- Explore Data ----
str(gapminder)

# This command gives you the structure of the data, but it looks much better
glimpse(gapminder)
glimpse(gm_cells)
glimpse(gm_car_deaths)

# ---- Exploring Variables ----
# Grabbing the population column from the data set
gapminder %>%
  select(pop)

# What are all the column names for gm_cells
names(gm_cells)

# What if we want to look at names from the 1970s?
gm_cells %>%
  select(starts_with("197"))

# What if we want to look at names from columns that end in 5?
gm_cells %>%
  select(ends_with("5"))

# Columns that contain a
gapminder %>%
  select(contains("a"))

# All columns that happen between 1990 and 1997
gm_cells %>%
  select(num_range(prefix = "", range = 1995:1999))

# Only want to see when the country is Canada
gapminder %>%
  filter(country == "Canada")

# What if we only wanted countries with the letters ad in them?
gapminder %>%
  filter(grepl("ad", country)) %>% # grepl is a help function that looks for "ad" pattern in the country cell
  distinct(count)

# ---- Tidyng the data ----
View(gm_cells)

# We noticed that there is a function called cell_phones. Let's change it to be country
gm_cells_tidy <-
gm_cells %>%
  rename(country = cell_phones) %>%
# The data is in wide column format and we need to change it to long format
  gather(key = "year", 
         value = "cell_subscribers",
         -country) 

# key is to name the column that contains your important variable. Our key name is year.
# value is 
# - country means do not reshape country

# Do the same thing we did above, but for the car deaths data
View(gm_car_deaths)
gm_car_deaths_tidy <-  
  gm_car_deaths %>%
    rename(country = car_deaths_per_100k) %>%
    gather(key = "year",
           value = "car_deaths",
           -country)

# Now we want to look at ways to manipulate our data, but we need to merge it first.

# ---- Merge Data ----
# First, let's look at the first six rows
head(gapminder)
  
# We want to merge the car death info into this data frame
gapminder %>%
  left_join(gm_cells_tidy, by = c("country", "year")) #what columns do we have in common that we can join by?
# we joined by country and year

glimpse(gapminder)
glimpse(gm_cells_tidy)

# Before we can merge this we have to convert year to be an integer

gm_cells_tidy <- gm_cells_tidy %>%
  mutate(year = as.integer(year)) #creates new columns based on values from other columns

gapminder <- gapminder %>%
    left_join(gm_cells_tidy, by = c("country", "year")) 

# Merge gm_car deaths
gm_car_deaths_tidy <- gm_car_deaths_tidy %>%
  mutate(year = as.integer(year))

gapminder <- gapminder %>%
  left_join(gm_car_deaths_tidy, by = c("country", "year"))

glimpse(gapminder)

# ---- Manipulating Your Data ----
# We want cell subscribers to be an integer, and a percent of the population
gapminder %>%
  mutate(cell_subscribers = as.integer(cell_subscribers), 
         cell_subscribers_pct = cell_subscribers/pop) %>% # adding a column to the data for percentage of subscribers
# Lets select the columns we made
  select(starts_with("cell")) %>%
  tail() # looks at the bottom six rows

# We want cell subscribers to be an integer, and a percent of the population
gapminder <- gapminder %>% #overrides and updates the object
  mutate(cell_subscribers = as.integer(cell_subscribers), 
         cell_subscribers_pct = cell_subscribers / pop)

# This is a mutate helper
gapminder %>%
  mutate(lifeExp_lag = lag(lifeExp, 5)) %>% #creates a new column
  select(year, lifeExp, lifeExp_lag)
# The first 5 are NA because that data is not in the dataset


# What if you want to rank the values in a column
gapminder %>%
  mutate(pop_rank = dense_rank(pop)) %>% #dense_rank is the function 
  select(starts_with("pop"))

# If you need to scale the values in a column between 0 and 1
gapminder %>%
  mutate(pop_rank = percent_rank(pop)) %>% #percent_rank is the function to scale and normalize data
  select(starts_with("pop"))

# ---- Summarize Data ----
# Produce a table of max population by each country by year
# Case One: One function on one variable
gapminder %>%
  group_by(contitent) %>%
  summarize(max(pop))

# Case Two: Multiple functions on one variable
# Max and mean of pop by continent
gapminder %>%
  group_by(continent) %>%
  summarize(max(pop),
            mean(pop))
  
# OR
gapminder %>%
  group_by(continent) %>%
  summarize_at(vars("pop"), funs(max,mean)) #takes two peaces of info: what variable you want to summarize

# Case Three: One function on multiple variables
# Max of pop & lifeExp by continent
gapminder %>%
  group_by(continent) %>%
  summarize(max(pop),
            max(lifeExp))

# OR
gapminder %>%
  group_by(continent) %>%
  summarize_at(vars("pop", "lifeExp"), funs(max))

# Case Four: Multiple functions on multiple variables
# Maximum and mean of pop, lifeExp, and gdpPercap

gapminder %>%
  group_by(continent) %>%
  summarize_at(vars("pop", "lifeExp", "gdpPercap"), funs(max,mean)) %>%
  View()
        
# ---- Plotting ----
# % cell phones by continenet
ggplot(gapminder) +
  geom_line(aes(x = year, y = cell_subscribers_pct, 
                colour = country)) + 
  facet_wrap(~ continent) +
  theme(legend.position = "none")

# Only data after 1977
# Trick: You can pipe into ggplot
gapminder %>%
  filter(year > 1977) %>%
  ggplot() +
  geom_line(aes(x = year, y = cell_subscribers_pct, 
                colour = country)) + 
  facet_wrap(~ continent) +
  theme(legend.position = "none")

# Find country with max adoption rate of cell phones 
gapminder %>%
  filter(cell_subscribers_pct == max(cell_subscribers_pct, na.rm = TRUE)) %>%
# If you filter without the na.rm you'll get an error because you can't filter with NAs
  select(country, cell_subscribers_pct)

# We added a group_by so that we can repeat the filtering operation on each section of the dataset
gapminder %>%
  group_by(continent) %>%
  filter(cell_subscribers_pct == max(cell_subscribers_pct, na.rm = TRUE)) %>%
  select(country, cell_subscribers_pct)

# Now we will do a filtering join
max_countries <- gapminder %>%
  group_by(continent) %>%
  filter(cell_subscribers_pct == max(cell_subscribers_pct, na.rm = TRUE)) %>%
  select(continent, country)

gapminder %>%
  semi_join(max_countries, by = c("continent", "country")) %>%
  #distinct(country) # check to make sure it works
  filter(year > 1977) %>%
  ggplot() +
  geom_line(aes(x = year, y = cell_subscribers_pct, 
                colour = country)) + 
  facet_wrap(~ continent) +
  theme(legend.position = "none")

# What if we want to plot the top 3 countries?
top_3_countries <- gapminder %>%
  group_by(continent) %>%
  # desc() slice() arrange() are the functions you could use
  # OR you could use top_n()
  top_n(3, cell_subscribers_pct) %>%
  select(continent, country)

# Plot without the legend
gapminder %>%
  semi_join(top_3_countries, by = c("continent", "country")) %>%
  #distinct(country) # check to make sure it works
  filter(year > 1977) %>%
  ggplot() +
  geom_line(aes(x = year, y = cell_subscribers_pct, 
                colour = country)) + 
  facet_wrap(~ continent) +
  theme(legend.position = "none")


# Plot with the legend
gapminder %>%
  semi_join(top_3_countries, by = c("continent", "country")) %>%
  #distinct(country) # check to make sure it works
  filter(year > 1977) %>%
  ggplot() +
  geom_line(aes(x = year, y = cell_subscribers_pct, 
                colour = country)) + 
  facet_wrap(~ continent) 
# use interaction to add the continent with the country

# Another approach
gapminder %>%
  group_by(continent) %>%
  arrange(desc(cell_subscribers_pct)) %>%
  slice(1:3) # It says we want rows 1 through 3 for each group
