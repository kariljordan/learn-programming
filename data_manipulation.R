# data manipulation in R
gapminder <- read.csv("http://kbroman.org/datacarp/gapminder.csv")

head(gapminder)
tail(gapminder)
tail(gapminder, 2)
str(gapminder)

# subsetting data frames
gapminder[1 , 1]
gapminder[3, 5]
gapminder[3, ]
gapminder[,5]
gapminder[80, "lifeExp"]
gapminder$lifeExp[80]

# dplyr for manipulating data
library(dplyr)

# filter : choose a subset of rows
filter(gapminder, country=="Sweden")
filter(gapminder, country=="Sweden",
       year < 1969)

# challenge
filter(gapminder, country=="United States", year==1952)

# dplyr function: select - to choose columns
us1952 <- filter(gapminder, country=="United States", year==1952)

sweden <- filter(gapminder, country=="Sweden")
select(sweden, year, pop, lifeExp)

# another approach
select( filter(gapminder, country=="Sweden") , year, pop, lifeExp)

# yet another approach : "pipe operator" %>%
filter(gapminder, country=="Sweden") %>% 
    select(year, pop, lifeExp)

# Challenge 5
filter(gapminder, gdpPercap >= 35000) %>% 
    select(country, year, gdpPercap)

filter(gapminder, gdpPercap >= 35000) %>% 
    select(country, year, gdpPercap) %>% 
    arrange(gdpPercap)

filter(gapminder, gdpPercap >= 35000) %>% 
    select(country, year, gdpPercap) %>% 
    arrange(desc(gdpPercap)) %>% 
    head()

# mutate: adding columns (usually with arithmetic)
mutate(gapminder, total_gdp_billions = gdpPercap * pop / 1e9)

mutate(gapminder, total_gdp_billions = gdpPercap * pop / 1e9) %>% 
    head()

# challenge 6
mutate(gapminder, total_gdp_billions = gdpPercap * pop / 1e9) %>% 
    filter(year == 2007) %>% 
    arrange(desc( total_gdp_billions ))

# summarize: summarizes a column
summarize(gapminder, mean_pop= mean(pop))
summarize(gapminder, mean_lifeExp= mean(lifeExp))

gapminder %>% filter(year==2007) %>% 
    summarize(mean_lifeExp= mean(lifeExp))

gapminder %>% filter(year==1952) %>% 
    summarize(mean_lifeExp= mean(lifeExp))

# group_by: used *with* summarize, to get within-group summaries
gapminder %>% filter(year==2007) %>% 
    group_by(continent) %>% 
    summarize(ave_pop = mean(pop))

gapminder %>% filter(year==2007) %>% 
    group_by(continent) %>% 
    summarize(mean_pop = mean(pop),
              median_pop=median(pop),
              number=n())

# challenge 7
gapminder %>% filter(year==2007) %>% 
    group_by(continent) %>% 
    summarize(mean_lifeExp = mean(lifeExp), n=n())
              
# group_by, grouping by multiple columns
gapminder %>% 
    group_by(continent, year) %>% 
    summarize(mean_lifeExp = mean(lifeExp)) %>% 
    arrange(mean_lifeExp)

# challenge 8
gapminder %>% filter(year == 1952 | year == 2007)

gapminder %>% 
    filter(year %in% c(1952,2007)) %>% 
    mutate(gdp = gdpPercap * pop) %>% 
    group_by(continent, year) %>% 
    summarize(total_gdp = sum(gdp), 
              total_pop = sum(pop*1.0)) %>% 
    mutate(overall_gdpPercap=total_gdp/total_pop)
