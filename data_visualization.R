# data visualization in R

# load the ggplot2 packages
library(ggplot2)

# load the data
gapminder <- read.csv("http://kbroman.org/datacarp/gapminder.csv")

# a first graph
ggplot(gapminder, aes(x=gdpPercap, y=lifeExp)) + geom_point()

# this does the same thing:
p1 <- ggplot(gapminder, aes(x=gdpPercap, y=lifeExp))
p2 <- p1 + geom_point()

# change x-axis scale to be a log scale
p2 + scale_x_log10()

# challenge 9
library(dplyr)
china <- gapminder %>% filter(country == "China")
ggplot(china, aes(x=gdpPercap, y=lifeExp)) + geom_point()

gapminder %>% 
    filter(country=="China") %>% 
    ggplot(aes(x=gdpPercap, y=lifeExp)) +
    geom_point()

# play with aesthetics
gapminder %>% 
    filter(country=="China") %>% 
    ggplot(aes(x=gdpPercap, y=lifeExp)) +
    geom_point(size=10, color="violetred")

# color == continent
gapminder %>% 
    ggplot(aes(x=gdpPercap, y=lifeExp, color=continent)) +
    geom_point() +
    scale_x_log10()

gapminder %>% 
    ggplot(aes(x=gdpPercap, y=lifeExp, 
               color=continent, size=pop)) +
    geom_point(alpha=0.3) +
    scale_x_log10()

# layers 
china <- gapminder %>% filter(country=="China")
ggplot(china, aes(x=gdpPercap, y=lifeExp)) +
    geom_line(color="lightblue") + 
    geom_point(color="violetred")

# lines on top of points
ggplot(china, aes(x=gdpPercap, y=lifeExp)) +
    geom_point(color="violetred") +
    geom_line(color="lightblue")
    
# points colored by year
ggplot(china, aes(x=gdpPercap, y=lifeExp)) +
    geom_point(aes(color=year)) +
    geom_line(color="lightblue")

# global aesthetics
ggplot(china, aes(x=gdpPercap, y=lifeExp, color=year)) +
    geom_point() +
    geom_line()

# yet another way...
ggplot(china, aes(x=gdpPercap, y=lifeExp)) +
    geom_point() +
    geom_line() +
    aes(color=year)

# challenge 11
filter(gapminder, country %in% c("India", "China")) %>% 
    ggplot(aes(x=gdpPercap, y=lifeExp, color=country)) +
    geom_point() +
    geom_line()

# faceting 
ggplot(gapminder, aes(x=gdpPercap, y=lifeExp, 
                      color=continent)) +
    geom_point() + scale_x_log10() + facet_wrap( ~ year)

ggplot(gapminder, aes(x=gdpPercap, y=lifeExp)) +
    geom_point() + scale_x_log10() + facet_wrap( ~ continent)

# univariate plots
gm_2007 <- filter(gapminder, year==2007)
ggplot(gm_2007, aes(x=lifeExp)) + geom_histogram(binwidth=2)
ggplot(gm_2007, aes(x=lifeExp)) + geom_density()
ggplot(gm_2007, aes(x=lifeExp, fill=continent)) + 
    geom_density(alpha=0.5)
ggplot(gm_2007, aes(y=lifeExp, x=continent)) + geom_boxplot()

ggplot(gm_2007, aes(y=lifeExp, x=continent)) + 
    geom_point(position=position_jitter(width=0.1, height=0))

# theme bw
ggplot(gm_2007, aes(y=lifeExp, x=continent)) + 
    geom_point(position=position_jitter(width=0.1, height=0)) +
    theme_bw()

# Karl's theme
library(broman)
ggplot(gm_2007, aes(y=lifeExp, x=continent)) + 
    geom_point(position=position_jitter(width=0.1, height=0)) +
    theme_karl()

#saving a graph to a file
lifeExp2007 <- ggplot(gm_2007, aes(y=lifeExp, x=continent)) + 
    geom_point(position=position_jitter(width=0.1, height=0)) +
    theme_karl()
ggsave("lifeExp2017.png", lifeExp2007)


