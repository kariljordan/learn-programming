# Load the SAFE Data
# Studying African Farmer-Led Irrigation looks at farming and irrigation methods in Tanzania and Mozambique
# The full dataset is located here: https://figshare.com/articles/SAFI_Survey_Results/6262019
interviews <- read_csv("Data_Carpentry/my-forked-repos/learn-programming/data/SAFI_clean.csv", na = "NULL")

# To check to see if our data has loaded, uncomment the command below
# interviews or View(interviews)

# To get summary statistics for each column, use the command below
summary(interviews)

# To get the first element in the first column of the data frame as a vector
interviews[1,1]

# To get the first element in the 6th column as a vector
interviews[1,6]

# To get the first column of the dataframe as a vector
interviews[,1]

## create a vector from the data frame column "memb_assoc"
memb_assoc <- interviews$memb_assoc

## convert it into a factor

# By default, R always sorts levels in alphabetical order.
respondent_floor_type <- factor(c("earth", "cement", "cement", "earth"))

# To order the factors in a different way
respondent_floor_type <- factor(respondent_floor_type, levels = c("earth", "cement"))

## bar plot of the number of interview respondents who were
## members of irrigation association:
plot(memb_assoc)
