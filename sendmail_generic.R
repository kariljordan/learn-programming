# Generic solution for sending personalized emails
# For more details see: https://github.com/jennybc/send-email-with-r

# Read in file with email addresses, names and any unique info that you want to include in your email
data = read.csv(file = "", stringsAsFactors = FALSE)

# # If your file has full names only and you want to use first names in email do:
# # Note: this only works if names are in the format "First Last"
# for(i in 1:nrow(data)) {
#   name = data[i, ""] #put the column name that has full names in the quotes
#   data$first_name[i] = strsplit(name, " ")[[1]][1]
# }

# # If names are in the format "Last, First" do:
# for(i in 1:nrow(data)) {
#   name = data[i, ""] #put the column name that has full names in the quotes
#   data$first_name[i] = strsplit(name, ", ")[[1]][2]
# }

#if gmailr is not already installed do:
#install.packages("gmailr") 
library(gmailr)

for(i in 1:nrow(data)) {
    name = paste0(data$first_name[i], ",")
    address = data$email[i] # replace "email" with the column name that has email addresses
    my_email = mime(
      To = address,
      From = "", # put your gmail address here
      Subject = "", # put a subject here
      body = paste("Dear", name, "" ) # put the body of your message in the quotes
      # Note: you can add another field to the body by putting a comma after the quotes
      )
    send_message(my_email)
}

