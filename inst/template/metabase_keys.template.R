# This creates a list with user credentials to access a metabase
# The credentials list can be used by MetaEgress (via jerald). 
# 
# **This is only a template - make a copy of this file with your name on it,
# insert your own username and password, and keep the file in either the 
# same directory you found the template, or in a safe local location.**
mbcred <- list(
  host = "hostaddress",      # Add metabase host address (leave quotes)
  port = 5432,
  user = "yourusername",     # Add your Metabase username (leave quotes)
  password = "yourpassword") # Add your Metabase password (leave quotes)

mbname <- "yourmetabasename" # Add the name of your Metabase (leave quotes)

