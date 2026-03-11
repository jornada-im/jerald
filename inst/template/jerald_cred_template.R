# This creates a list with user credentials to access the JRN Metabase
# The credentials list can be used by MetaEgress (via jerald). 
# 
# This is only a template - make a copy of this file with your name on it,
# insert your own username and password, and keep the file in either the 
# same directory you found the template, or in a safe local location.
# current template at https://github.com/jornada-im/jerald/blob/main/inst/template/jerald_cred.R


mbcred <- list(
  dbname = "my_metabase",   # Name of metabase (in quotes)
  host = "nnn.nn.nnn.nnn",  # Metabase host address (ip in quotes)
  port = 5432,              # Port number (PostgreSQL default is 5432)
  user = "myuname",         # Metabase username (in quotes)
  password = "12345abcde")  # Metabase password (in quotes)

# EDI (portal.edirepository.org) credentials
# Use the returned edicred list in EDIutils function arguments and similar
edicred <- list(
  user.id = "JRN",          # EDI username (in quotes)
  user.pass = "12345abcde", # EDI password (in quotes)
  affiliation = "EDI")      # EDI affiliation - usually 'EDI'

# This assigns the API keys for an s3 bucket into environmental variables so 
# that the aws.s3 R package can access them.
# Don't change these without consulting your S3 administrator.
#
# All variable names and values here should be quoted
Sys.setenv("AWS_S3_ENDPOINT" = "s3provider.com",         # S3 host address
           "AWS_S3_BUCKETNAME" = "mybucketname/subpath", # S3 bucket name, optional subpath
           "AWS_ACCESS_KEY_ID" = "abcde12345",           # S3 access key
           "AWS_SECRET_ACCESS_KEY" = "abcde12345",       # S3 secret acces key
           "AWS_DEFAULT_REGION" = "us-west3",            # Provider region/datacenter, eg us-west3
           "AWS_SESSION_TOKEN" = "")                     # Usually ok to leave blank