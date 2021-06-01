# `jerald_destination_keys.R` file. Store credentials for destination
# repository and s3 buckets here
#
#
# EDI (portal.edirepository.org) credentials
# Use the returned edicred list in EDIutils function arguments and similar
edicred <- list(
  user.id = "username",    # Add EDI username (leave quotes)
  user.pass = "password",  # Add EDI password (leave quotes)
  affiliation = "EDI")     # Add affiliation - usually 'EDI'


# This assigns the API keys for an s3 bucket into environmental variables so 
# that the aws.s3 R package can access them.
# Don't change these without consulting your S3 administrator.
#
# All variable names and values here should be quoted
Sys.setenv("AWS_S3_ENDPOINT" = "s3 endpointaddress", # Add host address
           "AWS_S3_BUCKETNAME" = "s3 bucketname",    # Add bucket name
           "AWS_ACCESS_KEY_ID" = "s3 accesskeystring", # Access key
           "AWS_SECRET_ACCESS_KEY" = "s3 secretkeystring", # Secret acces key
           "AWS_DEFAULT_REGION" = "region (if used)", # Region, eg us-west3
           "AWS_SESSION_TOKEN" = "") # Usually ok to leave blank
