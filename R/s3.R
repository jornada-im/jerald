#' Put entities in an S3 bucket
#'
#' This function takes a list of data entities and uploads them to an s3 
#' bucket
#'
#' @param ents A list of dataTable or otherEntity filenames
#' @param bucketname Name of the s3 bucket to upload to
#' @export
ents_to_s3 <- function(ents, bucket.name, multi.part=FALSE){
  for (fname in ents) {
    message("Pushing ", fname, " to ", bucket.name, " s3 bucket...")
    out <- aws.s3::put_object(fname, fname, bucket.name,
			      acl='public-read', multipart=multi.part,
			      verbose=FALSE, show_progress=TRUE)
    message(out) # Should print TRUE if successful
  }
message('Done.\n')
}

#' Remove entities from S3 bucket
#'
#' This function takes a list of data entities and deletes them from an s3 
#' bucket
#'
#' @param ents A list of dataTable or otherEntity filenames in an s3 bucket
#' @param bucketname Name of the s3 bucket to modify
#' @export
remove_ents_s3 <- function(ents, bucket.name){
  for (fname in ents) {
    message("Removing ", fname, " from ", bucket.name, "s3 bucket...")
    out <- aws.s3::delete_object(fname, bucket.name, verbose=TRUE,
				 show_progress=TRUE)
    message(out) # Should print TRUE if successful
  }
message('Done.\n')
}
