#' Put entities in an S3 bucket
#'
#' This function takes a list of data entities and uploads to an s3 bucket
#'
#' @param ents A list of filenames
#' @param bucketname Name of the s3 bucket to upload to
#' @export
ents_to_s3 <- function(ents, bucketname){
	for (fname in ents) {
		print(paste("Pushing", fname, "to", bucketname,
			    "s3 bucket..."))
		out <- aws.s3::put_object(fname, fname, bucketname,
					  acl='public-read', verbose=TRUE,
		show_progress=TRUE)
		print(out) # Should print TRUE if successful
	}
}

#' Remove entities from S3 bucket
#'
#' This function takes a list of data entities and uploads to an s3 bucket
#'
#' @param ents A list of filenames
#' @param bucketname Name of the s3 bucket to upload to
#' @export
remove_ents_s3 <- function(ents, bucketname){
	for (fname in ents) {
		print(paste("Removing", fname, "from", bucketname,
			    "s3 bucket..."))
		out <- aws.s3::delete_object(fname, bucketname, verbose=TRUE,
		show_progress=TRUE)
		print(out) # Should print TRUE if successful
	}
}
