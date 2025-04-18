% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fields_to_phenos.R
\name{fields_to_phenos}
\alias{fields_to_phenos}
\title{Check UK Biobank field IDs}
\usage{
fields_to_phenos(fields, filename = "", abort = TRUE, verbose = FALSE)
}
\arguments{
\item{fields}{A vector of character strings. The field IDs to check if valid.}

\item{filename}{A string. If provided, will save as a fieldname file ready for the table-exporter (including "eid").
\code{default=""}}

\item{abort}{Logical. Abort if a field is missing?,
\code{default=TRUE}}

\item{verbose}{Logical. Be verbose,
\code{default=FALSE}}
}
\value{
Returns a vector of strings (valid phenotypes).
}
\description{
Check if provided field IDs are valid and return all possible phenotype names in the UK Biobank RAP
}
\examples{

# not instanced, not arrayed
fields_to_phenos("31")  # sex

# instanced, not arrayed
fields_to_phenos("53")  # assessment date

# instanced and arrayed
fields_to_phenos("93")  # systolic blood pressure, manual reading

# instanced and arrayed, MRI assessments only
fields_to_phenos("12673") # Heart rate recorded during vicorder run (Heart MRI)

# check multiple simultaneously
fields_to_phenos(c("31","93"))

# only warn if an invalid field is provided (default is to abort)
fields_to_phenos(c("31","notafield","93"))
fields_to_phenos(c("31","notafield","93"), abort=FALSE)

# save as fieldname file for the table-exporter (don't forget to upload to the RAP)
fields_to_phenos(c("31","93"), filename="fieldnames.txt")

}
\author{
Luke Pilling
}
