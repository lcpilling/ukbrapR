% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/label_ukb_field.R
\name{label_ukb_fields}
\alias{label_ukb_fields}
\title{Update a data frame containing UK Biobank fields with `title` and `label` from the schema}
\usage{
label_ukb_fields(d, verbose = FALSE)
}
\arguments{
\item{d}{A data frame. The data frame containing UK Biobank fields to update.}

\item{verbose}{Logical. Be verbose,
\code{default=FALSE}}
}
\value{
Returns a data frame.
}
\description{
Variables such as education and ethnicity are provided as integers but have specific codes. 

The UK Biobank schema are machine-readable dictionaries and mappings defining the internal structure of the online Showcase. https://biobank.ctsu.ox.ac.uk/crystal/schema.cgi

This function updates a data frame of UK Biobank data field with information from the Schema. 

It is in effect a wrapper to apply ukbrapR::label_ukb_field() to each variable in a data frame that looks like a UK Biobank field.

Only recognised fields are modified (variables named things like "p54_i0"). Other variables are ignored.
}
\examples{

# say the below data frame contains 4 variables: `eid`, `p54_i0`, `p50_i0` and `age_at_assessment` 
names(ukb)

# update the variables that looks like UK Biobank fields with titles and, where cateogrical, labels 
# i.e., `p54_i0` and `p50_i0` only -- `eid` and `age_at_assessment` are ignored
ukb <- ukbrapR::label_ukb_fields(ukb)

table(ukb$p54_i0)                   # tabulates the values
table(haven::as_factor(ukb$p54_i0)) # tabulates the labels
haven::print_labels(ukb$p54_i0)     # show the value:label mapping for this variable

}
\author{
Luke Pilling
}
