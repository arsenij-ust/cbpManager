% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{cBioPortalToDataFrame}
\alias{cBioPortalToDataFrame}
\title{Convert the cBioPortal sample- and patient-data file format into a data.frame}
\usage{
cBioPortalToDataFrame(data)
}
\arguments{
\item{data}{The data.frame of a cBioPortal sample/patient data file}
}
\value{
data.frame
}
\description{
This function takes a file object (from read.table), removes the # symbol,
sets the 5th row as the column names of the data.frame
and removes the rows containing the priority, data type and column name.
use read.table as follows: \code{read.table(file, sep='\t', colClasses = 'character', comment.char = '')}
}
\examples{
df <- data.frame(
        V1=c("#attr_1", "#attribute 1", "#STRING", "#1", "ATTRIBUTE_1", "value_1"), 
        V2=c("attr_2", "attribute 2", "STRING", "1", "ATTRIBUTE_2", "value_2")
    )
cbpManager:::cBioPortalToDataFrame(df)

}
