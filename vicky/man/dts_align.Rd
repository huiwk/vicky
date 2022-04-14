\name{dts_align}

\alias{dts algin}

\title{To smoothen time series for drug data}

\usage{
dts_algin(
dat,
type,
refid,
startid,
endid
)
}

\argument{
  
\item{dat}{your drug data file, of \code{data.frame} structure.}

\item{type}{Element for by group smoothing, optional.}

\item{refid}{Your "Refkey" variable name, can use quotes or apostrophe to quote the name. If name is exactly "Reference Key", can ignore it, that is optional}

\item{startid}{Your "PrescriptionStartDate" variable name, can use quotes or apostrophe to quote the name. If name is exactly "Prescription Start Date", can ignore it, that is optional}

\item{endid}{Your "PrescriptionEndDate" variable name, can use quotes or apostrophe to quote the name. If name is exactly "Prescription End Date", can ignore it, that is optional}

}
\dontrun{
# style 1
# vicky::dts_algin(dat=Drug,type="Drug",refid="ReferenceKey",startid="Prescription Start Date",endid="Prescription End Date")
# style 2
# vicky::dts_algin(dat=Drug,type=`Drug`,refid=`ReferenceKey`,startid=`Prescription Start Date`,endid=`Prescription End Date`)
# style 3
# vicky::dts_algin(dat=Drug,type="Drug")
# style 4
# vicky::dts_algin(dat=Drug)
}
