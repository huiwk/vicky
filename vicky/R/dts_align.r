#' To smoothen time series for drug data
#'
#'
#' @param dat your drug data file, of \code{data.frame} structure.
#'
#' @param type Element for by group smoothing, optional.
#' @param refid Your "Refkey" variable name, can use quotes or apostrophe to quote the name. If name is exactly "Reference Key", can ignore it, that is optional
#' @param startid Your "PrescriptionStartDate" variable name, can use quotes or apostrophe to quote the name. If name is exactly "Prescription Start Date", can ignore it, that is optional
#' @param endid Your "PrescriptionEndDate" variable name, can use quotes or apostrophe to quote the name. If name is exactly "Prescription End Date", can ignore it, that is optional
#'
#' @examples \dontrun{
#' # style 1
#' # vicky::dts_algin(dat=Drug,type="Drug",refid="ReferenceKey",startid="Prescription Start Date",endid="Prescription End Date")
#' # style 2
#' # vicky::dts_algin(dat=Drug,type=`Drug`,refid=`ReferenceKey`,startid=`Prescription Start Date`,endid=`Prescription End Date`)
#' # style 3
#' # vicky::dts_algin(dat=Drug,type="Drug")
#' # style 4
#' # vicky::dts_algin(dat=Drug)}
#' @export

dts_align<-function (dat, type, refid = `Reference Key`, startid = `Prescription Start Date`, 
    endid = `Prescription End Date`) 
{
    type <- if (hasArg(type)) 
        ensym(type)
    refid <- ensym(refid)
    startid <- ensym(startid)
    endid <- ensym(endid)
    refidname <- expr(!!refid) %>% as.character
    startidname <- expr(!!startid) %>% as.character
    endidname <- expr(!!endid) %>% as.character
    dat %>% distinct(.keep_all = T) %>% arrange(across(contains(refidname) | 
        contains(startidname) | contains(endidname))) %>% {
        if (!is.null(type)) 
            group_by(., !!refid, !!type)
        else group_by(., !!refid)
    } %>% mutate(`Last End Date` = lag(!!endid) %>% as.Date(), 
        `Min Start Date` = min(!!startid) %>% as.Date(), `Accumulated Last End Date` = cummax((`Last End Date` %>% 
            as.numeric %>% ifelse(is.na(.), -999, .))) %>% ifelse(. == 
            -999, min(!!endid), .) %>% as.Date(), `Aligned Prescription Start Date` = ifelse((startid < 
            `Accumulated Last End Date`) & (startid > `Min Start Date`), 
            `Accumulated Last End Date`, !!startid) %>% as.Date(), 
        `Drug Duration` = time_length(interval(`Aligned Prescription Start Date`, 
            !!endid), "day"), `Accumulated Drug Duration` = cumsum(ifelse(`Drug Duration` < 
            0, 0, `Drug Duration`)))
}
