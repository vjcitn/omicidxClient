#' obtain sample attributes for a study.
#' @param srp character(1) SRP tag for a study in NCBI SRA
#' @param size numeric(1) maximum number of records to return
#' @param cursor character(1) the value of the cursor returned in a previous call; see note.
#' @param \dots not currently used
#' @note If the number of records available in query response exceeds the value of `size`, the
#' returned tibble will have an attribute 'cursor' which can be retrieved and used in
#' subsequent calls to this function.
#' @note If `size` is zero, a list is returned with information on the number of records available.
#' @examples
#' t1 = samp_atts_by_study(size=10)
#' t1
#' attr(t1, "cursor")
#' samp_atts_by_study(size=0)
#' @export
samp_atts_by_study = function (srp = "SRP082656", size = 500, cursor=NULL, ...)
{
    base_url = "https://api.omicidx.cancerdatasci.org/sra/studies/%s/runs?size=%s"
    if (!is.null(cursor)) base_url = paste0(base_url, "&cursor=", cursor)
    qstr = sprintf(base_url, srp, size)
    md = try(jsonlite::fromJSON(txt = qstr, flatten = TRUE))
    if (size==0) return(md)
    if (inherits(md, "try-error"))
        stop("could not parse JSON")
    metadata = as_tibble(md$hits)
    if (inherits(metadata, "try-error"))
        stop("check srp")
    sampatts = metadata %>% dplyr::select(accession, sample.attributes) %>%
        tidyr::unnest(sample.attributes) %>% dplyr::filter(tag !=
        "BioSampleModel") %>% tidyr::pivot_wider(names_from = "tag",
        values_from = "value")
    attr(sampatts, "cursor") = md$cursor
    sampatts
}
