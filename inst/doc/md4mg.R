## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(BiocStyle)

## ----ck1, cache=TRUE----------------------------------------------------------
library(tibble)
metadata=as_tibble(jsonlite::fromJSON(txt=
   url('https://api.omicidx.cancerdatasci.org/sra/studies/SRP114847/runs?size=500'),
   flatten=TRUE)$hits)
dim(metadata)

## ----lkf----------------------------------------------------------------------
colnames(metadata)

## ----chkl---------------------------------------------------------------------
length(unique(metadata$experiment.accession))

## ----lktv---------------------------------------------------------------------
metadata$sample.attributes[1:2]

## ----tickle-------------------------------------------------------------------
sampatts = metadata %>% dplyr::select(accession, sample.attributes) %>% 
    tidyr::unnest(sample.attributes) %>% 
    tidyr::pivot_wider(names_from='tag', values_from='value') 
DT::datatable(sampatts)

## ----lkjso--------------------------------------------------------------------
ref  = jsonlite::fromJSON("https://api.omicidx.cancerdatasci.org/openapi.json")
names(ref)

## ----lklk---------------------------------------------------------------------
names(ref$paths)

## ----dolistv------------------------------------------------------------------
ourendpt = "/sra/studies/{accession}/experiments"
as.data.frame(cbind(qualname=ref$paths[[ourendpt]]$get$parameters$name, 
  ref$paths[[ourendpt]]$get$parameters$schema))  # wanted datatable but threw warning

## ----domcq,cache=TRUE---------------------------------------------------------
mcq = jsonlite::fromJSON(
 "https://api.omicidx.cancerdatasci.org/sra/studies?q=microbiome%20AND%20cancer&size=15",
 flatten=TRUE)
as_tibble(mcq$hits)

## ----modit--------------------------------------------------------------------
ourendpt = "/sra/studies"
as.data.frame(cbind(qualname=ref$paths[[ourendpt]]$get$parameters$name, 
  ref$paths[[ourendpt]]$get$parameters$schema))  # wanted datatable but threw warning

## ----kjkj,cache=TRUE----------------------------------------------------------
pref = "https://api.omicidx.cancerdatasci.org/"
do_one = "sra/runs/SRR4052021"
lk1 = jsonlite::fromJSON(paste0(pref, do_one)) 
names(lk1)

## ----dosa,cache=TRUE----------------------------------------------------------
suppressPackageStartupMessages({
library(md4mg)
})
samp_atts_by_study
sa_asnicar = samp_atts_by_study()
dim(sa_asnicar)
DT::datatable(sa_asnicar)

## ----lkcotel------------------------------------------------------------------
library(httr)
base_url = function() "https://cmgd-telemetry-whnnxetv4q-uc.a.run.app"

nf_metadata_names = function(url=base_url(), limit=5, offset=0) {
  x = GET(sprintf(paste(url, "/nextflow/events?limit=%s&offset=%s", sep=""), limit, offset))
  sapply(content(x)$hits, function(z) z$metadata$parameters$metadata_tsv)
  }

nf_metadata_wf_cmdline = function(url=base_url(), limit=5, offset=0) {
  x = GET(sprintf(paste(url, "/nextflow/events?limit=%s&offset=%s", sep=""), limit, offset))
  sapply(content(x)$hits, function(z) z$metadata$workflow$commandLine)
  }

nf_trscript = function(url=base_url(), limit=5, offset=0) {
  x = GET(sprintf(paste(url, "/nextflow/events?limit=%s&offset=%s", sep=""), limit, offset))
  sapply(content(x)$hits, function(z) z$trace$script)
  }

nf_accessions = function(url=base_url(), limit=5, offset=0) {
  tmp = nf_trscript(url=url, limit=limit, offset=offset)
  gsub(".*accessions: (.*\\]).*", "\\1", tmp)
  }
nf_rowhash = function(url=base_url(), limit=5, offset=0) {
  tmp = nf_trscript(url=url, limit=limit, offset=offset)
  gsub(".*rowhash: (.*)....sampleinfo.txt.*", "\\1", tmp)
}

nf_metadata_names(limit=5)
nf_accessions(limit=5, offset=100)
nf_rowhash(limit=5, offset=100)

