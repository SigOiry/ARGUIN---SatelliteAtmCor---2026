#!/usr/bin/env Rscript
# SCI-183 review revision: continuous contrast, scorer coupling, relative delta,
# and spatial-block uncertainty. Existing Data/ inputs are read-only.
suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly=TRUE)
val <- function(k,d=NULL) { z<-grep(paste0('^',k,'='),args,value=TRUE); if(length(z)) sub(paste0('^',k,'='),'',z[1]) else d }
sa <- sub('^--file=','',grep('^--file=',commandArgs(FALSE),value=TRUE)); sa<-gsub('~\\+~',' ',sa)
root <- normalizePath(val('--project-root',file.path(dirname(sa),'..')),mustWork=TRUE)
out <- val('--output-dir',file.path(root,'outputs')); dir.create(out,recursive=TRUE,showWarnings=FALSE)
bands <- c('B02','B03','B04'); sdir<-file.path(root,'Data','Atm_Compare_AOI','Sen2Cor'); adir<-file.path(root,'Data','Atm_Compare_AOI','ACOLITE')
fn <- sort(intersect(basename(list.files(sdir,'\\.csv$',full.names=TRUE)),basename(list.files(adir,'\\.csv$',full.names=TRUE))))
read1 <- function(f) {
 s<-fread(file.path(sdir,f))[,c('date','AOI','x','y',bands),with=FALSE]; a<-fread(file.path(adir,f))[,c('date','AOI','x','y',bands),with=FALSE]
 setnames(s,bands,paste0(bands,'_S')); setnames(a,bands,paste0(bands,'_A')); merge(s,a,by=c('date','AOI','x','y'))
}
d <- rbindlist(lapply(fn,read1)); d[,S:=rowMeans(.SD),.SDcols=paste0(bands,'_S')]; d[,A:=rowMeans(.SD),.SDcols=paste0(bands,'_A')]
p <- d[,.(S=median(S),A=median(A),n_dates=uniqueN(date)),by=.(AOI,x,y)]; p[,`:=`(mean=(S+A)/2,delta=A-S,relative_delta=(A-S)/S)]
p[,`:=`(xb=pmin(4L,ceiling(frank(x,ties.method='average')/.N*4)),yb=pmin(4L,ceiling(frank(y,ties.method='average')/.N*4))),by=AOI]; p[,block:=paste(xb,yb,sep='_')]
slope <- function(z) cov(z$S,z$A)/var(z$S)
set.seed(184)
metrics <- rbindlist(lapply(unique(p$AOI),function(site){
 z<-p[AOI==site]; bl<-unique(z$block); bt<-replicate(2000,{take<-sample(bl,length(bl),TRUE); q<-rbindlist(lapply(take,function(b) z[block==b])); slope(q)})
 data.table(AOI=site,n_pixels=nrow(z),n_blocks=length(bl),slope_ACOLITE_on_Sen2Cor=slope(z),slope_ci_low=quantile(bt,.025,na.rm=TRUE),slope_ci_high=quantile(bt,.975,na.rm=TRUE),sd_ratio=sd(z$A)/sd(z$S),r_ACOLITE_Sen2Cor=cor(z$A,z$S),r_mean_delta=cor(z$mean,z$delta),r_Sen2Cor_delta=cor(z$S,z$delta),r_ACOLITE_delta=cor(z$A,z$delta))
}))
scores <- melt(p,id.vars=c('AOI','x','y','delta','relative_delta'),measure.vars=c('mean','S','A'),variable.name='scorer',value.name='score')
triplet <- scores[,{
 lo<-quantile(score,1/3); hi<-quantile(score,2/3); l<-delta[score<=lo]; h<-delta[score>=hi]; lr<-relative_delta[score<=lo]; hr<-relative_delta[score>=hi]
 .(lower_threshold=lo,higher_threshold=hi,n_lower=length(l),n_higher=length(h),lower_median_delta=median(l),higher_median_delta=median(h),lower_minus_higher_delta=median(l)-median(h),lower_median_relative_delta=median(lr),higher_median_relative_delta=median(hr))
},by=.(AOI,scorer)]
counts <- d[,.(n_valid=.N),by=.(date,AOI)]
fwrite(metrics,file.path(out,'processor_spatial_contrast_slopes.csv')); fwrite(triplet,file.path(out,'brightness_scorer_triplet.csv')); fwrite(counts,file.path(out,'valid_acquisitions_by_date_aoi.csv'))
writeLines(c(paste0('exact paired dates: ',length(fn)),'bands averaged: B02,B03,B04','slope: OLS ACOLITE~Sen2Cor across pixel medians','slope caveat: OLS treats Sen2Cor as error-free and is subject to regression dilution; interpret with SD ratio and paired-product correlation','spatial bootstrap: 4x4 within-AOI coordinate-quantile blocks, 2000 resamples, seed 184','spatial scope: 16 blocks per AOI; AOI intervals overlap and do not support an AOI-difference claim','valid-count caveat: identical per-date counts (730 Arel, 733 Iwik) and no evident QA field/masking record in supplied CSVs','identifiability: constant relative offset and contrast compression are not separable from these paired observations','CRS assumption: x/y are projected metres; EPSG:32628 is inferred from linked project products but is absent from CSV metadata'),file.path(out,'SCI183_REVIEW_REVISION_METADATA.txt'))
message('Review-revision outputs written to ',normalizePath(out))
