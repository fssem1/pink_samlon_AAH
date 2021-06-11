#standard model function (includes reference to year); 3 or 4 day fishery
standard_3_day_fishery <-function(statweek, y){ #create release table through saturday/sunday of sw; #create recovery table through wednesday of sw
  read.csv(paste0('./retrospective_analysis/data/processed/', 'merge_releases_recoveries.csv')) -> releases_recoveries
  releases_recoveries %>% filter(year == y & sw_release <= statweek) %>% 
    mutate (include = ifelse(sw_release < statweek,1,ifelse(sw_release == statweek & day_release == 'Sunday', 1,0))) %>%
    dcast(sw_release~year, fun.aggregate=sum, value.var='include', na.rm=TRUE) -> release_table
  write.csv(release_table, paste0('./retrospective_analysis/output/final/',y, '/', 'sw',statweek , '_','releases_standard.csv'))
  releases_recoveries %>% filter(year == y & sw_release <= statweek & sw_recap <= statweek)  %>%
    mutate (include = ifelse(sw_release == statweek & sw_recap == statweek & day_release == 'Sunday' & day_recap == 'Tuesday', 1,
                             ifelse(sw_release < statweek & sw_recap < statweek, 1,
                                    ifelse(sw_release < statweek & sw_recap == statweek & day_recap %in% c('Sunday','Monday','Tuesday'), 1, 0)))) %>%
   dcast(sw_release~sw_recap, fun.aggregate=sum, value.var='include', na.rm=TRUE) -> recap_table      
   write.csv(recap_table, paste0('./retrospective_analysis/output/final/',y, '/', 'sw',statweek , '_','recaps_standard.csv'))}

#standard model function (includes reference to year); 2 day fishery
standard_2_day_fishery <-function(statweek, y){ 
    read.csv(paste0('./retrospective_analysis/data/processed/', 'merge_releases_recoveries.csv')) -> releases_recoveries
    releases_recoveries %>% filter(year == y & sw_release < statweek) %>% 
      mutate (include = ifelse(sw_release < statweek,1,0)) %>%
      dcast(sw_release~year, fun.aggregate=sum, value.var='include', na.rm=TRUE) -> release_table
    write.csv(release_table, paste0('./retrospective_analysis/output/final/',y, '/', 'sw',statweek , '_','releases_standard.csv'))
     releases_recoveries %>% filter(year == y & sw_release < statweek & sw_recap <=statweek) %>%
      mutate (include =  ifelse(sw_release == statweek-1 & sw_recap == statweek & day_release == 'Saturday' & day_recap  %in% c('Monday', 'Tuesday'),1,
                    ifelse(sw_release == statweek-1 & sw_recap == statweek & day_release != 'Saturday'& day_recap %in% c('Sunday', 'Monday','Tuesday'), 1,
                                  ifelse(sw_release == statweek-1 & sw_recap == statweek-1 & day_release != 'Saturday',1,  
                                         ifelse(sw_release < statweek-1 & sw_recap == statweek-1, 1,
                                                ifelse(sw_release < statweek-1 & sw_recap < statweek-1, 1,   
                                                ifelse(sw_release < statweek-1 & sw_recap == statweek & day_recap %in% c('Sunday', 'Monday' , 'Tuesday'), 1,0))))))) %>%
      dcast(sw_release~sw_recap, fun.aggregate=sum, value.var='include', na.rm=TRUE) -> recap_table
      write.csv(recap_table, paste0('./retrospective_analysis/output/final/',y, '/', 'sw',statweek , '_','recaps_standard.csv'))}

#full model function (includes reference to year) 
full_model <-function(statweek,y){ 
  read.csv(paste0('./retrospective_analysis/data/processed/', 'merge_releases_recoveries.csv')) -> releases_recoveries
  releases_recoveries %>% filter(year == y & sw_release <= statweek) %>% 
    mutate (include = ifelse(sw_release < statweek,1,ifelse(sw_release == statweek & day_release %in% c('Sunday', 'Monday', 'Tuesday'), 1,0))) %>%   
    dcast(sw_release~year, fun.aggregate=sum, value.var='include', na.rm=TRUE) -> release_table
  write.csv(release_table, paste0('./retrospective_analysis/output/final/',y, '/', 'sw',statweek , '_','releases_full.csv'))
  releases_recoveries %>% filter(year == y & sw_release <= statweek & sw_recap <=statweek) %>% 
    mutate (include = ifelse(sw_recap < statweek,1,ifelse(sw_recap == statweek & day_recap %in% c('Sunday', 'Monday', 'Tuesday'), 1,0))) %>%  
    dcast(sw_release~sw_recap, fun.aggregate=sum, value.var='include', na.rm=TRUE) -> recap_table
  write.csv(recap_table, paste0('./retrospective_analysis/output/final/',y, '/', 'sw',statweek , '_','recaps_poisson.csv'))}

#sulk_rate_model function (includes year reference)
sulk_rate_model <-function(statweek,y){ 
  read.csv(paste0('./retrospective_analysis/data/processed/', 'merge_releases_recoveries.csv')) -> releases_recoveries
  releases_recoveries %>% filter(year == y & sw_release <= statweek & sw_recap <=statweek) %>% 
    mutate (include = ifelse(sw_recap < statweek,1,ifelse(sw_recap == statweek & day_recap %in% c('Sunday', 'Monday', 'Tuesday'), 1,0))) %>% 
    filter(include!= "0") %>%
    dcast(sw_release~include, fun.aggregate=mean, value.var='sulk', na.rm=TRUE) %>%
    dplyr::select(sw_release = sw_release,sw_sulk_rate = '1') -> sulk_rate
  releases_recoveries %>% filter(year == y & sw_release <= statweek & sw_recap <=statweek) %>% 
    mutate (include = ifelse(sw_recap < statweek,1,ifelse(sw_recap == statweek & day_recap %in% c('Sunday', 'Monday', 'Tuesday'), 1,0))) %>% 
    filter(include!= "0") %>%
    mutate(include ="total") %>% 
    dcast(include~include, fun.aggregate=mean, value.var='sulk', na.rm=TRUE) %>% 
    dplyr::select(total = include,avg_sulk_rate = total) -> sulk_rate_avg
  sulk_rate <- cbind(sulk_rate_avg, sulk_rate)
  sulk_rate %>% dplyr::select(sw_release,sw_sulk_rate,avg_sulk_rate) -> sulk_rate_table
  read.csv(paste0('./retrospective_analysis/output/final/',y, '/', 'sw',statweek , '_','releases_full.csv')) -> releases_full
  names(releases_full) <- c('NaN', 'sw_release', 'full_releases')
  releases_full %>% dplyr::select(sw_release,full_releases) -> releases_full
  poisson<- merge(releases_full, sulk_rate_table, by=c("sw_release"), all.x=TRUE) 
  poisson[is.na(poisson)] <- 0
  poisson %>% mutate (mean = ifelse(sw_sulk_rate > 0, sw_sulk_rate, avg_sulk_rate),x = (statweek - sw_release) * 7) %>% 
    mutate (poisson_dist = ppois(x,lambda=mean)) %>%
    mutate (total_poisson = round (poisson_dist * full_releases, 0)) %>% 
    dplyr::select(sw_release,total_poisson) -> releases_poisson
  write.csv(releases_poisson, paste0('./retrospective_analysis/output/final/',y, '/', 'sw',statweek , '_','releases_poisson.csv'))} #create sulk_model releases



trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#figure layouts
vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)

arrange_ggplot2 <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
  dots <- list(...)  
  n <- length(dots)	
  if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}	
  if(is.null(nrow)) { nrow = ceiling(n/ncol)}	
  if(is.null(ncol)) { ncol = ceiling(n/nrow)}        
  #grid.Newpage()
  pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )  
  ii.p <- 1	
  for(ii.row in seq(1, nrow)){	
    ii.table.row <- ii.row		
    if(as.table) {ii.table.row <- nrow - ii.table.row + 1}		
    for(ii.col in seq(1, ncol)){			
      ii.table <- ii.p			
      if(ii.p > n) break			
      print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))			
      ii.p <- ii.p + 1}}}

#function to extract data from ADMB report
reptoRlist = function(fn)
{
  ifile=scan(fn,what="character",flush=T,blank.lines.skip=F,quiet=T)
  idx=sapply(as.double(ifile),is.na)
  vnam=ifile[idx]  #list names
  nv=length(vnam)           #number of objects
  A=list()
  ir=0
  for(i in 1:nv)
  {
    ir=match(vnam[i],ifile)
    if(i!=nv) irr=match(vnam[i+1],ifile) else irr=length(ifile)+1 #next row
    dum=NA
    if(irr-ir==2) dum=as.double(scan(fn,skip=ir,nlines=1,quiet=T,what=""))
    if(irr-ir>2) dum=as.matrix(read.table(fn,skip=ir,nrow=irr-ir-1,fill=T))
    
    if(is.numeric(dum))#Logical test to ensure dealing with numbers
    {
      A[[ vnam[i ] ]]=dum
    }
  }
  return(A)
  A = reptoRlist(fn='code/admb/model.rep') 
}

#formatting axis function
fmt <- function(){
  function(x) format(x,nsmall = 2,scientific = FALSE)}
round2<-function(x){trunc(x+0.5)} 

#labelling axes
every_nth <- function(x, nth, empty = TRUE, inverse = FALSE)
{
  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}
#to get rid of scientific notation
value_formatter <- function (val) {
  format(val, scientific =FALSE)
}
#test for normality
eda.norm <- function(x, ...)
{
  par(mfrow=c(2,2))
  if(sum(is.na(x)) > 0)
    warning("NA's were removed before plotting")
  x <- x[!is.na(x)]
  hist(x, main = "Histogram and non-\nparametric density estimate", prob = T)
  iqd <- summary(x)[5] - summary(x)[2]
  lines(density(x, width = 2 * iqd))
  boxplot(x, main = "Boxplot", ...)
  qqnorm(x)
  qqline(x)
  plot.ecdf(x, main="Empirical and normal cdf")
  LIM <- par("usr")
  y <- seq(LIM[1],LIM[2],length=100)
  lines(y, pnorm(y, mean(x), sqrt(var(x))))
  shapiro.test(x)
}





