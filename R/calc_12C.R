##' Identify the 12C peak for a compound and calculate its percentage vs all ions
##' @param raw_m Raw data in melted form
##' @export
calc_12C <- function(raw_m) {
  
  p <- ggplot(mtcars, aes(x=mpg, y=cyl)) + geom_point()
  print(p)
  
  # Add a column for the sum of all ion counts for that compound
  ### Damn that's slow - shold rewrite in dplyr
  raw_ion_sum <- plyr::ddply(raw_m, c("sample", "compound", "treatment", "replicate", "sample.type", "time"), mutate, 
                       sum.ion.count=sum(ion.count, na.rm=TRUE), 
                       is.12C = medMz==min(medMz), 
                       relative.ion.count = ion.count/sum.ion.count)
 # message("* This algorithm currently identifies 12C peaks as the minimum medMz value for each unique combination of sample and compound. I want to confirm with the chemists that is correct.")
 # message("* This calculates relative ion count (i.e., %12C) as the ion count for the 12C divided by the ion count for all isotopomers IN A SINGLE SAMPLE. I'm not sure that is correct")
  raw_ion_sum
#   # CHeck to see if there's exactly one 12C identified 
#   c12_check <- ddply(raw_ion_sum, c("compoundId", "sample"), summarise,
#                      good_cpd=sum(is.12C==1))
#   sum(c12_check$good_cpd)==nrow(c12_check) #TRUE! Seems to be working
#   
#   # Figure out if I'm doing this right
#   single_mol <- subset(raw_ion_sum, sample=="X24_154" & compound=="ATP")
#   
#   isotopomers <- ddply(raw_m, c("compound", "sample"), summarise, 
#                        count=length(ion.count))
#   
#   subset(isotopomers, count==13) # just glutathione and UDP-D-glucuronate
#   subset(raw_m, compound=="glutathione" & sample=="X24_154")
#   
#   ggplot(single_mol, x=sample, y=relative.ion.count) + 
#     geom_rug()
  
}