source("/home/taha/chepec/chetex/common/R/common/ProvideSampleId.R")

##################################################
#################### ocp2df ######################
##################################################
ocp2df <- function(datafilename) {
   ## Description:
   ##   Reads voltage-time data (from AUTOLAB potentiostat)
   ##   and returns a dataframe with the data.
   ## Usage:
   ##   ocp2df(datafilename)
   ## Arguments:
   ##   datafilename: text string with full path to experimental file
   ## Value:
   ##   Dataframe with the following columns:
   ##   $ sampleid        : chr
   ##   $ time            : num
   ##   $ potential       : num
   #
   datafile <- file(datafilename, "r")
   chifile <- readLines(datafile, n = -1) #read all lines of input file
   close(datafile)
   #
   substrateid <- 
      ProvideSampleId(datafilename, implementation="dirname")
   sampleid <- 
      paste(ProvideSampleId(datafilename, implementation="dirname"),
            ProvideSampleId(datafilename, implementation="filename"),
            sep = "-")
   #
   rgxp.number <- "^\\d+\\.\\d+"
   # regexp that matches a decimal number at the beginning of the line.
   # Matches numbers _without_ a negative sign (hyphen), 
   # followed by one or more digits before the decimal, a decimal point,
   # and an one or more digits after the decimal point.
   # Note that backslashes are escaped.
   #
   numrow.idx <- regexpr(rgxp.number, chifile)
   # Save the match length attribute to another variable,
   numrow.len <- attr(numrow.idx, "match.length")
   # then scrap the attribute of the original variable.
   attr(numrow.idx, "match.length") <- NULL
   #
   i <- seq(1, length(numrow.idx) - 1, 1)
   j <- seq(2, length(numrow.idx), 1)
   # Start indices of data ranges
   starts <- which(numrow.idx[i] != 1 & numrow.idx[j] == 1) + 1
   # End indices, except for the last
   ends <- which(numrow.idx[i] == 1 & numrow.idx[j] != 1)
   # Fix the last index of end indices
   ends <- c(ends, length(numrow.idx))
   #
   
   # complicating issue: sometimes GPES software produces *.txt 
   # with 2 cols (time, pot), 
   # and sometimes with 3 cols (time, pot, charge)...
   # take the first row containing numbers, and check how many "\t" it 
   # contains, this number is the same as the number of columns
   number.of.columns <- length(strsplit(chifile[starts[1]], "\t")[[1]])
   
   ff <- data.frame(NULL)
   for (s in 1:length(starts)) {
      zz <- textConnection(chifile[starts[s]:ends[s]], "r")
      ff <- rbind(ff,
               data.frame(substrateid,
                          sampleid,
                          stringsAsFactors = FALSE,                  
                          matrix(scan(zz, 
                                      what = numeric(), 
                                      sep = ""),
                                 ncol = number.of.columns, 
                                 byrow = T)))
      close(zz)
   }
   
   # assign column names (depends on number of columns)
   if (number.of.columns == 2) {
      names(ff) <- 
         c("substrateid", 
           "sampleid",
           "time", 
           "potential")
   }
   if (number.of.columns == 3) {
      names(ff) <- 
         c("substrateid", 
           "sampleid",
           "time", 
           "potential",
           "charge")
   }
   #
   return(ff)
}
