suppressMessages({
        require(dplyr)  
        require(lubridate)
        require(ggplot2)
})

# Reads in the file in the desired format
readData <- function(filename, trip.no=1) {
        read.table(file=filename, 
                   col.name=c("date","modulation","bitrate","preamble","encoding","packet"),
                   colClasses = c("character","factor","numeric","character","factor","character"),
                   skip=3) %>% mutate(trip=trip.no)
}

analyzeOdyssey <- function(returnAll=F) {
        # Read in the data from odyssey*.txt files in the current dir
        files <- list.files(
                getwd(), pattern="odyssey(.*).txt", full.names=T, ignore.case=T) 
        odyssey <- do.call(rbind,lapply(seq_along(files), function(x) readData(files[x],x))) %>%
                analyzeTPMS(evalq(if(!returnAll)"110110101110001")) %>%
                # The tire IDs are bits 25-56 of the packet data for Odyssey (we think)
                group_by_(if(returnAll) "preamble" else "tireID=substr(packet,25,56)")
        odyssey
}

analyzeAcura <- function() {
        acura <- analyzeTPMS(readData("acura.txt"), "01010101010101010101010101010110")
        acura
}

analyzePacket <- function(packets, startBit, stopBit) {
        sapply(packets, substr, startBit, stopBit, USE.NAMES=F)
}

analyzeTPMS <- function(tpms, carID=NULL) {
        
        tpms <- # Filter only the relevant data if necessary
                if(!is.null(carID)) filter(tpms, preamble==carID) else tpms %>%
                # Convert date
                mutate(date=ymd_hms(date)) %>%
                # Group by trip
                group_by(trip)
 
        # Return the dataframe
        tpms
}

plotCountBy <- function(data, x, facet=trip) {
       eval(substitute(ggplot(data, aes(x)))) + stat_count() + 
                eval(substitute(facet_grid(facet~.))) +
                theme(axis.text.x=element_text(angle=90, hjust=1))
}
