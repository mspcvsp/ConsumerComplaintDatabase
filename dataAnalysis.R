# http://stackoverflow.com/questions/18931006/
#   how-to-suppress-warning-messages-when-loading-a-library
suppressWarnings(library(dplyr))

computeStateIssueCategoryPercentage <- function(complaintData) {
    issueCategoryPercentage <- data.frame()
    
    for (stateAbb in unique(complaintData$state)) {
        stateData <- complaintData[which(complaintData$state == stateAbb),]
        
        # http://stackoverflow.com/questions/1195826/
        #   dropping-factor-levels-in-a-subsetted-data-frame-in-r
        stateData$issuecategory <-
            as.factor(as.character(stateData$issuecategory))
        
        stateIssueCategoryPercentage <- table(stateData$issuecategory)
        stateIssueCategoryPercentage <- 100*stateIssueCategoryPercentage / 
            sum(stateIssueCategoryPercentage)
        
        for (issueCategory in names(stateIssueCategoryPercentage)) {
            currentRow <- 
                data.frame(state=stateAbb,
                           issuecateogry=issueCategory,
                           percentage=
                               stateIssueCategoryPercentage[issueCategory])
            
            rownames(currentRow) <- NULL
            
            issueCategoryPercentage <- rbind(issueCategoryPercentage,
                                             currentRow)
        }
    }
    
    return(issueCategoryPercentage)
}

initializePercentIssue <- function(issuecategory,
                                   stateIssueCategoryPercentage,
                                   state.regions) {
    issueCategoryRows <-
        which(stateIssueCategoryPercentage$issuecateogry == issuecategory)
    
    issueCategoryPercentage <-
        stateIssueCategoryPercentage[issueCategoryRows, c("state",
                                                          "percentage")]
    
    rownames(issueCategoryPercentage) <- NULL
    
    colnames(issueCategoryPercentage) <- c("abb", "value")
    issueCategoryPercentage$abb <- as.character(issueCategoryPercentage$abb)
    
    issueCategoryPercentage <- dplyr::left_join(issueCategoryPercentage,
                                                state.regions,
                                                by=c("abb"))
    
    issueCategoryPercentage <- issueCategoryPercentage[,c("region","value")]
    
    return(issueCategoryPercentage)
}


