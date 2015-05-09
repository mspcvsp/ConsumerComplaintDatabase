library(choroplethr)
library(choroplethrMaps)
library(data.table)

data(state.regions)

initializeSummaryIssueCategoryPercentage <- function(complaintData) {
    summaryTableData <- table(complaintData$issuecategory)
    
    summaryTableData <- round(data.frame(sort(100 * summaryTableData / 
                                              sum(summaryTableData),
                                              decreasing=TRUE)),1)
    
    summaryTableData$issuecategory <- rownames(summaryTableData)
    
    rownames(summaryTableData) <- NULL
    
    colnames(summaryTableData) <- c("percentoccurence", "issuecategory")
    
    summaryTableData <- summaryTableData[,c("issuecategory",
                                            "percentoccurence")]
    
    return(summaryTableData)
}

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

computeStateSubmittedViaPercentage <- function(complaintData) {
    submittedViaPercentage <- data.frame()
    
    for (stateAbb in unique(complaintData$state)) {
        stateData <- complaintData[which(complaintData$state == stateAbb),]
        
        # http://stackoverflow.com/questions/1195826/
        #   dropping-factor-levels-in-a-subsetted-data-frame-in-r
        stateData$submittedvia <-
            as.factor(as.character(stateData$submittedvia))

        stateSubmittedViaPercentage <- table(stateData$submittedvia)

        stateSubmittedViaPercentage <- 100*stateSubmittedViaPercentage / 
                                       sum(stateSubmittedViaPercentage)
        
        for (submittedvia in names(stateSubmittedViaPercentage)) {
            currentRow <- 
                data.frame(state=stateAbb,
                           submittedvia=submittedvia,
                           percentage=
                               stateSubmittedViaPercentage[submittedvia])
            
            rownames(currentRow) <- NULL
            
            submittedViaPercentage <- rbind(submittedViaPercentage,
                                            currentRow)
        }
    }
    
    return(submittedViaPercentage)
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

initializeSummaryPercentSubmittedVia <- function(complaintData) {
    tableData <- sort(table(complaintData$submittedvia),
                            decreasing =TRUE)
    
    tableData <- round(100 * tableData / sum(tableData),1)
    tableData <- as.data.frame(tableData)

    colnames(tableData) <- c("percent")
    tableData <- t(tableData)
    
    return(tableData)
}

initializePercentSubmittedVia <- function(submittedvia,
                                          stateSubmittedViaPercentage,
                                          state.regions) {
    submittedViaRows <-
        which(stateSubmittedViaPercentage$submittedvia == submittedvia)
    
    submittedViaPercentage <-
        stateSubmittedViaPercentage[submittedViaRows, c("state",
                                                        "percentage")]
    
    rownames(submittedViaPercentage) <- NULL
    
    colnames(submittedViaPercentage) <- c("abb", "value")
    submittedViaPercentage$abb <- as.character(submittedViaPercentage$abb)
    
    submittedViaPercentage <- dplyr::left_join(submittedViaPercentage,
                                               state.regions,
                                               by=c("abb"))
    
    submittedViaPercentage <- submittedViaPercentage[,c("region","value")]
    
    return(submittedViaPercentage)
}

initializeZipCodeIssueCategoryPercentage <- function(complaintData,
                                                     stateList) {
    categoryPercentage <- data.frame()

    for (state in stateList) {
        stateData <- complaintData[which(complaintData$state == state),]
        
        zipCodes <- unique(stateData$zipcode)
        numberZipCodes <- length(zipCodes)

        for (index in seq_len(numberZipCodes)) {
            zipCodeData <- complaintData[which(complaintData$zipcode == 
                                               zipCodes[index]),]
            
            zipcodePercentage <- table(zipCodeData$issuecategory)
            zipcodePercentage <- 100 * (zipcodePercentage / 
                                        sum(zipcodePercentage))
            
            baseRow <- zipCodeData[1,c("city",
                                       "state",
                                       "latitude",
                                       "longitude",
                                       "zipcode")]
            baseRow$percentage <- NA
            baseRow$issuecateogry <- NA
            
            for (issueCategory in names(zipcodePercentage)) {
                if (zipcodePercentage[issueCategory] > 0) {
                    currentRow <-baseRow
                    currentRow$issuecateogry <- issueCategory
                    currentRow$percentage <- zipcodePercentage[issueCategory] 
                    
                    categoryPercentage <- rbind(categoryPercentage,
                                                currentRow)
                }
            }
        }
    }
    
    return(categoryPercentage)
}

estimateTopFiveStateIssueCategories <- function(complaintData,
                                                stateAbbreviation) {
    stateIssueCategoryPercentage <- 
        initializeZipCodeIssueCategoryPercentage(complaintData,
                                                 c(stateAbbreviation))
    colnames(stateIssueCategoryPercentage) <-
        gsub("latitude","lat",colnames(stateIssueCategoryPercentage))
    
    colnames(stateIssueCategoryPercentage) <-
        gsub("longitude","lon",colnames(stateIssueCategoryPercentage))
    
    stateIssueCategoryPercentage$percentageRange <-
        cut(stateIssueCategoryPercentage$percentage,
            seq(0,100,by=12.5))
    
    stateIssueCategoryPercentage$fraction <-
        stateIssueCategoryPercentage$percentage / 100.0
    
    # http://rprogramming.net/aggregate-data-in-r-using-data-table/
    stateIssueCategoryPercentage <- data.table(stateIssueCategoryPercentage)
    setkey(stateIssueCategoryPercentage, issuecateogry)
    
    # http://www.statmethods.net/management/sorting.html
    weightedSum <- stateIssueCategoryPercentage[,sum(fraction),
                                                by=issuecateogry]
    weightedSum <- weightedSum[order(-V1),]
    weightedSum <- data.frame(weightedSum)
    colnames(weightedSum) <- c("issuecategory","weightedsum")
    
    topFiveCategories <- weightedSum[1:5,c("issuecategory")]
    
    stateIssueCategoryPercentage <- data.frame(stateIssueCategoryPercentage)
    
    isTopFive <- stateIssueCategoryPercentage$issuecateogry %in% 
                 topFiveCategories
    
    stateIssueCategoryPercentage <- stateIssueCategoryPercentage[isTopFive,]
    
    return(stateIssueCategoryPercentage)
}
