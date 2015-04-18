loadComplainData <- function(csvFilePath) {

    complaintData <- read.csv("./Data/rows.csv",
                              header=TRUE,
                              stringsAsFactors=FALSE)
    
    # Transform the variable names to lower case
    colnames(complaintData) <- tolower(colnames(complaintData))

    # Remove '.' from variable names
    colnames(complaintData) <- gsub('\\.','',colnames(complaintData))
    
    # Remove partial observations
    percentMissingData <- computePercentMissingData(complaintData)
    
    dataFrameSegmentation <-
        initializeVariablesToAnalyze(maximumPercentMissing,
                                     percentMissingData,
                                     complaintData)

    complaintData <- complaintData[,dataFrameSegmentation$variables]
    complaintData <- complaintData[dataFrameSegmentation$isValidRow,]

    for (variable in colnames(complaintData)) {

        if (class(complaintData[1,variable]) == "character") {
            if (!grepl("date", variable)) {
                # Transform to lower case
                complaintData[,variable] <- tolower(complaintData[,variable])
                
                # Remove apostrophe (e.g. change cont'd to contd)
                complaintData[,variable] <-
                    gsub("([a-z]+)'([a-z]+)","\\1\\2",complaintData[,variable])

                # Remove punctuation
                # http://www.regular-expressions.info/posixbrackets.html
                complaintData[,variable] <-
                    sub("[[:punct:]]", " ", complaintData[,variable])
                
                # http://stackoverflow.com/questions/1981349/
                #   regex-to-replace-multiple-spaces-with-a-single-space
                complaintData[,variable] <-
                    sub("\\s\\s+"," ",complaintData[,variable])
            }
        }
    }    
}

computePercentMissingData <- function(complaintData) {
    numberObservations <- nrow(complaintData)
    percentMissingData <- list()

    for (variable in colnames(complaintData)) {
        isVariableMissing <- findMissingObservations(variable,
                                                     complaintData)
        
        percentMissingData[[variable]] <-
            100 * (sum(isVariableMissing) / numberObservations)
    }
    
    return(percentMissingData)
}

findMissingObservations <- function(variable,
                                    complaintData) {
    variableClass <- class(complaintData[1,variable])

    if (variableClass == "integer") {
        isVariableMissing <- is.na(complaintData[,variable])
    #----------------------------------------------------
    } else if (variableClass == "character") {
        isVariableMissing <- complaintData[,variable] == "" 
    #----------------------------------------------------
    } else {
        warning(sprintf("Need to add %s variable class", variableClass))
        
        isVariableMissing <- vector(mode="logical", nrow(complaintData))
    }
    
    return(isVariableMissing)
}

initializeVariablesToAnalyze <- function(maximumPercentMissing,
                                         percentMissingData,
                                         complaintData) {
    dataFrameSegmentation <- list()
    dataFrameSegmentation[["variables"]] <- vector()

    dataFrameSegmentation[["isValidRow"]] <-
        !vector(mode="logical", nrow(complaintData))

    for (variable in names(percentMissingData)) {
        if (percentMissingData[[variable]] <= maximumPercentMissing) {
            dataFrameSegmentation[["variables"]] <-
                append(dataFrameSegmentation[["variables"]], variable)
            
            isVariableMissing <- findMissingObservations(variable,
                                                         complaintData)
            
            dataFrameSegmentation[["isValidRow"]] <-
                dataFrameSegmentation[["isValidRow"]] & !isVariableMissing
        }
    }
    
    return(dataFrameSegmentation)
}

initializeIssueCategorySelection <- function(complaintData) {
    categorySelection <- list()

    categorySelection$Banking <-
        initializeBankingCategorySelection(complaintData) 

    categorySelection$Communication <-
        initializeCommunicationIssueSelection(complaintData)
    
    categorySelection$Credit <-
        initializeCreditIssueSelection(complaintData)
    
    categorySelection$CustomerService <-
        initializeCustomerServiceIssueSelection(complaintData)
    
    categorySelection$DebtCollection <-
        initializeDebtCollectionIssueSelection(complaintData)
    
    categorySelection$Fee <-
        initializeFeeIssueSelection(complaintData)
    
    categorySelection$FinancialCrime <-
        initializeFinancialCrimeIssueSelection(complaintData)
    
    categorySelection$Loan <-
        initializeLoanIssueSelection(complaintData)

    categorySelection$Mortgage <-
        initializeMortgageIssueSelection(complaintData)
    
    categorySelection$Other <-
        initializeOtherIssueSelection(complaintData)
    
    categorySelection$Transaction <-
        initiaizeTransationIssueSelection(complaintData)
    
    categorySelection$Privacy <- 
        initializePrivacyIssueSelection(complaintData)
    
    categorySelection$UnauthorizedTransaction <-
        initializeUnauthorizedTransactionIssueSelection(complaintData)
    
    return(categorySelection)
}

initializeBankingCategorySelection <- function(complaintData) {

    isBankingIssue <-
        grepl("^deposits and withdrawals$", complaintData$issue) |
        grepl("^convenience checks$", complaintData$issue) |
        grepl("^adding money$", complaintData$issue) |
        grepl("^lost or stolen money order$", complaintData$issue) |
        grepl("^lost or stolen check$", complaintData$issue) |
        grepl("^incorrect exchange rate$", complaintData$issue) |
        grepl("^overdraft savings or rewards features$", complaintData$issue) |
        grepl("^using a debit or atm card$", complaintData$issue) |
        grepl("^managing opening or closing account$",
              complaintData$issue) |
        grepl("^balance transfer$", complaintData$issue) |
        grepl("^rewards$", complaintData$issue) |
        grepl("^closing cancelling account$", complaintData$issue) |
        grepl("^credit card protection debt protection$",
              complaintData$issue) |
        grepl("^account opening closing or management$", complaintData$issue)

    return(isBankingIssue)
}

initializeCommunicationIssueSelection <- function(complainData) {
    
    isCommunicationIssue <-
        grepl("^advertising marketing or disclosures$", complaintData$issue) |
        grepl("^taking threatening an illegal action$" , complaintData$issue) |
        grepl("^disclosures$", complaintData$issue) |
        grepl("^account terms and changes$", complaintData$issue) |
        grepl("^cant contact lender$", complaintData$issue) |
        grepl("^billing statement$", complaintData$issue) |
        grepl("^advertising and marketing$", complaintData$issue) |
        grepl("^incorrect missing disclosures or info$", complaintData$issue)
    
    return(isCommunicationIssue)
}

initializeCreditIssueSelection <- function(complaintData) {
    
    isCreditIssue <-
        grepl("^incorrect information on credit report$",
              complaintData$issue) |
        grepl("^unable to get credit report credit score$",
              complaintData$issue) |
        grepl("^credit reporting companys investigation$",
              complaintData$issue) |
        grepl("^credit monitoring or identity protection$",
              complaintData$issue) |
        grepl("^credit reporting$", complaintData$issue) |
        grepl("^bankruptcy$", complaintData$issue) |
        grepl("^credit determination$", complaintData$issue) |
        grepl("^improper use of my credit report$", complaintData$issue)
    
    return(isCreditIssue)
}

initializeCustomerServiceIssueSelection <- function(complaintData) {

    isCustomerServiceIssue <-
        grepl("^billing disputes$", complaintData$issue) |
        grepl("^arbitration$", complaintData$issue) |
        grepl("^charged bank acct wrong day or amt$", complaintData$issue) |
        grepl("^other service issues$", complaintData$issue) |
        grepl("^customer service customer relations$", complaintData$issue)
    
    return(isCustomerServiceIssue)
}

initializeDebtCollectionIssueSelection <- function(complaintData) {

    isDebtCollectionIssue <-
        grepl("^contd attempts collect debt not owed$",
              complaintData$issue) |
        grepl("^problems when you are unable to pay$", complaintData$issue) |
        grepl("^problems caused by my funds being low$", complaintData$issue) |
        grepl("^delinquent account$", complaintData$issue) |
        grepl("^lender repossessed or sold the vehicle$",
              complaintData$issue) |
        grepl("^repaying your loan$", complaintData$issue) |
        grepl("^collection practices$", complaintData$issue) |
        grepl("^collection debt dispute$", complaintData$issue) |
        grepl("^communication tactics$", complaintData$issue) |
        grepl("^cant repay my loan$", complaintData$issue) |
        grepl("^lender damaged or destroyed property$", complaintData$issue) |
        grepl("^lender sold the property$", complaintData$issue) |
        grepl("^lender damaged or destroyed vehicle$", complaintData$issue)
    
    return(isDebtCollectionIssue)
}

initializeFeeIssueSelection <- function(complaintData) {

    isFeeIssue <- 
        grepl("^cash advance fee$", complaintData$issue) |
        grepl("^other fee$", complaintData$issue) |
        grepl("^excessive fees$", complaintData$issue) |
        grepl("^overlimit fee$", complaintData$issue) |
        grepl("^fees$", complaintData$issue) |
        grepl("^balance transfer fee$", complaintData$issue) |
        grepl("^late fee$", complaintData$issue) |
        grepl("^unexpected other fees$", complaintData$issue) |
        grepl("^charged fees or interest i didnt expect$",
              complaintData$issue)
    
    return(isFeeIssue)
}

initializeFinancialCrimeIssueSelection <- function(complaintData) {

    isFinancialCrimeIssue <- 
        grepl("^fraud or scam$", complaintData$issue) |
        grepl("^false statements or representation$", complaintData$issue) |
        grepl("^identity theft fraud embezzlement$", complaintData$issue)
    
    return(isFinancialCrimeIssue) 
}

initializeLoanIssueSelection <- function(complaintData) {

    isLoanIssue <-
        grepl("^disclosure verification of debt$", complaintData$issue) |
        grepl("^money was not available when promised$",
              complaintData$issue) |
        grepl("^taking out the loan or lease$", complaintData$issue) |
        grepl("^applied for loan did not receive money$",
              complaintData$issue) |
        grepl("^getting a loan$", complaintData$issue) |
        grepl("^payoff process$", complaintData$issue) |
        grepl("^received a loan i didnt apply for$", complaintData$issue) |
        grepl("^credit line increase decrease$", complaintData$issue) |
        grepl("^apr or interest rate$", complaintData$issue) |
        grepl("^shopping for a loan or lease$", complaintData$issue) |
        grepl("^managing the line of credit$", complaintData$issue) |
        grepl("^shopping for a line of credit$", complaintData$issue) |
        grepl("^application processing delay$", complaintData$issue)
    
    return(isLoanIssue)
}
  
initializeMortgageIssueSelection <- function(complaintData) {

    isMortgageIssue <-
        grepl("^loan servicing payments escrow account$",
              complaintData$issue) |
        grepl("^loan modification collection foreclosure$",
              complaintData$issue) |
        grepl("^settlement process and costs$", complaintData$issue) |
        grepl("^managing the loan or lease$", complaintData$issue) |
        grepl("^dealing with my lender or servicer$", complaintData$issue) |
        grepl("^sale of account$", complaintData$issue) |
        grepl("^forbearance workout plans$", complaintData$issue) |
        grepl("^credit decision underwriting$", complaintData$issue) |
        grepl("^application originator mortgage broker$",
              complaintData$issue) |
        grepl("^loan servicing payments escrow account$",
              complaintData$issue)
    
    return(isMortgageIssue)
}

initializeOtherIssueSelection <- function(complaintData) {
    isOtherIssue <- 
        grepl("^other$", complaintData$issue)
    
    return(isOtherIssue)
}

initializePrivacyIssueSelection <- function(complaintData) {

    isPrivacyIssue <-
        grepl("^improper contact or sharing of info$", complaintData$issue) |
        grepl("^privacy$", complaintData$issue)
    
    return(isPrivacyIssue)
}
  
initiaizeTransationIssueSelection <- function(complaintData) {

    isTransactionIssue <- 
        grepl("^wrong amount charged or received$", complaintData$issue) |
        grepl("^making receiving payments sending money$",
              complaintData$issue) |
        grepl("^cash advance$", complaintData$issue) |
        grepl("^transaction issue$", complaintData$issue) |
        grepl("^other transaction issues$", complaintData$issue) |
        grepl("^payment to acct not credited$", complaintData$issue)

    return(isTransactionIssue)
}

initializeUnauthorizedTransactionIssueSelection <- function(complaintData) {

    isUnauthorizedTransactionIssue <-
        grepl("^unsolicited issuance of credit card$", complaintData$issue) |
        grepl("^unauthorized transactions trans issues$",
              complaintData$issue) |
        grepl("^cant stop charges to bank account$", complaintData$issue)    

    return(isUnauthorizedTransactionIssue)
}

findUncategorizedIssues <- function(categorySelection,
                                    complaintData) {
    isUncategorized <- vector(mode="logical", nrow(complaintData))

    for (key in names(categorySelection)) {
        isUncategorized <- isUncategorized | categorySelection[[key]]
    }
    isUncategorized <- !isUncategorized

    return (unique(complaintData[isUncategorized, c("issue")]))
}
