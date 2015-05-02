downloadData <- function(dataDirectory) {
    #-------------------------------------------------------------------------
    # Downloads the Consumer Finanical Protection Bureau Consumer
    # Compllaint Database
    #
    # Args:
    #   dataDirectory: String that stores the data download directory
    #                  path
    #
    # Returns:
    #   None
    #-------------------------------------------------------------------------
    if (!dir.exists(dataDirectory)) {
        dir.create(dataDirectory)    
    }

    # Initialize the CFPB Consumer Complaint Database URL
    fileURL <- paste0('https://data.consumerfinance.gov/api/views',
                      '/x94z-ydhh/rows.csv?accessType=DOWNLOAD')
    
    download.file(fileURL, destfile=file.path(dataDirectory,
                                              "/rows.csv"), method="curl")
}

createAnalyticDataset <- function(dataDirectory,
                                  maximumPercentMissing,
                                  analyticDataPath) {
    #-------------------------------------------------------------------------
    # Creates an analytic (i.e. tidy) data set. This process includes:
    # 1.) Cleaning the CFPB consumer complaint database
    # 2.) Splitting the data into training, validation, & test sets
    #
    # Args:
    #   dataDirectory: String that stores the data download directory
    #                  path
    #
    #   maximumPercentMissing: Defines the maximum percent of missing
    #                          data allowed for a variable. 
    #
    #   analyticDataPath: String that stores the analytic data directory path
    #
    # Returns:
    #   None
    #
    # Technical references:
    #   https://www.coursera.org/specialization/jhudatascience/1
    #
    # http://www.prometheusresearch.com/
    #   good-data-management-practices-for-data-analysis-tidy-data-part-2/
    #-------------------------------------------------------------------------
    csvFile <- list.files(path=dataDirectory, pattern="*.csv")
    if (length(csvFile) != 1) {
        simpleError('Number of *.csv files != 1')
    }

    cleanData <- cleanComplaintData(file.path(dataDirectory, csvFile),
                                    maximumPercentMissing=10.0)
    
    complaintData <- cleanData$complaintData
    percentMissingData <- cleanData$percentMissingData
    rm(cleanData)
    
    if (!dir.exists(analyticDataPath)) {
        dir.create(analyticDataPath)    
    }

    save(file=file.path(analyticDataPath, "PercentMissingData.RData"),
         percentMissingData)
    
    splitData(analyticDataPath,
              complaintData)
}

cleanComplaintData <- function(csvFilePath,
                               maximumPercentMissing) {
    #-------------------------------------------------------------------------
    # Transforms the CPFB Consumer Complaint Database into tidy data via
    # the following operations:
    #
    # 1.) Transform the variable names to lower case
    #
    # 2.) Remove '.' from variale names
    #
    # 3.) Remove U.S. states & territories outside of the continental U.S.
    #
    # 4.) Apply a maximum percent missing threshold to remove partial 
    #     observations
    #
    # 5.) Remove whitespace from the company type
    #
    # 6.) For each database variable:
    #     a.) Transform to lower case
    #     b.) Remove apostrophe (e.g. change cont'd to contd)
    #     c.) Remove punctuation
    #     d.) Replace multiple spaces with a single space
    #
    # 7.) Remove white space from the company type
    #
    # 8.) Aggregate the complaint issues into categories
    #
    # 9.) Remove the "other" category
    #
    # 10.) Append city, state, latitude, & lognitude to consumer complaint 
    #      database
    #
    # 11.) Add a companyid variable
    #
    # 12.) Convert date variables
    #
    # 13.) Convert factor variables
    #
    # Args:
    #   csvFilePath: String that stores the CPFB Consumer Complaint Database
    #                *.csv file path
    #
    #   maximumPercentMissing: Defines the maximum percent of missing
    #                          data allowed for a variable. 
    #
    # Returns:
    #   complaintData: Data frame that stores the transformed CPFB Consumer
    #                  Complaint Database
    #-------------------------------------------------------------------------
    complaintData <- read.csv(csvFilePath,
                              header=TRUE,
                              stringsAsFactors=FALSE)

    # Transform the variable names to lower case
    colnames(complaintData) <- tolower(colnames(complaintData))

    # Remove '.' from variable names
    colnames(complaintData) <- gsub('\\.','',colnames(complaintData))
    
    # Remove U.S. territories & states outside of the continental U.S.
    continentalUSStates <- append(state.abb, "DC")

    complaintData <-
        complaintData[which(complaintData$state %in% continentalUSStates),]
    
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
                    gsub("[[:punct:]]", " ", complaintData[,variable])
                
                # http://stackoverflow.com/questions/1981349/
                #   regex-to-replace-multiple-spaces-with-a-single-space
                complaintData[,variable] <-
                    gsub("\\s\\s+"," ",complaintData[,variable])
            }
        }
    }
    
    # Remove white space from company type
    complaintData$company <- gsub("p\\sc","pc",complaintData$company)
    complaintData$company <- gsub("l\\sl\\sc","llc",complaintData$company)
    
    # Initialize complaint issue category
    categorySelection <- initializeIssueCategorySelection(complaintData)

    complaintData$issuecategory <- NA
    for (category in names(categorySelection)) {
        complaintData[categorySelection[[category]],"issuecategory"] <-
            tolower(category)
    }

    isOtherCategory <- complaintData$issuecategory == "other"
    
    percentMissingData$othercategory <- 100 * sum(isOtherCategory) / 
                                        nrow(complaintData)
    
    complaintData <- complaintData[!isOtherCategory,]

    # Append city, state, latitude, & lognitude to consumer complaint database 
    data(zipcode)
    colnames(zipcode) <- c("zipcode","city","state","latitude","longitude")
    zipcode$zipcode <- as.integer(zipcode$zipcode)
    complaintData$state <- toupper(complaintData$state)

    # http://www.rstudio.com/wp-content/uploads/2015/02/
    #   data-wrangling-cheatsheet.pdf
    complaintData <- dplyr::left_join(complaintData,
                                      zipcode,
                                      by=c("zipcode","state"))
    
    percentMissingData$dataframejoin <-
        100 * (1.0 - sum(complete.cases(complaintData)) /
                     nrow(complaintData))

    complaintData <- complaintData[complete.cases(complaintData),]

    # Add a companyid variable
    #
    # http://stackoverflow.com/questions/8214303/
    #   conditional-replacement-of-values-in-a-data-frame
    complaintData$companyid <- complaintData$company

    complaintData$companyid <- as.factor(complaintData$companyid)
    
    levels(complaintData$companyid) <-
        as.character(seq_len(length(levels(complaintData$companyid))))

    # Convert date variables
    for (dateVariable in c("datereceived","datesenttocompany")) {
        complaintData[,dateVariable] <- mdy(complaintData[,dateVariable])
    }
    
    # Convert factor variables
    factorVariables <- c("product",
                         "issue",
                         "state",
                         "zipcode",
                         "submittedvia",
                         "company",
                         "companyresponse",
                         "timelyresponse",
                         "consumerdisputed",
                         "issuecategory",
                         "city")
    
    for (factorVariable in factorVariables) {
        complaintData[,factorVariable] <-
            as.factor(complaintData[,factorVariable])
    }
        
    cleanData <- list()
    cleanData$complaintData <- complaintData
    cleanData$percentMissingData <- percentMissingData

    return(cleanData) 
}

splitData <- function(analyticDataPath,
                      complaintData) {
    #-------------------------------------------------------------------------
    # Splits the CPFB Consumer Complaint Database into training, validation,
    # and test data sets stored as *.csv files in the directory defined
    # by the analyticDataPath input variable
    #
    # Args:
    #   analyticDataPath: String that stores the analytic data directory path
    #
    #   complaintData: Data frame that stores the transformed CPFB Consumer
    #                  Complaint Database
    #
    # Returns:
    #   None
    # 
    # Technical References:
    # --------------------
    # http://stackoverflow.com/questions/13610074/
    #   is-there-a-rule-of-thumb-for-how-to-divide-a-dataset-into-
    #   training-and-validatio
    #
    # http://topepo.github.io/caret/splitting.html
    #-------------------------------------------------------------------------
    trainIndex <- createDataPartition(complaintData$issue,
                                      p = .8,
                                      list = FALSE,
                                      times = 1)
    
    testIndex <- seq_len(nrow(complaintData))
    testIndex <- testIndex[!(testIndex %in% trainIndex)]
    
    trainingData <- complaintData[trainIndex,]
    testData <- complaintData[testIndex,]
    
    trainIndex <- createDataPartition(trainingData$issue,
                                      p = .8,
                                      list = FALSE,
                                      times = 1)

    validationIndex <- seq_len(nrow(trainingData))
    validationIndex <- validationIndex[!(validationIndex %in% trainIndex)]
    
    validationData <- trainingData[validationIndex,]
    trainingData <- trainingData[trainIndex,]    

    if (is.na(file.info(analyticDataPath)[1,"isdir"])) {
        dir.create(analyticDataPath)
    }

    write.csv(file=file.path(analyticDataPath, "trainingData.csv"),
              trainingData,
              row.names=FALSE)
    
    write.csv(file=file.path(analyticDataPath, "validationData.csv"),
              validationData,
              row.names=FALSE)

    write.csv(file=file.path(analyticDataPath, "testData.csv"),
              testData,
              row.names=FALSE)
}

loadAnalyticData <- function(analyticDataPath,
                             analyticDataFile) {
    analyticData <- read.csv(file.path(analyticDataPath,
                                       analyticDataFile),
                             header=TRUE)
    
    analyticData$companyid <- as.factor(as.character(analyticData$companyid))
    
    return(analyticData)    
}

loadPercentMissingData <- function(analyticDataPath) {
    load(file=file.path(analyticDataPath,
                        "PercentMissingData.RData"))
    
    return(percentMissingData)
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
    
    percentMissingData[["numberobservations"]] <- numberObservations

    percentMissingData[["numbercompanies"]] <-
        length(unique(complaintData$company))
    
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

initializeVariables <- function(percentMissingData) {
    variables <- names(percentMissingData)
    variables <- variables[!variables %in% c("numberobservations",
                                             "othercategory",
                                             "dataframejoin")]
    
    return(variables)
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

initializeCommunicationIssueSelection <- function(complaintData) {
    
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
