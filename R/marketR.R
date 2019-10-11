#' @title Segment Purchasing Customers by Recency
#
#' @description This package segments customers into New User, Retained or Returned for each month.
#
#' @param data A dataframe.
#' @param id_column_index A column index of the dataframe where Customer IDs are stored.
#' @param date_column_index A column index of the dataframe where date (of transaction) is stored in dmy format.
#' @param data_mau A dataframe which is the output of table_mau(data,id_column_index,date_column_index).
#
#' @return NULL
#
#' @examples
#' table_mau(data,1,2)
#' plot_mau(data_mau)
#
#' @export table_mau
#' @export plot_mau


table_mau <- function(data, id_column_index, date_column_index) {
  #To ignore warnings during usage
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)

  colnames(data)[id_column_index] <- 'ACCT_ID'
  colnames(data)[date_column_index] <- 'Date'
  #create month_year column in data
  #Date column needs to be in dmy format
  data_rev <- data %>%
    mutate(month_year = format(dmy(Date),'%Y-%m'))


  #remove duplicates by acct_id and month_year
  data_uniq <- sqldf("SELECT DISTINCT ACCT_ID, month_year FROM data_rev")
  data_uniq$month_year <- paste(data_uniq$month_year,'-01',sep='')

  #spread month_year by ACCT_ID
  data_grp <- data_uniq %>% group_by(ACCT_ID,month_year) %>% summarise(n_user = n_distinct(ACCT_ID))
  data_spread <- data_grp %>%
    spread("month_year","n_user")

  #replace 1s with the respective ACCT_ID and NAs with 0s
  data_cohort <- data_spread
  data_cohort_bin <- data_spread
  data_cohort_bin[,2:ncol(data_cohort_bin)] <- ifelse(is.na(data_cohort_bin[,2:ncol(data_cohort_bin)]),0,1)

  #deal with 2nd column, i.e. first month_year column
  data_cohort_bin <- as.data.frame(data_cohort_bin)
  data_cohort_bin_diff <- data_cohort_bin
  data_cohort_bin_diff[,2][which(data_cohort_bin[,2]==1)] <- "New User"

  #deal with retained users
  for(i in 3:ncol(data_cohort_bin)) {
    data_cohort_bin_diff[,i][which(data_cohort_bin[,i-1]==1 & data_cohort_bin[,i]==1)] <- "Retained"
  }

  #deal with new users
  x <- 3
  total <- data_cohort_bin[,(x-1)]
  total <- as.data.frame(total)
  colnames(total)[1] <- 'row_sums'
  data_cohort_bin_sum <- cbind(data_cohort_bin[,1:(x-1)],total)
  data_cohort_bin_diff[,x][which(data_cohort_bin_sum[,x]==0 & data_cohort_bin[,x]==1)] <- "New User"

  for(x in 4:ncol(data_cohort_bin_diff)){
    total <- rowSums(data_cohort_bin[,2:(x-1)])
    total <- as.data.frame(total)
    colnames(total)[1] <- 'row_sums'
    data_cohort_bin_sum <- cbind(data_cohort_bin[,1:(x-1)],total)
    data_cohort_bin_diff[,x][which(data_cohort_bin_sum[,x]==0 & data_cohort_bin[,x]==1)] <- "New User"
  }

  #deal with returned users
  for(x in 4:ncol(data_cohort_bin_diff)){
    total <- rowSums(data_cohort_bin[,2:(x-1)])
    total <- as.data.frame(total)
    colnames(total)[1] <- 'row_sums'
    data_cohort_bin_sum <- cbind(data_cohort_bin[,1:(x-1)],total)
    data_cohort_bin_diff[,x][which(data_cohort_bin_sum[,x] != (x-2) & data_cohort_bin_sum[,x] != 0 & data_cohort_bin[,(x-1)] == 0 & data_cohort_bin[,x] == 1 )] <- "Returned"
  }


  #create dataframe versions of table() for each column starting from column 2
  results_list <- list()

  for(i in 2:ncol(data_cohort_bin_diff)) {
    results_list <- c(results_list,list(data.frame(table(data_cohort_bin_diff[,i]))))
  }

  #find last column with nrow < 4
  for(i in length(results_list):1) {
    if(nrow(results_list[[i]]) < 4 ) {
      break
    }
  }

  #i

  #appending categories to elements of list with nrow < 4
  for(j in 1:i) {
    if(nrow(results_list[[j]] == 2)){
      if(results_list[[j]][2,1] == "New User") {
        #append Retained and Returned
        x1_reta <- data.frame('Retained',0)
        colnames(x1_reta) <- c('Var1', 'Freq')
        results_list[[j]] <- rbind(results_list[[j]],x1_reta)
        x1_retu <- data.frame('Returned',0)
        colnames(x1_retu) <- c('Var1', 'Freq')
        results_list[[j]] <- rbind(results_list[[j]],x1_retu)
      }
      else if(results_list[[j]][2,1] == "Retained") {
        #append New and Returned
        x1_reta <- data.frame('New User',0)
        colnames(x1_reta) <- c('Var1', 'Freq')
        results_list[[j]] <- rbind(results_list[[j]],x1_reta)
        x1_retu <- data.frame('Returned',0)
        colnames(x1_retu) <- c('Var1', 'Freq')
        results_list[[j]] <- rbind(results_list[[j]],x1_retu)
        results_list[[j]] <- results_list[[j]] %>% arrange(as.character(Var1))
      }
      else{
        #append New and Retained
        x1_reta <- data.frame('New User',0)
        colnames(x1_reta) <- c('Var1', 'Freq')
        results_list[[j]] <- rbind(results_list[[j]],x1_reta)
        x1_retu <- data.frame('Retained',0)
        colnames(x1_retu) <- c('Var1', 'Freq')
        results_list[[j]] <- rbind(results_list[[j]],x1_retu)
        results_list[[j]] <- results_list[[j]] %>% arrange(as.character(Var1))
      }
    }
    else if(nrow(results_list[[j]]) == 3) {
      if('New User' %in% results_list[[j]][,1] && 'Retained' %in% results_list[[j]][,1]){
        #append Returned
        x1_reta <- data.frame('Returned',0)
        colnames(x1_reta) <- c('Var1', 'Freq')
        results_list[[j]] <- rbind(results_list[[j]],x1_reta)
        results_list[[j]] <- results_list[[j]] %>% arrange(as.character(Var1))
      }
      else if('New User' %in% results_list[[j]][,1] && 'Returned' %in% results_list[[j]][,1]) {
        #append Returned
        x1_reta <- data.frame('Retained',0)
        colnames(x1_reta) <- c('Var1', 'Freq')
        results_list[[j]] <- rbind(results_list[[j]],x1_reta)
        results_list[[j]] <- results_list[[j]] %>% arrange(as.character(Var1))
      }
      else {
        #append New
        x1_reta <- data.frame('New User',0)
        colnames(x1_reta) <- c('Var1', 'Freq')
        results_list[[j]] <- rbind(results_list[[j]],x1_reta)
        results_list[[j]] <- results_list[[j]] %>% arrange(as.character(Var1))
      }
    }
    else
    {
      results_list[[j]] <- results_list[[j]]
    }
  }

  for(j in 1:length(results_list)){
    results_list[[j]] <- results_list[[j]][!duplicated(results_list[[j]]$Var1),]
  }

  #initialise dataframe
  comb <- results_list[[1]]

  #combine all elements in results_list
  for(j in 2:length(results_list)){
    comb <- cbind(comb,results_list[[j]])
  }

  #renaming column names and deleting duplicate column names
  m <- colnames(data_cohort_bin_diff)[-1]
  n <- colnames(comb)

  for(i in 1:ncol(comb)){
    if(i == 2){
      n[i] <- m[1]
    }
    else if (i %% 2 == 0){
      n[i] <- m[i/2]
    }
  }

  colnames(comb) <- n

  comb <- comb[,!duplicated(colnames(comb))]

  colnames(comb)[1] <- 'Category'

  comb

}

plot_mau <- function(data_mau) {
  data_mau %>%
    filter(Category != 0) %>%
    gather(Month_Year, Users, -Category) %>%
    mutate(Month_Year = ymd(Month_Year)) %>%
    ggplot(aes(x=Month_Year,y=Users,fill=Category)) +
    geom_bar(stat='identity',colour="black",width=20) +
    stat_summary(fun.y=sum,aes(label=..y..,group=Month_Year),geom="text",vjust=-1,size=1.75) +
    scale_x_date(date_labels="%b %y",date_breaks="6 months")+
    ggtitle("New, Retained and Returned Users") +
    theme_wsj() + theme(legend.text = element_text(size=15,face="bold")) + scale_fill_wsj("colors6")

}
