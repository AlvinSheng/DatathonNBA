library(rvest)
library(stringr)
library(magrittr)
library(data.table)
library(tidyverse)

# Get salary dataset 
page <- read_html("https://www.spotrac.com/nba/contracts/")
salary.table <- page %>% html_table(header = FALSE) %>% extract2(1)
names(salary.table) <- salary.table[1,]
salary.table <- salary.table[-c(1,450,451),]
playerName = read.csv('1718.csv',header=FALSE)
playerName$V1 = as.character(playerName$V1)
playerName$V1[296] = "Elie Okobo"


# Get the contract year
ContractYear = c()
for (i in salary.table$Player){
  split1 = str_split(i,"\\(")[[1]][2]
  year = str_sub(split1, start=1, end = 4)
  ContractYear = c(ContractYear, year)
}
salary.table = cbind(ContractYear,salary.table)

player_name = c()
for (i in playerName$V1){
  split2 = str_split(i,"\xa0")[[1]][1]
  split2 = str_split(split2, "\\(")[[1]][1]
  split2 = gsub(" Jr\\.","",split2)
  split2 = gsub("\\.","",split2)
  player_name = c(player_name, split2)
}

salary.table = cbind(player_name,salary.table)
salary.table = cbind(1:nrow(salary.table),salary.table)
names(salary.table)[1] = "Index"
salary.table$ContractYear = as.numeric(as.character(salary.table$ContractYear))
salary.table$player_name = as.character(salary.table$player_name)

dupli_name = salary.table$player_name[ which(duplicated(salary.table$player_name))]

index = c()
for (i in dupli_name){
  compare_year = subset(salary.table, player_name==i )
  larger_obser = compare_year[which.max(compare_year$ContractYear),]
  smaller_obser = compare_year[which.min(compare_year$ContractYear),]
  if (larger_obser$ContractYear == 2019){
    index = c(index,larger_obser$Index)
  }else{
    index = c(index,smaller_obser$Index) 
    }
}

salary.table = salary.table[-index,]
salary.table[103,2] = "Moe Harkless"
salary.table[231,2] = "Bam Adebayo"
salary.table[248,2] = "DeAndre Bembry"
salary.table[314,2] = "Luc Mbah a Moute"
salary.table[317,2] = "Timothe Luwawu"
salary.table[324,2] = "Devonte Graham"
salary.table = salary.table[-361,]

################################################################################
# Get contract year salary dataset   14!!!!!!!! 
page2 <- read_html("https://hoopshype.com/salaries/players/2014-2015/")
contractYear14.table <- page2 %>% html_table(header = FALSE) %>% extract2(1)
contractYear14.table[1,1] = "Index"
names(contractYear14.table) <- contractYear14.table[1,]
contractYear14.table <- contractYear14.table[-1,]
contractYear14.table <- contractYear14.table[,-4]
names(contractYear14.table)[3] = "Contract2014"





################################################################################
# Get contract year salary dataset   15!!!!!!!! 
page3 <- read_html("https://hoopshype.com/salaries/players/2015-2016/")
contractYear15.table <- page3 %>% html_table(header = FALSE) %>% extract2(1)
contractYear15.table[1,1] = "Index"
names(contractYear15.table) <- contractYear15.table[1,]
contractYear15.table <- contractYear15.table[-1,]
contractYear15.table <- contractYear15.table[,-4]
names(contractYear15.table)[3] = "Contract2015"

################################################################################
# Get contract year salary dataset   16!!!!!!!! 
page4 <- read_html("https://hoopshype.com/salaries/players/2016-2017/")
contractYear16.table <- page4 %>% html_table(header = FALSE) %>% extract2(1)
contractYear16.table[1,1] = "Index"
names(contractYear16.table) <- contractYear16.table[1,]
contractYear16.table <- contractYear16.table[-1,]
contractYear16.table <- contractYear16.table[,-4]
names(contractYear16.table)[3] = "Contract2016"

################################################################################
# Get contract year salary dataset   17!!!!!!!! 
page5 <- read_html("https://hoopshype.com/salaries/players/2017-2018/")
contractYear17.table <- page5 %>% html_table(header = FALSE) %>% extract2(1)
contractYear17.table[1,1] = "Index"
names(contractYear17.table) <- contractYear17.table[1,]
contractYear17.table <- contractYear17.table[-1,]
contractYear17.table <- contractYear17.table[,-4]
names(contractYear17.table)[3] = "Contract2017"
contractYear17.table[245,2] = "Nene Hilario"

################################################################################
# Get contract year salary dataset   18!!!!!!!! 
page6 <- read_html("https://hoopshype.com/salaries/players/")
contractYear18.table <- page6 %>% html_table(header = FALSE) %>% extract2(1)
contractYear18.table[1,1] = "Index"
names(contractYear18.table) <- contractYear18.table[1,]
contractYear18.table <- contractYear18.table[-1,]
contractYear18.table <- contractYear18.table[,c(1,2,3)]
names(contractYear18.table)[3] = "Contract2018"
contractYear18.table[217,2] = "Mohamed Bamba"
################################################################################
lighter_salary = cbind(salary.table[,c(1,2,3,7)],rep(0,nrow(salary.table)))
names(lighter_salary)[5] = "Current Year Salary"
for (i in lighter_salary$player_name){
  year = substr(lighter_salary[which(i == lighter_salary$player_name),]$ContractYear,start=3,stop=4)
  tableName = paste("contractYear",year,".table",sep="")
  step2 = strsplit(i," ")[[1]]
  #step1 = head(strsplit(i," ")[[1]],-1)
  #step2 = tail(unlist(strsplit(step1, "(?<=[a-z])(?=[A-Z])", perl = TRUE)),2)
  judge1 = grepl(tolower(step2)[2], tolower(eval(as.name(paste(tableName)))$Player),ignore.case = TRUE)
  judge2 = grepl(tolower(step2)[1], tolower(eval(as.name(paste(tableName)))$Player),ignore.case = TRUE)
  index = which(judge1 & judge2)
  if (length(index) == 2){
    index = which(grepl(i,eval(as.name(paste(tableName)))$Player))
  }
  #index = which(grepl(i,eval(as.name(paste(tableName)))$Player))
  #if (length(index) != 0){
  lighter_salary[which(i == lighter_salary$player_name),]$`Current Year Salary` = 
    eval(as.name(paste(tableName)))[index,3]
  #}
}

salary = c()
for (i in lighter_salary$`Current Year Salary`){
  split = gsub(",","",i)
  split = gsub("\\$","",split)
  salary = c(salary,split)
}
salary = as.numeric(salary)
lighter_salary$`Current Year Salary` = salary
write.csv(lighter_salary, "LastYearSalary!.csv")


################################################################################
page <- read_html("https://www.basketball-reference.com/leagues/NBA_2018_totals.html")
per1718 <- page %>% html_table(header = TRUE) %>% extract2(1)
per1718 = cbind(year = rep(2018,nrow(per1718)),per1718)
page <- read_html("https://www.basketball-reference.com/leagues/NBA_2017_totals.html")
per1617 <- page %>% html_table(header = TRUE) %>% extract2(1)
per1617 = cbind(year = rep(2017,nrow(per1617)),per1617)
page <- read_html("https://www.basketball-reference.com/leagues/NBA_2016_totals.html")
per1516 <- page %>% html_table(header = TRUE) %>% extract2(1)
per1516 = cbind(year = rep(2016,nrow(per1516)),per1516)
page <- read_html("https://www.basketball-reference.com/leagues/NBA_2015_totals.html")
per1415 <- page %>% html_table(header = TRUE) %>% extract2(1)
per1415 = cbind(year = rep(2015,nrow(per1415)),per1415)
page <- read_html("https://www.basketball-reference.com/leagues/NBA_2014_totals.html")
per1314 <- page %>% html_table(header = TRUE) %>% extract2(1)
per1314 = cbind(year = rep(2014,nrow(per1314)),per1314)
stats = rbind(per1718,per1617,per1516,per1415,per1314)

player_name = c()
for (i in stats$Player){
  split2 = str_split(i,"\xa0")[[1]][1]
  split2 = str_split(split2, "\\(")[[1]][1]
  split2 = gsub(" Jr\\.","",split2)
  split2 = gsub("\\.","",split2)
  split2 = gsub("\\'","",split2)
  player_name = c(player_name, split2)
}
stats$Player = player_name


table(lighter_salary$ContractYear)

performance = stats[1:435,]
performance[,] = 0
#lighter_salary = lighter_salary[,-c(6:ncol(lighter_salary))]
lighter_salary = cbind(lighter_salary,performance)

lighter_salary[58,]$player_name = "Dennis Schroder"
#lighter_salary[71,]$player_name = "J.R. Smith"
lighter_salary[90,]$player_name = "Patty Mills"
lighter_salary[103,]$player_name = "Maurice Harkless"
lighter_salary[118,]$player_name = "ETwaun Moore"
lighter_salary[142,]$player_name = "Lou Williams"
lighter_salary[146,]$player_name = "DAngelo Russell"



for (i in 1:nrow(lighter_salary)){
  year1 = lighter_salary$ContractYear[i]
  sub_year = subset(stats, year==year1)
  index = which(lighter_salary$player_name[i] == sub_year$Player)
  lighter_salary[i,c(6:ncol(lighter_salary))] = sub_year[index,]
}

for (i in 1:nrow(lighter_salary)){
  which(!(lighter_salary$player_name[i] %in% stats$Player))
  }

Ifexis = c()
for (i in 1:nrow(lighter_salary)){
  jud = !lighter_salary$player_name[i] %in% stats$Player
  Ifexis = c(Ifexis,jud)
}
View(lighter_salary[which(Ifexis),])








