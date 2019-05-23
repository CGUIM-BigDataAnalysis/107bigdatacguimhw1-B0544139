library(jsonlite)
library(dplyr)
library(readr)
#1

X103_edu_salary <- read_csv("C:/R_ImportData/103_edu_salary.csv", 
                            locale = locale(encoding = "BIG5"))
X104_edu_salary <- read_csv("C:/R_ImportData/104_edu_salary.csv", 
                            locale = locale(encoding = "BIG5"))
X105_edu_salary <- read_csv("C:/R_ImportData/105_edu_salary.csv", 
                            locale = locale(encoding = "BIG5"))
X106_edu_salary <- read_csv("C:/R_ImportData/106_edu_salary.csv", 
                            locale = locale(encoding = "BIG5"))
join103_106 <- inner_join(X103_edu_salary,X106_edu_salary,by = "大職業別")
join103_106 <- mutate(join103_106,`大學_薪資比例` = `大學-薪資.y`/`大學-薪資.x`)

join103_106_new <- filter(join103_106,`大學_薪資比例` > 1)
arrange(join103_106_new,desc(`大學_薪資比例`))
head(join103_106_new,10)


filter(join103_106,`大學_薪資比例` > 1.05)

split_time <- c(strsplit (join103_106$`大職業別`,"-"))
split_new <- data.frame(job_title=c(1:length(split_time)))
for (n in 1:length(split_time)) {
  split_new[n,1] <- split_time[[n]][1]
}
table(split_new)


#2
head(arrange(filter(X103_edu_salary,`大學-女/男`< 100),`大學-女/男`),10)
head(arrange(filter(X104_edu_salary,`大學-女/男`< 100),`大學-女/男`),10)
head(arrange(filter(X105_edu_salary,`大學-女/男`< 100),`大學-女/男`),10)
head(arrange(filter(X106_edu_salary,`大學-女/男`< 100),`大學-女/男`),10)


head(arrange(filter(X103_edu_salary,`大學-女/男`> 100),desc(`大學-女/男`)),10)
head(arrange(filter(X104_edu_salary,`大學-女/男`> 100),desc(`大學-女/男`)),10)
head(arrange(filter(X105_edu_salary,`大學-女/男`> 100),desc(`大學-女/男`)),10)
head(arrange(filter(X106_edu_salary,`大學-女/男`> 100),desc(`大學-女/男`)),10)



#3
X106_edu_salary <- mutate(X106_edu_salary,`研究所_大學_薪資差異比例` = `研究所及以上-薪資`/`大學-薪資`)
head(arrange(X106_edu_salary,desc(`研究所_大學_薪資差異比例`)),10)



#4
X106_edu_salary_new <- filter(X106_edu_salary,`大職業別` %in% c("資訊及通訊傳播業-專業人員",
                                                                "專業_科學及技術服務業-專業人員",
                                                                "服務業部門-專業人員",
                                                                "醫療保健服務業-專業人員",
                                                                "藝術_娛樂及休閒服務業-專業人員"))
X106_edu_salary_new <- select(X106_edu_salary_new,`大職業別`,`大學-薪資`,`研究所及以上-薪資`)
X106_edu_salary_new

X106_edu_salary_new <- mutate(X106_edu_salary_new,`研究所_大學_薪資差異` = `研究所及以上-薪資`-`大學-薪資`)
X106_edu_salary_new




