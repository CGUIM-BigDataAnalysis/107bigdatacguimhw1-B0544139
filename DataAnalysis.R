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
##可看出提高比例較多的大部分是屬於專業人員，技術員及助理專業人員這些較強調專業能力的職業

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
##從103到106年，可發現男生薪資比女生多的行業相當多是技藝_機械設備操作及組裝人員及事務支援人員，
##此外這些人員大多是屬於電力及燃氣供應業、礦業及土石採取業及教育服務業

head(arrange(filter(X103_edu_salary,`大學-女/男`> 100),desc(`大學-女/男`)),10)
head(arrange(filter(X104_edu_salary,`大學-女/男`> 100),desc(`大學-女/男`)),10)
head(arrange(filter(X105_edu_salary,`大學-女/男`> 100),desc(`大學-女/男`)),10)
head(arrange(filter(X106_edu_salary,`大學-女/男`> 100),desc(`大學-女/男`)),10)
##從103到106年，可發現女生薪資比男生多的行業數量相當少，每一年最多只有一筆，103年甚至沒有，薪資的差異也很小


#3
X106_edu_salary <- mutate(X106_edu_salary,`研究所_大學_薪資差異比例` = `研究所及以上-薪資`/`大學-薪資`)
head(arrange(X106_edu_salary,desc(`研究所_大學_薪資差異比例`)),10)
##從結果可看出前十名有一半是屬於專業_科學及技術服務業、礦業及土石採取業、工業部門、
##工業及服務業部門等偏向理工科系的行業


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

##薪資跟我想像的差不多，大多都在35000~40000之間

##不會改變原本想要念研究所的心意，因為多了研究所的學歷，薪資確實增加不少，
##且讀研究所主要還是為了增進自己的專業能力，這樣才有可能在未來擁有比大學畢業出來還更好的出路


