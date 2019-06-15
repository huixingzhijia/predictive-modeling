
df <- read_excel('/Users/wenhuizeng/Library/Mobile Documents/com~apple~CloudDocs/read books/crack interview/Tenants.xlsx')
df1 <- read_excel('/Users/wenhuizeng/Library/Mobile Documents/com~apple~CloudDocs/read books/crack interview/AptTenants.xlsx')
colnames(df)[1] <- "ID"


#The following are same results, count non-missing result of AptID
inner_join(df,df1,by=c('ID'='TenantID')) %>% filter(!is.na(AptID)) %>% count(ID) 
inner_join(df,df1,by=c('ID'='TenantID')) %>% filter(!is.na(AptID)) %>% group_by(ID) %>% count() 
inner_join(df,df1,by=c('ID'='TenantID')) %>% filter(!is.na(AptID)) %>% group_by(ID) %>% tally()


#Question 1 Result:
#Method 1
inner_join(df,df1,by=c('ID'='TenantID')) %>% filter(!is.na(AptID)) %>% group_by(ID) %>% mutate(cnt=n()) %>% 
filter(cnt>1)%>% ungroup() %>% select(TenantName) %>% unique()
  
#Method 2
inner_join(df,df1,by=c('ID'='TenantID')) %>% filter(!is.na(AptID)) %>% add_count(ID) %>% filter(n>1) %>% 
distinct(TenantName)

#group by ID and group by AptID and count
inner_join(df,df1,by=c('ID'='TenantID')) %>% group_by(ID) %>% count(AptID) 

data.clean %>% select(-c('Age','Sex'))


library(lubridate)
df2 <- read_excel('/Users/wenhuizeng/Library/Mobile Documents/com~apple~CloudDocs/read books/crack interview/Apt_status.xlsx')
df3 <- read_excel('/Users/wenhuizeng/Library/Mobile Documents/com~apple~CloudDocs/read books/crack interview/Request.xlsx')

colnames(df2)[1]<-'ID'

left_join(df2,df3,by=c('ID'='AptID')) %>% mutate(status=ifelse(Status=='Open',1,0)) %>% group_by(BuildingID) %>%
  summarise(num=sum(status))


left_join(df2,df3,by=c('ID'='AptID')) %>% group_by(BuildingID) %>% summarise(num=sum(Status=='Open'))


### LinkedIN

df1 <- read_excel('/Users/wenhuizeng/Library/Mobile Documents/com~apple~CloudDocs/read books/crack interview/LinkedIn CID .xlsx',sheet='spend')
df2 <- read_excel('/Users/wenhuizeng/Library/Mobile Documents/com~apple~CloudDocs/read books/crack interview/LinkedIn CID .xlsx',sheet='Campaigns')
df3 <- read_excel('/Users/wenhuizeng/Library/Mobile Documents/com~apple~CloudDocs/read books/crack interview/LinkedIn CID .xlsx',sheet='Exchange_rate')
df1$Date <- ymd(df1$Date)


# Q1 CID, Total spend in USD

inner_join(df1,df3,by=c('Currency')) %>% mutate(spend=Spend_amount*`Rate(to USD)`) %>% 
  group_by(`Campaign_id (CID)`) %>% summarise(total=sum(spend))



# Q2 AID, number of days from first spend date to highest spend date

 inner_join(df1,df2,by=c("Campaign_id (CID)")) %>% inner_join(df3,by=c('Currency')) %>% 
  mutate(total=Spend_amount*`Rate(to USD)`) %>% group_by(`Account_id (AID)`) %>% 
  mutate(first_date=min(Date),highest_spend=max(total)) %>% filter(total==highest_spend) %>%
   mutate(date_diff=Date-first_date) %>% select(`Account_id (AID)`,date_diff)
 

#dplyr traditional way
df <- merge(df1,df3,by=c('Currency'))
df['Total']=df['Spend_amount']*df['Rate(to USD)']
df3 <- merge(df,df2,by=c('Campaign_id (CID)'))

#df3$`Account_id (AID)`<- factor(df3$`Account_id (AID)`)
df4 <- df3 %>% group_by(`Account_id (AID)`) %>% summarise(max_value=max(Total),first_date=min(Date))
df5 <- merge(df3,df4,by.x =c("Account_id (AID)","Total"),by.y=c("Account_id (AID)","max_value"))




##pipeline way
a <- inner_join(df1,df2,by=c('Campaign_id (CID)')) %>%inner_join(df3,by=c('Currency')) %>% 
  mutate(total=Spend_amount*`Rate(to USD)`) 
 
 b <- a %>% group_by(`Account_id (AID)`) %>% 
  summarise(max_value=max(total),first_date=min(Date)) %>% 
  inner_join(a,by=c("Account_id (AID)", "max_value"= "total")) %>% mutate(Date-first_date)
  

##Given a daily login table showing when users logged in each day, 
 #figure out the number of customers that logged in two days in a row. (consecutively)

df <- read_csv('/Users/wenhuizeng/Library/Mobile Documents/com~apple~CloudDocs/read books/crack interview/LinkedIn_userlog.csv')
df$Date <- mdy(df$Date)

inner_join(df,df,by=c('User_id')) %>% mutate(datediff=Date.y-Date.x) %>% filter(datediff==1) %>%
  summarise(num=n_distinct(User_id))


#article view
data <- read_csv('/Users/wenhuizeng/Library/Mobile Documents/com~apple~CloudDocs/read books/crack interview/article.csv')
data$date <- mdy(data$date)

#Q1: how many article authors have never viewed their own article?
  
data %>% filter(author_id!=viewer_id) %>% distinct(author_id) %>% count()
data %>% filter(author_id!=viewer_id) %>% summarise(count=n_distinct(author_id))


#Q2: how many members viewed more than one article on 2017-08-01?

data %>% filter(date=='2017-08-01') %>% group_by(viewer_id) %>%
  summarise(count=n_distinct(article_id)) %>% filter(count>1) %>%
 count()




#Q1 find the country with largest population in each continent, 
#with strictly output: continent, country, population. 
#and sort by population in descending order.
#Consider corner case that two country have same largest population in the same continent. 

data <- read_csv('/Users/wenhuizeng/Library/Mobile Documents/com~apple~CloudDocs/read books/crack interview/continent.csv')

data %>% group_by(continent) %>% summarise(population=max(population)) %>% 
  inner_join(data,by=c("population"="population","continent"="continent")) %>% 
  arrange(desc(population))%>% select(continent,country,population)   

#Q2 now for each continent, find the country with largest % of populaiton in given continent. 

data %>% group_by(continent) %>% mutate(pct=population/sum(population),max_pct=max(pct)) %>%
  filter(pct==max_pct) %>% select(country)


# Microsoft

#Q1: count members who ever moved from Microsoft to Google?
data <- read_excel('/Users/wenhuizeng/Library/Mobile Documents/com~apple~CloudDocs/read books/crack interview/company.xlsx')

inner_join(data,data,by=c("Member_id"="Member_id")) %>% 
  filter(Company.x=="Microsoft" & Company.y=="Google" & Year_Start.x<Year_Start.y) %>%
  summarise(count=n_distinct(Member_id))
  


#Q2  count members who directly moved from Microsoft to Google? (Microsoft -- Linkedin -- Google doesn't count)

a <- data %>% group_by(Member_id) %>% mutate(year_rank=rank(Year_Start,ties.method='first')) %>% ungroup()
 
inner_join(a,a,by=c("Member_id"="Member_id"))%>%
  filter(Company.x=="Microsoft" &Company.y=="Google" & year_rank.x+1==year_rank.y) %>% 
 count()


#Q3Which is the most popular company given a specific year:2013

data %>% filter(Year_Start==2000) %>% group_by(Company) %>% 
  summarise(count=n_distinct(Member_id)) %>% top_n(1)


## Customer Table output
#table,customer, product.A, poduct.B, product.C
#1        x1          x2          x3


data <- read_csv('/Users/wenhuizeng/Library/Mobile Documents/com~apple~CloudDocs/read books/crack interview/customer.csv')

#gather(data,key,value) takes multiple columns, and gathers them into key-value pairs: it makes “wide” data longer.
# Melt

#spread(data,key,value) takes two columns (key & value), and spreads into multiple columns: it makes “long” data wider.
# dcast()

#long to wide
b <-spread(data,product,amount)

#gather A,B,C, wide to long
gather(b,key="product",value="amount",A,B,C)


##














