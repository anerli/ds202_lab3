# Libraries
#library(ggplot2)
#library(dpylr)
library(tidyverse)

# Requires install.packages('readxl')
dat <- readxl::read_xls('GSS.xls')

#View(dat)

'
names(dat)

# Rename Columns
dat <- dat %>%
  rename(
    years_edu=`Highest year of school completed`
  )

names(dat)

str(dat$years_edu)

#dat$years_edu <- as.numeric(dat$years_edu)
dat <- dat %>% mutate(years_edu = as.numeric(years_edu))

str(dat$years_edu)
'

"
# Format years of education and age as numeric
dat <- dat %>%
  mutate(
    `Highest year of school completed`=as.numeric(`Highest year of school completed`),
    `Age of respondent`=as.numeric(`Age of respondent`)
  )

str(dat$`Highest year of school completed`)
str(dat$`Age of respondent`)
"

# Rename columns
names(dat)
dat <- dat %>%
  rename(
    survey_year=`Gss year for this respondent`,
    god=`Rs confidence in the existence of god`,
    happiness=`General happiness`,
    lib_or_consv = `Think of self as liberal or conservative`,
    polparty = `Political party affiliation`,
    fam_income = `Total family income`,
    race = `Race of respondent`,
    sex = `Respondents sex`,
    years_edu = `Highest year of school completed`,
    age = `Age of respondent`,
    marital_status = `Marital status`,
    weekly_hrs_worked = `Number of hours usually work a week`,
    id = `Respondent id number`,
    ballot = `Ballot used for interview`
  )
names(dat)

# Reformat years education and age to numeric
dat <- dat %>%
  mutate(
    years_edu=as.numeric(years_edu),
    age=as.numeric(age)
  )


# === Part Two ===
# 1
dat %>% 
  group_by(marital_status) %>%
  summarize(avg_edu_years=mean(years_edu, na.rm=TRUE)) %>%
  arrange(desc(avg_edu_years))

'
We can see that people who did not answer about their marital status have significantly
higher education on average, for some reason. Also, those who were widowed or separated have less
education on average than those who were never married, divorced, or are married.
'

# 2
better_educated <- dat %>%
  group_by(marital_status) %>%
  mutate(avg_edu_years=mean(years_edu, na.rm=TRUE)) %>%
  filter(years_edu > avg_edu_years) %>%
  select(-avg_edu_years) %>%
  ungroup

str(better_educated)

'
# To show the new average edu is higher
better_educated %>% 
  group_by(marital_status) %>%
  summarize(avg_edu_years=mean(years_edu, na.rm=TRUE)) %>%
  arrange(desc(avg_edu_years))
'

# 3
#happiness_vals = c(2,1,0,NA)
#names(happiness_vals) = c('Very happy', 'Pretty happy', 'Not too happy', "Don't know")

"
get_hap_val <- function(hap_str){
  if (hap_str == 'Very happy') return(2);
  if (hap_str == 'Pretty happy') return(1);
  if (hap_str == 'Not too happy') return(0);
  
  # For Don't know / No answer / NA
  return(NA);
}
"

dat <- dat %>%
  mutate(hap_val=
          ifelse(happiness == 'Very happy', 2,
          ifelse(happiness == 'Pretty happy', 1,
          ifelse(happiness == 'Not too happy', 0, NA
          )))
        )
  #mutate(hap_val=get_hap_val(happiness))

dat %>%
  group_by(marital_status) %>%
  summarize(avg_happiness_val=mean(hap_val, na.rm=TRUE)) %>%
  arrange(desc(avg_happiness_val))

dat <- dat %>%
  mutate(hap_bool=
          ifelse(hap_val >= 1, TRUE,
          ifelse(hap_val == 0, FALSE, NA
          ))
        )


#dat <- dat %>%
#  mutate(hap_val=switch(
#    'Very happy'=2,
#    'Pretty happy'=1,
#    'Not too happy'=0,
#    "Don't know"=NA,
#    'No answer'=NA
#  ))

#dat <- dat %>%
#  mutate(happiness)

#dat$happiness <- factor(dat$happiness 

str(dat)

#%>% count(marital_status) %>% mutate(perc=n/nrow(marital_status)),

#count_sorted_order <- dat %>%
#  count(marital_status) %>%
#  .$marital_status

#ggplot(dat, aes(x=reorder(marital_status, count_sorted_order), fill=hap_bool)) + geom_bar(position='dodge')

#dat$marital_status <- factor(dat$marital_status)
dat$marital_status <- fct_infreq(dat$marital_status)
#reorder(dat$marital_status, dat$martial_status, FUN=length)

ggplot(dat, aes(x=marital_status, fill=hap_bool)) + geom_bar(position='dodge')





#dat1$marital_status <- fct_infreq(dat1$marital_status)
#dat1$marital_status <- factor(dat1$marital_status, levels=c('No answer', ))
#dat1$marital_status <- factor(dat1$marital_status, levels=unique(order(dat1$marital_status)))
#dat1 <- dat1 %>%
#  arrange(marital_status, years_edu)