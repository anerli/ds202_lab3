# Libraries
library(ggplot2)
library(dpylr)

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



