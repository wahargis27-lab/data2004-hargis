# Lab 2: is this file ready to analyze?

library(tidyverse)


# you're a junior analyst at a state library association. a colleague
# downloaded the 2024 Public Libraries Survey and wants to compare
# library systems on visits, circulation, and programs.

# your supervisor wants an audit before anyone analyzes anything.

# nothing today is new. you've done every piece of this already.


# grain and key

libraries <- read_csv(
  "Data/Raw/PLS_FY24_AE_pud24i.csv",
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
)

glimpse(libraries)

# one row is one one library system. 

# the download also has an "outlet" file. it has more rows.
# why would it? (the user's guide will tell you.)
# one of the reasons is the outlet has indetifiying information on top of outlet being braoder in scope

## this is one row per outlet, so centrla library, branch, bookmobile

# which column should identify a row?
#FSCSKEY

# what did those tell you?
# what would it have meant if the count() came back with rows?
#it would mean each id is unique
libraries %>%
  summarise(
    n = n_distinct(FSCSKEY)
  )

# let's focus on VISITS for now. what does this column mean?
#how many visits were paid to the library

# VISITS counts visits to the library in a year.
# before you run anything: what values would be impossible?
#anything that does not belong to the set of natural numbers

# how many different negative values? how many rows of each?
libraries %>%
  filter(VISITS < 0) |>
  summarise(
    n = n()
  )
#74

# is a negative number here bad data, missing data, or a code?
# can you tell from the data alone?
#we cannot tell from the data alone

# does R think any of these are missing?

is.na(libraries$VISITS) |>
  table()

libraries %>%
  filter(VISITS < 0) |>
  filter(is.na(VISITS)) |>
  summarise(
    n = n()
  )

#r says they're not missing

# on tuesday, NA told us THAT something was missing but not WHY.
# what's different here?
#There's a lot of reasons for data to be missing. Like was it not recorded vs what was recorded isn't appliable.
#For example: the negative data might be unintentionally missing whereas the na data is intentionally missing which means radically diffrent thing

# the survey doesn't use NA values, it uses codes. R cannot tell that -1 or -3 is missing. 


# go to the user's guide. for each negative value:
# what does it mean, in the guide's words? where did you find it?
#if num = -1 then num = .M; /*recode missing value into .M*/
#if num = -3 and STATSTRU ='23' then num = .C; /*recode Temporary Closed
#Library into .C*/
#  if num = -4 then num = .N; /*recode "Not Applicable" into .N*/
#  if num = -9 then num = .S; /*recode suppressed value into .S*/

# do the two codes mean the same thing?
# No. One is something wasn't reported. The other is temporarily closed. 


# every numeric column has a flag column. find the one for VISITS.
#F_VISITS


# what does the flag tell you? does it tell the two codes apart?
#  that the library was either temporarily closed or the value is missing

distinct(libraries, F_VISITS)

# make a clean version. VISITS stays exactly as it is.

libraries <- libraries |>
  mutate(visits_clean = if_else(VISITS %in% c(-1, -3), NA_real_, VISITS))

# why list the codes instead of writing VISITS < 0?

#because we are interested in some of the negative flags

# both codes just turned into NA. what did we lose?
# where can we still find it?
libraries %>%
  filter(VISITS < 0 & !is.na(visits_clean))
#we lost the other negative codes but we can still find them in the orginal coloumns



# did it do what we meant? three questions:
# same number of library systems?
#yes
# did every code become NA?
#no
# did anything else become NA?
#yes, the other negative visitor codes

# does any of this matter?
#it matters if we care about those other codes which I'm assuming is yes

# why is the raw mean lower? what's in each denominator?
#because it has an artificially inflated denominator
# Raw has 9,249 systems and clean has 9,175. Small differences because it's 0.8% difference. 



# your turn :)
# get in your group project groups

# do that process for each of the following:
# TOTATTEN - program attendance
# TOTPRO - number of programs
# TOTCIR - total circulation

libraries <- libraries |>
  mutate(total_attendace_clean = if_else(TOTATTEN %in% c(-1, -3), NA_real_, TOTATTEN))

# you're not solving a new problem. same steps, different variable.
# everything you need is in the VISITS section.

# what should it measure? what would be impossible?
#it ought to have only valid values

# range. any odd values? how many of each?
max(libraries$TOTATTEN, na.rm = TRUE)
min(libraries$TOTATTEN, na.rm = TRUE)


# what does the user's guide say they mean?
# ("we couldn't find it" is an answer. "we assumed" isn't.)
# there are negative values that the guide says means
# -1 missing value 
#-3 temporarily closed


# what's the flag column? (the names get shortened. look for it.)
libraries$F_TOTATT


# clean version. keep the raw column.


# check it: same rows? every code became NA? nothing else did?
libraries %>%
  filter(TOTATTEN < 0 & !is.na(total_attendace_clean))

#Every negative value is na, but the positive values of course stay in tact

# mean before and after. big change or small?
mean(libraries$total_attendace_clean, na.rm = TRUE)
#11910.21
mean(libraries$TOTATTEN, na.rm = TRUE)
#11387.35
# a medium chnage


# where are they? 

# recoding to NA fixes the number. does it finish the job?
# It depends on the distribution, because something might be reported as too high

# are the coded rows spread out, or do they bunch up?
ggplot(libraries, aes(x = total_attendace_clean)) +
  geom_boxplot(fill = "steelblue") 

#There's a lot of outliers

# if someone compares circulation across states, what goes wrong?
#different forms of measurement I'm assuming

# this is for you to answer
# is this file ready to analyze as is? 3-4 sentences.
#no, there would need to be a good answer for the distribution. 
#right now there are a lot of very suspicious outliers including a library with an attenacde of over 1 million
#I'd first need to learn the reason for this distribution and if it's truely normal to make a judgement call
