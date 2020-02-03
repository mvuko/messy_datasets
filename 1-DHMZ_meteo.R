
library(tidyverse)

#input
dd<- read_lines("maksimalne temp")

str(dd)

#transform to a tibble
tabla<- tibble(text= dd)

str(tabla)

#years are an important variable which is here written in a row together
# with some unimportant metadata, so I will save years into a separate table

god <- as.data.frame(apply(tabla[grepl("Postaja", tabla$text), ], 1,
                           function(x) as.numeric(gsub("[^0-9]", "", x))))
colnames(god) <- "years"


#cleaning the table
tabla2 <- tabla %>%
        #trim white spaces
        mutate_if(is.character, str_trim) %>%
        #remove empty rows
        filter(text != "") %>%
        #filter out rows by regex expressions
        filter(!str_detect(text, "REPUBLIKA|KLIMA|ZAGREB|MINIMALNA|MAKSIMALNA|std|maks|datum|---|ampl|min|Postaja|VIII"))

#since the means are already calculated at the end of each year ("sred")
# I will use those results
tabla3<- tabla2 %>%
        filter(str_detect(text, "sred"))

#add the years
tabla4<- as.data.frame(cbind(tabla3$text, god$years)) %>%
        #and delete 1953 cause it has missing values in the beginning of the year
        filter(V2 != 1953)

#the measurements are not separated by an equal number of spaces, so I'll
# replace the spaces with commas
tabla5<- as.data.frame(gsub("[[:blank:]]+", ",", tabla4$V1))
colnames(tabla5)<- "commas"

#now they can be nicely separated into months
tabla5<- tabla5 %>%
        separate(commas, into = c("bla","jan", "feb", "mar", "apr", "may", "jun",
                                  "jul", "aug", "sep", "oct", "nov", "dec"),
                 sep = ",")
# add year column again
tabla6<- as.data.frame(cbind(tabla5, tabla4$V2))
colnames(tabla6)[14]<- "years"

# delete bla column
tabla6<- tabla6 %>%
        select(-bla) %>%
        # and move last column (years) to first
        select(years, everything())

# done!
