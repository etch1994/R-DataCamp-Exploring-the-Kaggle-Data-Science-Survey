
# Load necessary packages
library(tidyverse)

# Load the data
responses <- read_csv('datasets/kagglesurvey.csv')

# Print the first 10 rows
head(responses,10)

library("testthat")
library('IRkernel.testthat')

run_tests({
    test_that("Read in data correctly.", {
        expect_is(responses, "tbl_df", 
            info = 'You should use read_csv (with an underscore) to read "datasets/kagglesurvey.csv" into responses')
    })
    
    test_that("Read in data correctly.", {
        responses_test <- read_csv('datasets/kagglesurvey.csv')
        expect_equivalent(responses, responses_test, 
            info = 'responses should contain the data in "datasets/kagglesurvey.csv"')
    })
    
})

# Print the first respondent's tools and languages
responses %>%
  filter(Respondent == 1) %>%
    select(WorkToolsSelect, LanguageRecommendationSelect)
# Create a new data frame called tools
tools <- responses

# Add a new column, and unnest the new column
tools <- tools  %>% 
    mutate(work_tools = strsplit(WorkToolsSelect, ","))  %>% 
    unnest(work_tools)

# View the first 6 rows of tools
head(tools)

run_tests({
    test_that("Tools and Languages were Split and Unnested", {
        expect_true(nrow(tools) == 47409, 
            info = 'Make sure that you split the tools at the commas and unnested them')
    })
    
    test_that("Tools and Languages were Unnested", {
        expect_is(tools$work_tools, "character", 
            info = 'The work_tools column should be of class "character". Make sure that you unnested the results of strsplit()')
    })
    
})

# Create a new data frame
tool_count <- tools

# Group the data by work_tools, summarise the counts, and arrange in descending order
tool_count <- tool_count  %>% 
    group_by(work_tools)  %>%
     summarise(count = n()) %>%  #or count('work_tools')
       arrange(desc(count))

#Print the first 6 results of tool_count
head(tool_count)

run_tests({
    test_that("Tools were Grouped and Summarised", {
        expect_true(nrow(tool_count) == 50, 
            info = 'Make sure that you grouped by tools and then summarised')
    })
    
    test_that("Values were sorted correctly", {
        expect_true(tool_count[1, 2] == 6073, 
            info = 'Do not forget to sort your tool counts from largest to smallest')
    })
    
})

# Create a bar chart of the work_tools column, most counts on the far right
ggplot(tool_count, aes(x = reorder(work_tools, count), y = count)) + 
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90))

run_tests({
   test_that("Plot is a bar chart",{
      p <- last_plot()
      q <- p$layers[[1]]
      expect_is(q$geom, "GeomBar", 
                info = "You should plot a bar chart with ggplot")
    })
})

# Create a new data frame called debate_tools
debate_tools <- responses

# Creat a new column called language preference
debate_tools <- debate_tools  %>% 
   mutate(language_preference = case_when(
             grepl("R", WorkToolsSelect) & ! grepl("Python", WorkToolsSelect) ~ "R",
             grepl("Python", WorkToolsSelect) & ! grepl("R", WorkToolsSelect) ~ "Python",
             grepl("R", WorkToolsSelect) &  grepl("Python", WorkToolsSelect) ~ "both",
             ! grepl("R", WorkToolsSelect) & ! grepl("Python", WorkToolsSelect) ~ "neither"
   ))

# Print the first 6 rows
head(debate_tools,300)

run_tests({
    test_that("New column was created", {
        expect_is(debate_tools$language_preference, "character", 
            info = 'The language_preference column should be of class "character". Make sure that you filled this new column correctly')
    })
    
})

# Create a new data frame
debate_plot <- debate_tools

# Group by language preference, calculate number of responses, and remove "neither"
debate_plot <- debate_plot  %>% 
   group_by(language_preference)  %>% 
   summarise(count = n()) %>%
    filter(language_preference != "neither")
head(debate_plot)


# Create a bar chart
ggplot(debate_plot, aes(x = language_preference, y = count)) +
geom_bar(stat = 'identity') 

run_tests({
   test_that("Plot is a bar chart",{
      p <- last_plot()
      q <- p$layers[[1]]
      expect_is(q$geom, "GeomBar",
               info = "You should plot a bar chart with ggplot")
    })
})

# Create a new data frame
recommendations <- debate_tools

# Group by, summarise, filter, arrange, mutate, and filter
recommendations <- recommendations  %>% 
    group_by(language_preference,LanguageRecommendationSelect)  %>%
    summarise(count = n()) 

tidy <- filter(recommendations, LanguageRecommendationSelect != 'NA' ) %>%
    arrange(language_preference, desc(count)) 

final <- tidy %>%
group_by(language_preference) 
a = filter(final, language_preference == 'Python') %>% head(4) 
b = filter(final, language_preference == 'R') %>% head(4) 
c = filter(final, language_preference == 'both') %>% head(4) 
d = filter(final, language_preference == 'neither') %>% head(4) 
 
recommendations <- rbind(a,b,c,d)
recommendations

run_tests({
    test_that("Tools have been summarised", {
        expect_true(nrow(recommendations) == 16, 
            info = 'Make sure that you are only keeping the top 4 responses for each language used')
    })
    
})

# Create a faceted bar plot
ggplot(recommendations, aes(x = LanguageRecommendationSelect, y = count)) +
    geom_bar(stat = "identity") +
    facet_wrap(~language_preference) +
    theme(axis.text.x = element_text(angle = 90))

run_tests({
   test_that("Plot is a bar chart",{
      p <- last_plot()
      q <- p$layers[[1]]
      expect_is(q$geom, "GeomBar")
    })
})

# Would R users find this statement TRUE or FALSE?
R_is_number_one = TRUE

run_tests({
    test_that("The question has been answered", {
        expect_true(R_is_number_one, 
            info = 'Try again! Should R_is_number_one be set to TRUE or FALSE?')
    })
    
})
