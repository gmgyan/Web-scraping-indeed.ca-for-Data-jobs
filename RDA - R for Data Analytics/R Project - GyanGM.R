## FUNCITON DEFINITIONS ##
  # User defined function to clean junk characters from data
tidy.data <- function(data)
{
  str_replace_all(data, regex('\r\n|\n|\t|\r|,|/|<|>|\\.'), ' ')
}

### SCRAPE FUNCTION I
  # Function to pull URL from web and extract HTML content as R dataset
  # First argument `search_page` is the first page of search results.
  # Second argument `nsPages` is the number of pages of search results to be scraped.

scrape_web = function(search_page, nsPages) {
  # Checking and installing required R packages
  # if (!require("magrittr")) {
  #   install.packages("magrittr")
  # }
  # if (!require("rvest")) {
  #   install.packages("rvest")
  # }
  # if (!require("tidyverse")) {
  #   install.packages("tidyverse")
  # }
  require(magrittr)
  require(rvest)
  require(tidyverse)
  
  # current_url is the page that loop works upon in each and every iteration.
  # Initializing it as search_page as that's the 1st page to be scraped our of other search results.
  current_url = search_page
  
  # Creating empty lists to store the scraped data.
  titles = list()
  companies = list()
  summaries = list()
  locations = list()
  joblinks = list()
  posteddate = list()
  
  # Setting index of the first element edited in each iteration of the loop to zero.
  k = 0
  # Looping through each search result.
  for (i in 1:nsPages) {
    # Navigating to current_url & loading it into R as XML.
    current_page = read_html(current_url)
    # Extracting job postings from current page, finding their HTML nodes containing job details, & storing them in empty lists created above.
    jobs = current_page %>%
    html_nodes('[class="  row  result"]')
    # Loop through each job posting.
    for (j in 1:length(jobs)) {
      
      # Get the job title and add it to a list.
      titles[[j + k]] = jobs[[j]] %>%
        html_nodes('[class=jobtitle]') %>%
        html_text()  # Outputted as character.
      
      # Get the company and add it to a list.
      companies[[j + k]] = jobs[[j]] %>%
        html_nodes('[class=company]') %>%
        html_text()
      
      # Get the job summary and add it to a list.
      summaries[[j + k]] = jobs[[j]] %>%
        html_nodes('[class=summary]') %>%
        html_text() 
      
      # Get the company location and add it to a list.
      locations[[j + k]] = jobs[[j]] %>%
        html_nodes('[class=location]') %>%
        html_text() 
      
      # Get the job URLs and add it to a list.
      joblinks[[j + k]] = jobs[[j]] %>%
        html_nodes("h2 a") %>%
        html_attr('href')
      
      # Get the job posted time and add it to a list.
      posteddate[[j + k]] = jobs[[j]] %>%
        html_nodes('[class=date]') %>%
        html_text() 
    }
    
    # Each list has k + j elements. Increase k so that we don't overwrite list contents in the next loop iteration.
    k = k + j
    # Now, we've scraped the data we want from every job ad on the current i.e. first search result page.
    # So, changing the current URL to be the URL of the next page of search results.
    # Note: Indeed's search result page url's follow a simple pattern. 
    # Page 1 is 'https://www.indeed.ca/jobs?q=data+scientist&l=Toronto%2C+ON'.
    # Page 2 is 'https://www.indeed.ca/jobs?q=data+scientist&l=Toronto%2C+ON&start=10'.
    # Page 3 is 'https://www.indeed.ca/jobs?q=data+scientist&l=Toronto%2C+ON&start=20'.
    current_url = paste0(search_page, '&start=', as.character(i * 10))
    
    # Wait 1-5 seconds # optional
    Sys.sleep(runif(n = 1, min = 1, max = 5))
    # This loop will repeat for every next page of search results until nsPages max value is reached.
  }
  
  # Combinining above lists into matrices that is also appending data in rows in new matrices.
  titles_final = do.call(rbind, titles)
  companies_final = do.call(rbind, companies)
  summaries_final = do.call(rbind, summaries)
  locations_final = do.call(rbind, locations)
  joblinks_final = do.call(rbind, joblinks)
  posteddate_final = do.call(rbind, posteddate)
  
  ## Some Data Cleaning using tidy.data() function created above and built-in function trimws.
  ## Basically removing junk characters and leading and trailing white spaces
  titles_final = trimws(tidy.data(titles_final))
  companies_final = trimws(tidy.data(companies_final))
  summaries_final = trimws(tidy.data(summaries_final))
  joblinks_final = trimws(joblinks_final)
  locations_final = trimws(tidy.data(locations_final))
  posteddate_final = trimws(tidy.data(posteddate_final))
  
  # Combining above matrices into one tibble {let's same data.frames with StringsAsFactors = F by default} and giving variable names.
  scraped = cbind(titles_final, companies_final, locations_final, summaries_final, paste0("https://www.indeed.ca",joblinks_final), posteddate_final) %>%
    as.tibble() %>%
    set_colnames(c('title', 'company', 'location', 'summary', 'link', 'postedD'))
  return(scraped)
}
# Scrapping 'data jobs' near by Ontario, Canada from www.indeed.ca by calling above function
ds_jobs_scrapped = scrape_web(
  search_page = 'https://www.indeed.ca/jobs?as_and=&as_phr=&as_any=%22analyst%22,+%22scientist%22&as_not=&as_ttl=&as_cmp=&jt=all&st=&salary=&radius=50&l=Toronto,+ON&fromage=any&limit=50&sort=&psf=advsrch',
  nsPages = 20
)

## Filtering is not required for this project I don't need it as while searching itself I have supplied 'Analyst or Scientist' job titles.
## But can be useful if let's say we want to pull all job posting from ON and then filter only some using below filter conditions. OR can be used to refine the current results into specific job title or locaiton

## TRANSFORMING OR APPLYING FILTERS
# ds_jobs_scrapped_2 = ds_jobs_scrapped %>%
#   filter(
#     grepl(pattern = 'analyst | scientist', x = title, ignore.case = TRUE)
#     # Remove cases where job title contains "analyst" or seniority level.
#     #, !grepl(pattern = 'analyst|sr.|jr.|senior|junior|lead|intern', x = title, ignore.case = TRUE)
#   ) 

#ds_jobs_scrapped
View(ds_jobs_scrapped)
# ds_jobs_scrapped[order(ds_jobs_scrapped$title),]

## REMOVING DUPLICATES
View(unique(ds_jobs_scrapped))
dsJobsFinal <-unique(ds_jobs_scrapped)

### SCRAPE FUNCTION II for scrapping most popular SKILL SETS required for DS jobs based on their running total
# Load packages
library(rvest)
library(stringr)
library(dplyr)
library(ggplot2)

# Search key words
job_title <- "\"analyst%2C+scientist\""
location <- "Toronto%2C+ON"

# Advanced search indeed site to get 50 results in a page
abs_URL <- 'https://www.indeed.ca/'
search_URL <- paste0('https://www.indeed.ca/jobs?as_and=&as_phr=&as_any=%22analyst%22%2C+%22scientist%22&as_not=&as_ttl=&as_cmp=&jt=all&st=&salary=&radius=50&l=Toronto%2C+ON&fromage=any&limit=50&sort=&psf=advsrch', job_title, '&l=', location)
cat(search_URL)
cat(abs_URL)

# get the html file from search url
start_page <- read_html(search_URL)

# get the total job count 
job_count <- unlist(strsplit(start_page %>% 
                               html_node("#searchCount") %>%
                               html_text(), split = ' ')) 
job_count <- as.numeric(str_replace_all(job_count[length(job_count)],',',''))
cat('Total jobs in market: ', job_count)

# Get start page job URLs
links <- start_page %>%
  html_nodes("h2 a") %>%
  # html_text()
  html_attr('href')

# Get actual job urls from each search page
individual_job_urls <- start_page %>%
  html_nodes(xpath = '//div[contains(@class,"pagination")]//a') %>%
  html_attr('href')

keySkills <- c('Hadoop','Python','\\bSQL', 'NoSQL','\\bR\\b', 'Spark', 'SAS', 'Excel', 'Java', 'Tableau', 'CSS', 'Oracle')

# Start scrapping data
ScrapeJobLinks <- function(res, job.links){
  for(i in 1:length(job.links)){
    job.url <- paste0(abs_URL,job.links[i])
    
    #Sys.sleep(0.001)
    cat(paste0('Reading job ', i, '\n'))
    
    tryCatch({
      html <- read_html(job.url)
      company <- unlist(strsplit(html %>%
                                   html_node(".company") %>%
                                   html_text(), split = ' '))
      text <- html_text(html)
      text <- tidy.data(text)
      df <- data.frame(skill = keySkills, count = ifelse(str_detect(text, keySkills), 1, 0))
      res$running$count <- res$running$count + df$count
      res$num_jobs <- res$num_jobs + 1
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  return(res)
}

# Keywords for display purpose removing escape characters
disp_keySkills <- c('Hadoop','Python','SQL', 'NoSQL','R', 'Spark', 'SAS', 'Excel', 'Java', 'Tableau', 'CSS', 'Oracle')

# Creating running total dataframe
running <- data.frame(skill = disp_keySkills, count = rep(0, length(disp_keySkills)))

# # Since the indeed only display max of 20 pages from search result, we cannot use job_count but need to track by creating a custom variable num_jobs
num_jobs <- 0
# 
# Here is our result that contains the two stats
results <- list("running" = running, "num_jobs" = num_jobs)

# Calling second UDF for most sought skill sets
if(job_count != 0){
  cat('Scraping jobs in first search page\n')
  results <- ScrapeJobLinks(results, links)
}

# for(p in 1:length(individual_job_urls)-1){
#   
#   cat('Searching next 50 jobs in next search page\n')
#   
#   # Navigate to next page
#   new.page <- read_html(paste0(abs_URL, individual_job_urls[p]))
#   
#   # Get new page job URLs
#   links <- new.page %>%
#     html_nodes("h2 a") %>%
#     html_attr('href')
#   
#   # Scrap job links
#   results <- ScrapeJobLinks(results, links)
# }


###*** ANALYSIS PART for Metro College Project ****### 

# Exporting extracted datasets
write.csv(dsJobsFinal, "dsJobsFinal2.csv")
write.csv(results$running, "textMining.csv")

# Q1. Location wise job listings ?
# install.packages("plyr")
require(plyr)
ljobs<- print(arrange(count(dsJobsFinal, 'location'), -freq))
ljobs

install.packages("plotrix")
library(plotrix)
pie3D(ljobs$freq, labels = ljobs$location, explode = 0.1, labelcol = "blue")

# Q2. What are top six companies with most jobs in data science ?
require(plyr)
cjobs <- head(print(arrange(count(dsJobsFinal, 'company'), -freq)))
lbls = c("TD", "CPP", "BMO", "Scotia", "TCC", "CIBC")
plot(cjobs$freq, type="b", main="Top 6 DS job providers", xlab = "Companies", ylab = "No of jobs"
        , col="brown")
text(cjobs$freq, lbls, pos = 1)

# Q3. Analyst vs Scientist jobs
install.packages("sqldf")
require(sqldf)
aVSs <- sqldf("SELECT case when upper(title) like '%ANALYST%' then 'Analyst' else 'Scientist' end
      , count(*) from dsJobsFinal group by case when upper(title) like '%ANALYST%' then 'Analyst' else 'Scientist' end")
aVSs
names(aVSs) <- c("title", "count")
barplot(aVSs$count, main="Analyst vs Scientist Jobs", ylab = "Titles", xlab = "No of jobs"
        , names.arg = aVSs$title, col="darkmagenta", horiz = T)

# Q4. What is the most commonly used word in job postings ?
# install.packages("tm")
require("tm")
myCorpus <- Corpus(VectorSource(dsJobsFinal$summary))
my_tdm <- TermDocumentMatrix(myCorpus, control = list(removePunctuation = TRUE, stopwords=TRUE))
m <- as.matrix(my_tdm)
words <- sort(rowSums(m), decreasing = T)
my_data <-data.frame(word = names(words), freq = words)
View(my_data)
# install.packages("wordcloud")
require(wordcloud)
wordcloud(words = my_data$word, freq = my_data$freq, min.freq = 2, max.words = 145, random.order = F
          , rot.per = 0.35, colors = brewer.pal(8, "Paired"))

# Q5. Most popular skillsets required for DS job ?
print(arrange(results$running, desc(count)))
require(ggplot2)
ggplot(results$running, aes(reorder(skill,-count), count)) + geom_bar(stat="identity") +
  labs(x = 'Skill', y = 'Occurrences (%)', title = "Skills required")
