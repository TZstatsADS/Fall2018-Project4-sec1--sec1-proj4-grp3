#load neccesary packages
library('tidyverse')
library('stringi')
library('readtext')

#read in data
data_truth <- readtext('../data/ground_truth/*.txt', encoding='utf-8')
data_tesseract <- readtext('../data/tesseract/*.txt', encoding='utf-8')

#create list of character bigrams frequencies from ground_truth data
char_bigrams <- data_truth %>%
  unnest_tokens(token, text, token = "character_shingles", n = 2) %>%
  group_by(token) %>%
  summarise(frequency = (n())/1515) #1515 is number of unique tokens in ground_truth corpus

#tokenize tesseract data
tokens <- data_tesseract %>%
  unnest_tokens(token, text)

#compute bigramFreq feature using ground truth character bigram frequency
bigramScore <- tokens %>%
  mutate(token_id = token) %>%
  unnest_tokens(char, token, token = "character_shingles", n = 2) %>%
  inner_join(char_bigrams, by = c('char' = 'token')) %>%
  group_by(token_id) %>%
  mutate(n = n(), termFreq = sum(frequency), bigramFreq = (termFreq/n)) %>%
  select(doc_id, token_id, bigramFreq) %>%
  distinct()

#compute all features and save in new df
feature_set <- tokens %>%
  rowwise() %>% 
  mutate(len = nchar(token), 
         nvowels = str_count(tolower(token), "[aeoiu]"), 
         ncons = str_count(tolower(token), "[bcdfghjklmnpqrstvxzwy]"),
         vowelRatio = nvowels/len, 
         consRatio = ncons/len, 
         vowelConsRatio = ifelse(ncons > 0, nvowels/ncons, 0), 
         specialChar = str_count(token, "[^\\w\\d\\s:]"), 
         specialRatio = specialChar/len, 
         ndigits = str_count(token, "\\d"), 
         digitsRatio = ndigits/len,
         upperCase = str_count(token, "[A-Z]"), 
         lowerCase = str_count(token, "[a-z]"), 
         upperRatio = upperCase/len, 
         lowerRatio = lowerCase/len,
         seqRatio = ifelse(str_count(token, "(.)\\1{2,}") > 0, ((nchar(str_extract(token, "(.)\\1{2,}")))/len),0),
         majoritySpecial = ifelse(specialChar > floor(len/2), 1,0), 
         seqCons = ifelse(str_count(token, "(([b-df-hj-np-tv-z])(?!\2)){6}") > 0, 1, 0), #checks if more than 6 constants occur concurrently
         innerSpecial = ifelse(str_count(gsub(".$", "", gsub("^.", "", token)), "[^\\w\\d\\s:]") > 1, 1, 0), #after removing first and last characters, checks if more than 1 special characters remain
         interFreq = max(table(strsplit(token,"")))[1], mostFreq = ifelse(interFreq > 2, interFreq/len, 0), 
         countNonAlpha = (len -(nvowels + ncons))/(nvowels + ncons)) %>%
  left_join(bigramScore, by = c('token' = 'token_id')) %>%
  select(-interFreq, -doc_id.y) %>%
  distinct()

#create rough list of incorrectly OCR'ed terms
tokens_truth <- data_truth %>%
  select(-doc_id) %>%
  unnest_tokens(token, text) %>%
  distinct()

bad_terms <- subset(tokens$token, !(tokens$token %in% tokens_truth))
write.csv(bad_terms, 'bad_terms.csv', row.names = F)
write.csv(feature_set, "feature_dataset.csv", row.names = F)                    
