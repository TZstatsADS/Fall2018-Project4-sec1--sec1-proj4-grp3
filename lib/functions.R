library('dplyr')
library('tidytext')
library('stringi')
library('stringr')

generateBigramscore <- function(data, tokenize = FALSE){
  bigramScore <- data %>%
    mutate(token_id = token) %>%
    unnest_tokens(char, token, token = "character_shingles", n = 2) %>%
    inner_join(char_bigrams, by = c('char' = 'token')) %>%
    group_by(token_id) %>%
    mutate(n = n(), termFreq = sum(frequency), bigramFreq = (termFreq/n)) %>%
    select(token_id, class, bigramFreq) %>%
    distinct()
  return(bigramScore)
}
generateFeatures <- function(token){
  feature_set <- token %>%
    rowwise() %>% 
    mutate(len = nchar(token_id), 
           nvowels = str_count(tolower(token_id), "[aeoiu]"), 
           ncons = str_count(tolower(token_id), "[bcdfghjklmnpqrstvxzwy]"),
           vowelRatio = nvowels/len, 
           consRatio = ncons/len, 
           vowelConsRatio = ifelse(ncons > 0, nvowels/ncons, 0), 
           specialChar = str_count(token_id, "[^\\w\\d\\s:]"), 
           specialRatio = specialChar/len, 
           ndigits = str_count(token_id, "\\d"), 
           digitsRatio = ndigits/len,
           upperCase = str_count(token_id, "[A-Z]"), 
           lowerCase = str_count(token_id, "[a-z]"), 
           upperRatio = upperCase/len, 
           lowerRatio = lowerCase/len,
           seqRatio = ifelse(str_count(token_id, "(.)\\1{2,}") > 0, ((nchar(str_extract(token_id, "(.)\\1{2,}")))/len),0),
           majoritySpecial = ifelse(specialChar > floor(len/2), 1,0), 
           seqCons = ifelse(str_count(token_id, "(([b-df-hj-np-tv-z])(?!\2)){6}") > 0, 1, 0), #checks if more than 6 constants occur concurrently
           innerSpecial = ifelse(str_count(gsub(".$", "", gsub("^.", "", token_id)), "[^\\w\\d\\s:]") > 1, 1, 0), #after removing first and last characters, checks if more than 1 special characters remain
           interFreq = max(table(strsplit(token_id,"")))[1], mostFreq = ifelse(interFreq > 2, interFreq/len, 0), 
           countNonAlpha = (len -(nvowels + ncons))/(nvowels + ncons)) %>%
    select(-interFreq) %>%
    distinct()
  return(feature_set)
}
