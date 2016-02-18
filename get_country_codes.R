######################################################################
#
# Add Country Codes to list of input country names using countrycodes_data
#
# TODO
# + countrycode_data remains in enviroment
# + Implement some Unit testing
# + Transform for-loop-solution to a more vectorized one
#
######################################################################
require(dplyr)
require(magrittr)
require(countrycode)

data(countrycode_data)

match_country_names = function(clist, ret_codes=c("iso3c"), reformat=TRUE, keep_orig=FALSE)
{
  # Reformat input country names if badly formatted
  clist_f = gsub("([a-z])([A-Z])","\\1 \\2", clist, perl=T) # Add \s between lcase / ucase
  clist_f = gsub("(and)(the)"," \\1 \\2 ", clist_f, perl=T) # Add \s between .* "and" "the" .*
  clist_f = gsub("(?<!Falkl)(and|of)\\s"," \\1 \\2 ", clist_f, perl = T) # Add \s between .* "and" "the" .*
  clist_f = gsub("\\s\\s"," ", clist_f) # Remove double whitespaces
  clist_f_lower = clist_f %>% tolower() # Lowercase for caseinvariant comparison (baseR::match() does not support ignoreCase-option...)

  # Load data from countrycode package
  countrycode_data = countrycode_data %>% mutate(country.name = tolower(country.name))

  # Try direct match first
  ret = data.frame(original = clist,
                   corrected = clist_f,
                   corrected_lower = clist_f_lower,
                   stringsAsFactors=FALSE)

  # Filter for country codes not supplied by countrycode_data
  ret_codes = ret_codes[which(ret_codes %in% colnames(countrycode_data %>% select(-country.name, -regex, -continent, -region)))]

  # Append country code columns to returned data frame
  ret = cbind(ret,
              setNames(as.data.frame(matrix(ncol = length(ret_codes), nrow = length(clist))), ret_codes))

  # Start matching input country names with codes
  for (i in 1:length(ret_codes)){
    codename = ret_codes[i]
    # Try matching directly via name
    ret[[codename]] =
      countrycode_data[[codename]][match(clist_f_lower, countrycode_data$country.name, nomatch = NA)]

    # Try matching failed directs through regex entries within countrycode_data, return list if multiple matches occur
    for (j in 1:nrow(ret)) {
      match_results = NULL
      if (is.na(ret[[codename]][j])){
        for (k in 1:length(countrycode_data$regex)) {
          match = grepl(countrycode_data$regex[k], ret$corrected_lower[j], perl = TRUE)
          if (match == TRUE && !is.na(match)) {
            match_results = c(match_results, countrycode_data$iso3c[k])
          }
        }
        if (!is.null(match_results))
          ret[[codename]][j] = list(match_results)
      }
    }
  }

  # Format return data frame
  ret = ret %>% select(-corrected_lower)
  if (!reformat) ret = ret %>% select(-corrected)
  if (!keep_orig) ret = ret %>% select(-original)

  return(ret)
}
