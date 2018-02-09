# An alternative approach is to look at pmap

df <- data.frame(
  x = c("marine", "banana", "ocean"),
  pattern = c("marine", "n", "ocean"),
  replacement = c("|marine", "f", "|ocean"),
  stringsAsFactors = FALSE
  )

df1 <- data.frame(
  x = c("marine", "banana", "ocean"),
  pattern = c("marine", "marine", "marine"),
  stringsAsFactors = FALSE
)

# have to take out replacement as grepl only accepts pattern arg. Produces a vector of the same length that can use bind_cols on

df1 %>% pmap_chr(., grepl)

# so how would the above work in practice. It would imply that I would have to add the terms as a colum called pattern and do one at a time.

sentences_country %>%
  add_column(pattern = "marine") %>%
  select(word, pattern) %>%
  rename(x = word) %>%
  pmap_chr(grepl)

The above works ok and we could bind cols but the question is how do I do it with multiple terms and create columns with the contents?

# pmap_chr returns a character vector rather than a list and so should bind where the column is named
pmap_chr(df, gsub) %>%
  tibble(replaced = .) %>%
  bind_cols(df)

#One option here is to come up with a list of replacements for the code but I dont think it will work in cases where there is more than one entry in a sentence



pmap(df, str_replace_all)

# this works because grepl takes a pattern argument