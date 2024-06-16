

correct_master_pw <<- "This is my master password"

# Generate random string for passwords
create_pw <<- function(nchars, seed) {
  
  special_chars <- c("!", "#", "...")

  set.seed(as.integer(seed))
  pw_vector <- sample(c(LETTERS, letters, c(0:9), special_chars), size = nchars)
  
  pw <- paste0(pw_vector, collapse = "")
  
  return(pw)
}

