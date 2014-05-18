aAClibScenario = function(x) {
  aobj(x, "AClibScenario")
}


# removes whitespace lines from a char vec of lines
removeEmptyLines = function(lines) {
  Filter(function(x) str_trim(x) != "", lines)
}

# trims a char vec of lines
trimLines = function(lines) {
  str_trim(lines)
}

# trims a char vec of lines, then removes the empty ones
trimAndRemoveEmptyLines = function(lines) {
  removeEmptyLines(trimLines(lines))
}

# check, read, trim, remove empty lines
readTxtTrimAndRemove = function(path) {
  afile(path)
  lines = readLines(path, warn = FALSE)
  removeEmptyLines(trimLines(lines))
}

# splits a string, then trims the parts, then possibly converts the data type
splitAndTrim = function(x, s, n = Inf, convert = as.character) {
  astring(x)
  astring(s)
  afun(convert)
  x = str_split(x, s, n = n)[[1]]
  x = str_trim(x)
  convert(x)
}

# remove comments starting with # from char vec lines
removeComments = function(lines) {
  lines = str_replace_all(lines, "#.*$", "")
  trimAndRemoveEmptyLines(lines)
}

# removes chars from a string
removeChars = function(x, chars) {
  for (ch in chars)
    x = str_replace_all(x, ch, "")
  return(x)
}

# extract part from a string that matches a regexp, and also retuns the remaining rest as a string
consume = function(s, regexp) {
  astring(s)
  astring(regexp)
  loc = str_locate(s, regexp)[1L, ]
  e = substr(s, loc[1L], loc[2L])
  r = paste0(str_sub(s, 1, loc[1L] - 1L), str_sub(s, loc[2L] + 1L, str_length(s)))
  list(match = e, rest = r)
}


