#FIXME: checkmat

# check that path is a file
checkFile = function(path) {
  checkArg(path, "character", len = 1L, na.ok = FALSE)
  if (!file.exists(path))
    stopf("File not found: %s", path)
}

# check that path is a dir
checkDir = function(path) {
  checkArg(path, "character", len = 1L, na.ok = FALSE)
  if (!file.exists(path)) 
    stopf("Directory not found: %s", path)
  if(!isDirectory(path))
    stopf("Path is not a directory: %s", path)
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


# splits a string, then trims the parts, then possibly converts the data type
splitAndTrim = function(x, s, n = Inf, convert = as.character) {
  checkArg(x, "character", len = 1L, na.ok = FALSE)
  checkArg(s, "character", len = 1L, na.ok = FALSE)
  checkArg(convert, "function")
  x = str_split(x, s, n = n)[[1]]
  x = str_trim(x)
  convert(x)
}

# remove comments starting with '#' from lines
removeComments = function(lines) {
  lines = str_replace_all(lines, "#.*$", "")
  return(lines)
}

# removes chars from a string
removeChars = function(x, chars) {
  for (ch in chars)
    x = str_replace_all(x, ch, "")
  return(x)
}

# extract part from a string that matches a regexp, and also retuns the remaining rest as a string
consume = function(s, regexp) {
  checkArg(s, "character", len = 1L, na.ok = FALSE)
  checkArg(regexp, "character", len = 1L, na.ok = FALSE)
  e = str_extract(s, regexp)
  r = str_split(s, e)[[1L]][2L]
  list(match = e, rest = r)
}


