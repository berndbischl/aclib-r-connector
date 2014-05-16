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

# check, read, trim, remove empty lines
readTxtTrimAndRemove = function(path) {
  checkFile(path)
  lines = readLines(path, warn = FALSE)
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
  checkArg(s, "character", len = 1L, na.ok = FALSE)
  checkArg(regexp, "character", len = 1L, na.ok = FALSE)
  loc = str_locate(s, regexp)[1L, ]
  e = substr(s, loc[1L], loc[2L])
  r = paste0(str_sub(s, 1, loc[1L] - 1L), str_sub(s, loc[2L] + 1L, str_length(s)))
  list(match = e, rest = r)
}


