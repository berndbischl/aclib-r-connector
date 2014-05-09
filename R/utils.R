removeEmptyLines = function(lines) {
  Filter(function(x) str_trim(x) != "", lines)
}

trimLines = function(lines) {
  str_trim(lines)
}

trimAndRemoveEmptyLines = function(lines) {
  removeEmptyLines(trimLines(lines))
}


splitAndTrim = function(x, s, n = Inf, convert = as.character) {
  x = str_split(x, s, n = n)[[1]]
  x = str_trim(x)
  convert(x)
}

removeChars = function(x, chars) {
  for (ch in chars)
    x = str_replace_all(x, ch, "")
  return(x)
}

consume = function(s, regexp) {
  e = str_extract(s, regexp)
  r = str_split(s, e)[[1L]][1L]
  list(match = e, rest = r)
}


