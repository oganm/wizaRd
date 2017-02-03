#' @export
print.spell = function(spell){
  cat(spell$text)
  cat('\n')
  cat(spell$dice)
  cat('\n')
  results = spell$dice %>% sapply(ogbox::roll)
  print(results)
}
