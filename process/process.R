library(ogbox)
library(magrittr)
library(stringr)

system('svn checkout https://github.com/thebombzen/grimoire/trunk/_posts')
system('mv _posts data-raw/_posts')

allFiles = list.files('data-raw/_posts//',full.names = TRUE,recursive = TRUE)

spellText = allFiles %>% lapply(readLines) %>% lapply(paste,collapse='\n')
names(spellText) = spellText %>% sapply(function(x){
  x %>% str_extract('(?<=title:\\s).*?(?=\n)') %>% str_trim %>% str_replace_all('"',"")
})


spellParse = function(text){
  spell = list()



}
