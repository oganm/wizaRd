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
  spell$text= text %>% str_extract('(?<=-\\n\n)(.|\\n)*?$')
  spell$name = text %>% str_extract('(?<=title:\\s).*?(?=\n)') %>% str_trim %>% str_replace_all('"',"")
  spell$source = text %>% str_extract('(?<=source:\\s).*?(?=\n)') %>% str_trim %>% str_replace_all('"',"")
  spell$tags =  text %>% str_extract('(?<=tags:\\s).*?(?=\n)') %>% str_trim %>% str_replace_all('"|\\[|\\]',"") %>% str_split(',') %>%{.[[1]]} %>% str_trim
  spell$range = text %>% str_extract('(?<=Range\\*\\*:\\s).*?(?=\n)')
  spell$range = text %>% str_extract('(?<=Casting\\sTime\\*\\*:\\s).*?(?=\n)')
  spell$components = text %>% str_extract('(?<=Components\\*\\*:\\s).*?(?=\n)') %>% str_split(',') %>%{.[[1]]}
  spell$duration = text %>% str_extract('(?<=Duration\\*\\*:\\s).*?(?=\n)') %>% str_split(',') %>%{.[[1]]}

  spell$level= spell$tags %>% paste(collapse='\n') %>% str_extract('((?<=level)[0-9])|(cantrip)') %>% as.integer

  spell$dice = text %>% str_extract_all('[0-9]+?d[0-9]+') %>% {.[[1]]}

  class(spell) = append(class(spell), 'spell')
  return(spell)

}

spells = spellText %>% lapply(spellParse)

devtools::use_data(spells,overwrite=TRUE)
