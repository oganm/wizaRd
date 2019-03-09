library(ogbox)
library(magrittr)
library(stringr)
library(XML)
library(purrr)

unlink('data-raw/_posts/',recursive = TRUE)
system('svn checkout https://github.com/thebombzen/grimoire/trunk/_posts')
system('mv _posts data-raw/_posts')
download.file('https://www.dropbox.com/s/4f7zdx09nkfa9as/Core.xml?dl=1',destfile = 'data-raw/Core.xml')
allRules = xmlParse('data-raw/Core.xml') %>% xmlToList()
saveRDS(allRules,'data-raw/allRules.rds')
allRules = readRDS('data-raw/allRules.rds')
fightClubSpells = allRules[names(allRules) == 'spell']
names(fightClubSpells) = fightClubSpells %>% purrr::map_chr('name')

allFiles = list.files('data-raw/_posts//',full.names = TRUE,recursive = TRUE)

spellText = allFiles %>% lapply(readLines) %>% lapply(paste,collapse='\n')
names(spellText) = spellText %>% sapply(function(x){
    x %>% str_extract('(?<=title:\\s).*?(?=\n)') %>% str_trim %>% str_replace_all('"',"")
})

classNames =c('bard',
              'cleric',
              'druid',
              'paladin',
              'ranger',
              'sorcerer',
              'warlock',
              'wizard')

schools = c('abjuration',
            'conjuration',
            'divination',
            'enchantment',
            'evocation',
            'illusion',
            'necromancy',
            'transmutation')

components = c('V','S','M')

spellParse = function(text){
    spell = list()
    spell$text= text %>% str_extract('(?<=-\\n\n)(.|\\n)*?$')
    spell$name = text %>% str_extract('(?<=title:\\s).*?(?=\n)') %>% str_trim %>% str_replace_all('"',"") %>% str_replace_all("’","'")
    spell$source = text %>% str_extract('(?<=sources:\\s).*?(?=\n)') %>% str_trim %>% str_replace_all('"|\\[|\\]',"") %>% str_split(',') %>%{.[[1]]} %>% str_trim
    spell$tags =  text %>% str_extract('(?<=tags:\\s).*?(?=\n)') %>% str_trim %>% str_replace_all('"|\\[|\\]',"") %>% str_split(',') %>%{.[[1]]} %>% str_trim
    spell$range = text %>% str_extract('(?<=Range\\*\\*:\\s).*?(?=\n)')
    spell$castingTime = text %>% str_extract('(?<=Casting\\sTime\\*\\*:\\s).*?(?=\n)')
    spell$components = text %>% str_extract('(?<=Components\\*\\*:\\s).*?(?=\n)') %>%
        str_extract_all(ogbox::regexMerge(components)) %>%{.[[1]]}
    spell$duration = text %>% str_extract('(?<=Duration\\*\\*:\\s).*?(?=\n)') %>% str_split(',') %>%{.[[1]]}
    spell$ritual = 'ritual' %in% spell$tags
    spell$classes  = spell$tags[grepl(ogbox::regexMerge(classNames),spell$tags)]
    spell$school= spell$tags[grepl(ogbox::regexMerge(schools),spell$tags)]
    spell$level= suppressWarnings(spell$tags %>% paste(collapse='\n') %>% str_extract('((?<=level)[0-9])|(cantrip)') %>% as.integer)
    if(is.na(spell$level)){
        spell$level = 0 %>% as.integer
    }

    fightClubRoll = fightClubSpells[[which(tolower(names(fightClubSpells)) %in% tolower(spell$name))[1]]]
    fightClubRoll = fightClubRoll[names(fightClubRoll) %in% 'roll'] %>% unlist

    textRoll = text %>% str_extract_all('[0-9]+?d[0-9]+') %>% {.[[1]]} %>% unique

    textRollNoBeginning = text %>% str_extract_all('(?<= )d[0-9]+') %>% {.[[1]]}
    if(length(textRollNoBeginning)>0){
        textRoll = c(textRoll,paste0(1,textRollNoBeginning))
    }
    if(!is.null(fightClubRoll)){
        spell$dice = fightClubRoll %>%
            str_split('(\\+(?=[0-9]*?d))|(\\)\\+\\()') %>% unlist %>% gsub('\\(|\\)','',.) %>% unique
    } else{
        spell$dice = textRoll
        if(length(spell$dice)>0){
            print(spell$name)
        }
    }

    # spell$dice = text %>% str_extract_all('[0-9]+?d[0-9]+') %>% {.[[1]]}

    class(spell) = append(class(spell), 'spell')
    return(spell)
}
spells = spellText %>% lapply(spellParse)

class(spells) = append(class(spells),'spellList')

names(spells) = spells %>% map_chr('name')

# fix for immolation
spells$Immolation$dice = c('8d6','3d6')

devtools::use_data(spells,overwrite=TRUE)
spells %>% jsonlite::toJSON(pretty=TRUE) %>% writeLines('data-raw/spells.json')

spells %>% map('dice') %>% jsonlite::toJSON(pretty = TRUE) %>% writeLines('data-raw/spelldice.json')
