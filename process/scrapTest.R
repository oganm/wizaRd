library(rvest)
library(glue)
library(magrittr)
library(stringr)
library(readr)

spellsPage1 = read_html('https://www.dndbeyond.com/spells?filter-class=0&filter-partnered-content=t&filter-search=&page=1')

pageCount = spellsPage1 %>% html_nodes('.b-pagination-item') %>% html_text() %>% as.integer() %>% na.omit() %>% max

allSpells = seq_len(pageCount) %>% lapply(function(page){
    spellsPage = read_html(glue('https://www.dndbeyond.com/spells?filter-class=0&filter-partnered-content=t&filter-search=&page={page}'))
    pageSpells = spellsPage %>% rvest::html_nodes('.info')

    pageSpells %<>% lapply(function(spell){
        name = spell %>%
            rvest::html_node('.spell-name') %>%
            rvest::html_node('.name') %>% rvest::html_node('.link') %>%
            rvest::html_text() %>% trimws()
        print(name)
        level = spell %>% html_node('.spell-level') %>% html_text() %>% trimws()
        school = spell %>% html_node('.spell-name') %>% html_children() %>%
            {.[[2]]} %>% html_children() %>% {.[1]} %>% html_text() %>% trimws()
        components =  spell %>% html_node('.spell-name') %>% html_children() %>%
        {.[[2]]} %>% html_children() %>% {.[3]} %>% html_text() %>% trimws()

        castingTime = spell %>% html_node('.spell-cast-time') %>% html_text() %>% trimws()

        duration = spell %>% html_node('.spell-duration') %>% html_text() %>% trimws()
        range = spell %>% html_node('.range-distance') %>% html_text() %>% trimws()
        if(is.na(range)){
            range = spell %>% html_node('.spell-range') %>% html_text() %>% stringr::str_replace('\\(.*?\\)','') %>%  trimws()
        }

        aoeTypes = c(square = '.i-aoe-square',
                     cube = '.i-aoe-cube',
                     cone = '.i-aoe-cone',
                     sphere = '.i-aoe-sphere',
                     cylinder = '.i-aoe-cylinder',
                     line = '.i-aoe-line')

        aoe_html = spell %>% html_node('.aoe-size')
        if(length(aoe_html) == 0){
            aoe = NA
        } else{
            aoe = aoeTypes %>% sapply(function(type){
                aoeType = aoe_html %>% html_node(type)
                if(length(aoeType)>0){
                    out = aoe_html %>% html_text()
                    return(out)

                }
                return(NULL)

            }) %>% {.[!sapply(.,is.null)]} %>% unlist %>%
                {stringr::str_replace(.,'\\)',glue(names(.),')'))}

            if(length(aoe) == 0){
                aoe = aoe_html %>% html_text() %>% trimws()
            }
        }


        attackSave =  spell %>% html_node('.spell-attack-save') %>% html_text() %>% trimws()

        damageEffect =  spell %>% html_node('.spell-damage-effect') %>% html_text() %>% trimws()

        out = list(name = name %>% str_replace_all("’","'"),
                   level = level,
                   school = school,
                   components = components,
                   castingTime = castingTime,
                   duration = duration,
                   range = range,
                   aoe = aoe,
                   attackSave = attackSave,
                   damageEffect = damageEffect)
        return(out)
    })
    names(pageSpells) = pageSpells %>% purrr::map_chr('name')
    return(pageSpells)

})
allSpells = do.call(c,allSpells)
saveRDS(allSpells,'dndbeyond.rds')
allSpells %>% jsonlite::toJSON(pretty=TRUE) %>% writeLines('data-raw/spellDetails.json')

devtools::load_all()

names(spells)[!tolower(names(spells)) %in% tolower(names(allSpells))]

missingBeyondSpells = names(allSpells)[!tolower(names(allSpells)) %in% tolower(names(spells))]

splitSpells = strsplit(tolower(names(spells)),' ')

missingBeyondSpells %>% strsplit(' ') %>%
    sapply(function(x){
        splitSpells %>% sapply(function(y){
            all(tolower(x) %in% y)
        }) %>% any
    }) %>% {missingBeyondSpells[!.]} -> trulyMissingBeyondSpells
