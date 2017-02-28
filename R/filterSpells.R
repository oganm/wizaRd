#' @export
filterSpells = function(spells = spells,
                        class = c("sorcerer", "wizard", "ranger", "bard", "cleric (nature)",
                                  "druid", "warlock", "cleric", "paladin (vengeance)", "paladin",
                                  "cleric (light)", "warlock (fiend)", "cleric (trickery)", "warlock (great old one)",
                                  "cleric (war)", "paladin (ancients)", "warlock (fae)", "cleric (death)",
                                  "warlock (undying)", "cleric (tempest)", "paladin (oathbreaker)",
                                  "cleric (knowledge)", "cleric (arcana)", "paladin (devotion)",
                                  "druid (desert)", "paladin (crown)", "druid (underdark)", "druid (grassland)",
                                  "druid (arctic)", "druid (swamp)", "druid (forest)", "druid (mountain)",
                                  "druid (coast)"),
                        level = NULL,
                        sources =  c("PHB", "EE", "SCAG"),
                        # range = NULL,
                        components = NULL,
                        # castingTime = NULL,
                        school = c("conjuration", "abjuration", "enchantment", "evocation", "necromancy",
                                   "illusion", "divination", "transmutation")){
    Sclass = spells %>% purrr::map('classes')
    Slevels = spells %>% purrr::map_int('level')
    Ssources = spells %>% purrr::map_chr('source') %>% stringr::str_replace('\\..*?$','')
    Srange = spells %>% purrr::map_chr('range') %>%
        {.[grepl('Self',.)] = 'Self';.} %>%
        str_replace(' feet', '') %>%
        {.[grepl('mile',.)] %<>% str_replace('(\\smile(s|$))','') %>% as.integer %>% multiply_by(5280);.}

    Scomponents = spells %>% purrr::map('components')

    ScastingTime = spells %>% purrr::map_chr('castingTime')

    Sschool = spells %>% purrr::map_chr('school')

    out = rep(TRUE,length(spells))
    if(!is.null(class)){
        out = out & (Sclass %>% sapply(function(x){any(x %in% class)}))
    }
    if(!is.null(level)){
        out = out & (Slevels %in% level)
    }
    if(!is.null(sources)){
        out = out &(Ssources %in% sources)
    }
    if(!is.null(components)){
        out = out & (Scomponents %>% sapply(function(x){sort(components) == sort(x)}))
    }

    if(!is.null(school)){
        out = out &(Sschool %in% school)
    }

    outSpells = spells[out]
    return(outSpells)
}

#' @export
makeBook = function(level, extras = NULL,spells = wizaRd::spells, takeMax = TRUE){

    validSpells = spells

    out = list()
    class(out) = c('list','spellList')
    for(i in 1:level){
        if(i == 1){
            n = 6
        } else{
            n=2
        }
        maxLevel = (i/2) %>% ceiling
        if(maxLevel>9){maxLevel=9}

        if(takeMax){
            out = c(out,
                    validSpells %>% filterSpells(level = maxLevel,class = "wizard") %>% sample(n))
        } else{
            out = c(out,
                    validSpells %>% filterSpells(level = 1:maxLevel,class = "wizard") %>% sample(n))
        }
        validSpells = spells[!names(spells) %in% names(out)]
    }

    if(!is.null(extras)){
        extras = 1:length(extras) %>% sapply(function(i){
            validSpells %>% filterSpells(level=names(extras)[i] %>% as.integer) %>% sample(extras[i])})
        extras %<>% unlist(recursive = FALSE)
        out = c(out,extras)
    }

    return(out)

}
