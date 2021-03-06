#' @export
filterSpells = function(spells = spells,
                        class = wizaRd::spells %>% purrr::map('classes') %>% unlist %>% unique,
                        level = NULL,
                        sources =  wizaRd::spells %>% purrr::map('source') %>%
                            purrr::map(stringr::str_replace,'\\..*?$','') %>%
                            unlist %>%
                            unique,
                        # range = NULL,
                        components = NULL,
                        # castingTime = NULL,
                        school = wizaRd::spells %>%
                            purrr::map('school') %>%
                            unlist %>%
                            unique)
    {
    Sclass = spells %>% purrr::map('classes')
    Slevels = spells %>% purrr::map_int('level')
    Ssources = spells %>% purrr::map('source') %>% purrr::map(stringr::str_replace,'\\..*?$','')
    Srange = spells %>% purrr::map_chr('range') %>%
        {.[grepl('Self',.)] = 'Self';.} %>%
        stringr::str_replace(' feet', '') %>%
        {.[grepl('mile',.)] %<>% stringr::str_replace('(\\smile(s|$))','') %>% as.integer %>%
                magrittr::multiply_by(5280);.}

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
        out = out &(Ssources %>% purrr::map(`%in%`,sources) %>% purrr::map_lgl(any))
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
leveledList = function(spells){
    0:9 %>% lapply(function(i){
        spells %>% filterSpells(level = i)
    }) -> out
    names(out) = 0:9
    out = out[sapply(out,length)>0]
    return(out)
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
