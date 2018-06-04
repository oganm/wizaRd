#' @export
print.spell = function(spell){
    cat(spell$text)
    cat('\n')
    cat(spell$dice)
    cat('\n')
    if(length(spell$dice) > 0){
        results = spell$dice %>% gsub('SPELL|PROF|\\+SPELL|\\+PROF','',.) %>% sapply(diceSyntax::roll)
        print(results)
    }
}


#' @export
"[.spellList" = function(x,i){
    class(x) = 'list'
    x = x[i]
    class(x) = append(class(x),'spellList')
    return(x)
}

#' @export
"c.spellList" = function(x,...){
    class(x) = 'list'
    x = c(x,...)
    class(x) = append(class(x),'spellList')
    return(x)
}

#'@export
cSpells = function(...){
    x = list(...)
    names(x) = x %>% purrr::map_chr('name')
    class(x) = append(class(x),'spellList')
    return(x)
}

#' @export
print.spellList = function(spells){
    if(length(spells)==0){
        invisible(return(NULL))
    }
    for(i in 0:9){
        levelSpells = spells %>% filterSpells(level=i)
        if(length(levelSpells)==0){
            next
        } else{
            if(i == 0){
                cat('Cantrips\n')
                cat("========\n")

            } else{
                cat(paste0("\nLevel ",i,'\n'))
                cat("=======\n")
            }
            cat(names(levelSpells),sep='\n')
        }
    }
}
