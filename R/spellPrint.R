#' @export
print.spell = function(spell){
    cat(spell$text)
    cat('\n')
    cat(spell$dice)
    cat('\n')
    if(length(spell$dice) > 0){
        results = spell$dice %>% sapply(ogbox::roll)
        print(results)
    }
}

#' @export
filterSpells = function(sources =  NULL,
                        range = NULL,
                        components = NULL,
                        castingTime = NULL,
                        level = NULL,
                        school = NULL){

}
