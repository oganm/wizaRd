#' @export
spellCardsMP = function(spells, file, class = ''){
    nonSRD = FALSE
    card_file =  system.file('spell_cardsMP.pdf',package = 'wizaRd')
    fields = staplr::get_fields(card_file)

    spell_pages = cut(seq_along(spells),
                      breaks =
                          seq(0,length(spells),8)) %>%
        {.=as.character(.);.[is.na(.)] = 'last';.;factor(.,levels = unique(.))}

    spell_pages = split(spells,spell_pages)

    tmpdir = tempfile()
    dir.create(tmpdir)

    for(p in seq_along(spell_pages)){
        spell_page = spell_pages[[p]]
        spell_fields = fields
        for(i in seq_along(spell_page)){
            spell = spell_page[[i]]
            spell_fields[[paste0('Name',i)]]$value = spell$name
            spell_fields[[paste0('Class',i)]]$value = class
            if(!is.null(spell$source) && any(grepl("SRD",spell$source)) || nonSRD){
                spell_fields[[paste0("Level",i)]]$value = spell$level
                spell_fields[[paste0("School",i)]]$value = spell$school
                spell_fields[[paste0("Material",i)]]$value = spell$material
                spell_fields[[paste0("Casting",i)]]$value = spell$castingTime
                if(is.na(spell$aoe)){
                    spell_fields[[paste0("Range",i)]]$value = spell$range
                }else{
                    spell_fields[[paste0("Range",i)]]$value = paste(spell$range,spell$aoe)
                }
                spell_fields[[paste0("Duration",i)]]$value = paste(spell$duration,collapse= ' ')
                spell_fields[[paste0("Body",i)]]$value =
                    stringr::str_replace(spell$text,pattern = '^(.|\n)*?Duration.*?\n\n','') %>%
                    stringr::str_replace_all('\u{2022}','-') %>%
                    stringr::str_replace_all('\u{00BD}','1/2') %>%
                    stringr::str_replace_all('\u{00BE}','3/4') %>%
                    stringr::str_replace_all('(\u{00BC})','1/4') %>%
                    stringr::str_replace_all('(\u{00BC})','1/4') %>%
                    stringr::str_replace_all('(\u{2018})',"'") %>%
                    stringr::str_replace_all('(\u{2018})',"'") %>%
                    stringr::str_replace_all('(\u{FE58})',"-") %>%
                    stringr::str_replace_all('(\u{201C})','"') %>%
                    stringr::str_replace_all('(\u{201D})','"') %>%
                    stringr::str_replace_all('(\u{201D})','->')
                spell_fields[[paste0("Page",i)]]$value = paste(spell$source,collapse =' ')
                spell_fields[[paste0("Comp",i*3-2)]]$value = "V" %in%
                    spell$components %>% as.character %>%
                        switch('TRUE' = 'Yes','FALSE' = 'Off')
                spell_fields[[paste0("Comp",i*3-1)]]$value = "S" %in%
                    spell$components %>% as.character %>%
                    switch('TRUE' = 'Yes','FALSE' = 'Off')
                spell_fields[[paste0("Comp",i*3)]]$value = "M" %in%
                    spell$components %>% as.character %>%
                    switch('TRUE' = 'Yes','FALSE' = 'Off')
                }
            }

        set_fields_fix(card_file,output_filepath = file.path(tmpdir,paste0(p,'.pdf')),fields = spell_fields)

    }

    staplr::staple_pdf(input_files = paste0(tmpdir,'/',seq_along(spell_pages),'.pdf'),output_filepath = file)

}


spellCards =  function(spells, file, nonSRD = TRUE){
    card_file =  system.file('spell_cards.pdf',package = 'wizaRd')
    fields = staplr::get_fields(card_file)

    spell_pages = cut(seq_along(spells),
                      breaks =
                          seq(0,length(spells),8)) %>%
        {.=as.character(.);.[is.na(.)] = 'last';.;factor(.,levels = unique(.))}

    spell_pages = split(spells,spell_pages)

    tmpdir = tempfile()
    dir.create(tmpdir)

    for(p in seq_along(spell_pages)){
        spell_page = spell_pages[[p]]
        spell_fields = fields
        for(i in seq_along(spell_page)){
            spell = spell_page[[i]]
            spell_fields[[paste0('Spell Title ',i)]]$value = spell$name
            spell_fields[[paste0("Spell Level ",i)]]$value =
                switch(as.character(spell$level),
                       "0" = 'CANTRIP',
                       "1" = "1ST-LEVEL",
                       "2" = "2ND-LEVEL",
                       "3" = "3RD-LEVEL",
                       "4" = "4TH-LEVEL",
                       "5" = "5TH-LEVEL",
                       "6" = "6TH-LEVEL",
                       "7" = "7TH-LEVEL",
                       "8" = "8TH-LEVEL",
                       "9" = "9TH-LEVEL")

            spell_fields[[paste0("spell type ",i)]]$value = toupper(spell$school)

            componentOptions = spell_fields[[paste0("components ",i)]]$value %>% levels %>% trimws %>% stringr::str_replace("\\*",'') %>% stringr::str_split(', ')

            spell_fields[[paste0("components ",i)]]$value = componentOptions %>%
                sapply(function(x){all(x %in% spell$components) & all(spell$components %in% x)}) %>%
                {levels(spell_fields[[paste0("components ",i)]]$value)[.]}

            spell_fields[[paste0("CASTING TIME_",i)]]$value = spell$castingTime
            if(is.na(spell$aoe)){
                spell_fields[[paste0("RANGE_",i)]]$value = spell$range
            }else{
                spell_fields[[paste0("RANGE_",i)]]$value = paste(spell$range,spell$aoe)
            }
            spell_fields[[paste0("DURATION_",i)]]$value = paste(spell$duration,collapse= ' ')

            spellBody =
                stringr::str_replace(spell$text,pattern = '^(.|\n)*?Duration.*?\n\n','')

            if(!is.na(spell$material)){
                spellBody = paste0("Materials: ",spell$material,'\n\n',spellBody)
            }
            if(!is.null(spell$source) && any(grepl("SRD",spell$source)) || nonSRD){
                spellBody %>%
                    stringr::str_replace_all('\u{2022}','-') %>%
                    stringr::str_replace_all('\u{00BD}','1/2') %>%
                    stringr::str_replace_all('\u{00BE}','3/4') %>%
                    stringr::str_replace_all('(\u{00BC})','1/4') %>%
                    stringr::str_replace_all('(\u{00BC})','1/4') %>%
                    stringr::str_replace_all('(\u{2018})',"'") %>%
                    stringr::str_replace_all('(\u{2018})',"'") %>%
                    stringr::str_replace_all('(\u{FE58})',"-") %>%
                    stringr::str_replace_all('(\u{201C})','"') %>%
                    stringr::str_replace_all('(\u{201D})','"') %>%
                    stringr::str_replace_all('(\u{201D})','->')


                spell_fields[[paste0("Spell description ",i)]]$value = spellBody
            }
        }

        set_fields_fix(card_file,output_filepath = file.path(tmpdir,paste0(p,'.pdf')),fields = spell_fields)

    }

    staplr::staple_pdf(input_files = paste0(tmpdir,'/',seq_along(spell_pages),'.pdf'),output_filepath = file)

}



set_fields_fix = function (input_filepath = NULL, output_filepath = NULL, fields,
                           overwrite = TRUE)
{
    assertthat::assert_that(is.list(fields))
    if (is.null(input_filepath)) {
        input_filepath <- file.choose(new = FALSE)
    }
    if (is.null(output_filepath)) {
        output_filepath <- tcltk::tclvalue(tcltk::tkgetSaveFile(filetypes = "{Pdf {.pdf}}"))
    }
    input_filepath <- normalizePath(input_filepath, mustWork = TRUE)
    output_filepath <- normalizePath(output_filepath, mustWork = FALSE)
    fdfLines <- staplr:::get_fdf_lines(input_filepath)
    annotatedFDF = staplr:::fdfAnnotate(fdfLines)
    for (i in seq_along(fields)) {
        fieldToFill <- fields[[i]]
        annotatedFDF <- staplr:::fdfEdit(fieldToFill, annotatedFDF)
    }
    newFDF <- tempfile()
    f = file(newFDF, open = "w")
    writeLines(paste0(annotatedFDF$fdfLines, collapse = "\n"),
               f)
    close(f)
    system_command <- paste("pdftk", shQuote(input_filepath),
                            "fill_form", shQuote(newFDF), "output", "{shQuote(output_filepath)}")
    staplr:::fileIO(input_filepath = input_filepath, output_filepath = output_filepath,
                    overwrite = overwrite, system_command = system_command)
}
