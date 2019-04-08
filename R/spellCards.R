#' @export
spellCards = function(spells, file, nonSRD = TRUE){
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

            spell_fields[[paste0('Name',i)]]$value = spell$name
            spell_fields[[paste0("Level",i)]]$value = spell$level
            spell_fields[[paste0("School",i)]]$value = spell$school
            spell_fields[[paste0("Material",i)]]$value = spell$material
            spell_fields[[paste0("Casting",i)]]$value = spell$castingTime
            spell_fields[[paste0("Range",i)]]$value = spell$range
            spell_fields[[paste0("Duration",i)]]$value = paste(spell$duration,collapse= ' ')
            spell_fields[[paste0("Body",i)]]$value = stringr::str_replace(spell$text,pattern = '^(.|\n)*?Duration.*?\n\n','') %>% stringr::str_replace_all('•|\u{2022}','-') %>%
                stringr::str_replace_all('(½)|(\u{00BD})','1/2') %>%
                stringr::str_replace_all('(¾)|(\u{00BE})','3/4') %>%
                stringr::str_replace_all('(¼)|(\u{00BC})','1/4') %>%
                stringr::str_replace_all('(¼)|(\u{00BC})','1/4') %>%
                stringr::str_replace_all('(‘)|(\u{2018})',"'") %>%
                stringr::str_replace_all('(’)|(\u{2018})',"'") %>%
                stringr::str_replace_all('(—)|(\u{FE58})',"-") %>%
                stringr::str_replace_all('(“)|(\u{201C})','"') %>%
                stringr::str_replace_all('(”)|(\u{201D})','"') %>%
                stringr::str_replace_all('(→)|(\u{201D})','->')
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
