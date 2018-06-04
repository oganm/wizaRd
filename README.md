Readme
================

wizaRd
======

A list of D&D 5e spells and some functions to fiddle with them.

Spell data taken from <https://github.com/thebombzen/grimoire>, originally <https://github.com/ephe/grimoire/>.

Installation
------------

    devtools::install_github('oganm/wizaRd')

Usage
-----

The package includes a list that include all 5e spells called `spells`. Each element of this list is a list that holds different attributes of the spells.

``` r
spells$`Acid Splash` %>% ls
```

    ##  [1] "castingTime" "classes"     "components"  "dice"        "duration"   
    ##  [6] "level"       "name"        "range"       "ritual"      "school"     
    ## [11] "source"      "tags"        "text"

``` r
spells$`Acid Splash`$components
```

    ## [1] "V" "S"

Individual spells have a special print function (`print.spell`). When a spell is called the text for the spell is printed and any dice roll found to be associated with the spell based on the text is rolled

``` r
spells$Immolation
```

    ## **5th-level evocation**
    ## 
    ## **Casting Time**: 1 action
    ## 
    ## **Range**: 90 feet
    ## 
    ## **Components**: V
    ## 
    ## **Duration**: Concentration, up to 1 minute
    ## 
    ## Flames wreathe one creature you can see within range. The target must make a Dexterity saving throw. It takes 8d6 fire damage on a failed save, or half as much damage on a successful one. On a failed save, the target also burns for the spell’s duration. The burning target sheds bright light in a 30-foot radius and dim light for an additional 30 feet. At the end of each of its turns, the target repeats the saving throw. It takes 4d6 fire damage on a failed save, and the spell ends on a successful one. These magical flames can’t be extinguished by nonmagical means.
    ## 
    ## If damage from this spell kills a target, the target is turned to ash.
    ## 7d6 3d6
    ## [1] "Rolls: [ 2 3 4 *6* 2 *6* *6* ]"
    ## [1] "Rolls: [ 4 4 *1* ]"
    ## 7d6 3d6 
    ##  29   9

Note that dice annotations are not manually curated. If it sees a mention of a dice in the spell description, it rolls it.

The `spells` object also has a special print function (`print.spellList`).

``` r
head(spells)
```

    ## Cantrips
    ## ========
    ## Acid Splash
    ## 
    ## Level 1
    ## =======
    ## Alarm
    ## Animal Friendship
    ## Armor of Agathys
    ## Arms of Hadar
    ## Bane

A few utility functions exist to create subset `spellList`s

``` r
spells %>% filterSpells(level=c(0,1),class= 'bard',sources='PHB',school='evocation')
```

    ## Cantrips
    ## ========
    ## Dancing Lights
    ## Light
    ## 
    ## Level 1
    ## =======
    ## Cure Wounds
    ## Faerie Fire
    ## Healing Word
    ## Thunderwave

``` r
makeBook(level=3)
```

    ## 
    ## Level 1
    ## =======
    ## Disguise Self
    ## Detect Magic
    ## Silent Image
    ## Fog Cloud
    ## Sleep
    ## Illusory Script
    ## Thunderwave
    ## Cause Fear
    ## 
    ## Level 2
    ## =======
    ## Knock
    ## Pyrotechnics
