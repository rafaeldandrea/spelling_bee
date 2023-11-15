# required libraries
library(tidyverse)
library(ggforce)

# fixed parameters
min_length = 4 # minimum number of letters acceptable
set.seed(0) # seed for arranging letters on the screen
lexicon_size = 'medium' # options are short, medium, and long

# plot settings
theme_set(theme_bw())
theme_update(
  panel.grid = element_blank(),
  strip.background = element_rect(fill = 'orange'),
  aspect.ratio = 1
)


default_pangram = 'checkmat'
default_required_letter = 'k'
default_hive_letters = unique(unlist(str_split(default_pangram, pattern = '')))

status_thresholds = 
  c(0, 2, 5, 10, 15, 25, 45, 55, 75, 100) # percentage of the total possible points

status_list = # success categories
  c(
    'Beginner',
    'Good Start',
    'Moving Up',
    'Good',
    'Solid',
    'Nice',
    'Great',
    'Amazing',
    'Genius',
    'Queen Bee'
  )

# function to find solution based on lexicon and hive letters
find_solution = \(.dictionary, .required_letter, .hive_letters){
  dictionary = .dictionary
  required_letter = .required_letter
  hive_letters = .hive_letters
  
  verbotten =
    paste0('[', str_c(setdiff(c(LETTERS, letters), hive_letters), collapse = ''), ']')
  
  words =
    dictionary |>
    filter(
      str_detect(word, required_letter),
      str_detect(word, verbotten, negate = TRUE)
    )
  
  if(nrow(words) == 0){
    return(
      list(solution = tibble(), pangrams = tibble(), total_points = 0, breaks = NA)
    )
  }
  
  inds =
    sapply(words$word, function(word) {
      all(str_detect(word, hive_letters))
    })
  
  pangrams = words[inds,]
  
  if(nrow(pangrams) == 0){
    return(
      list(solution = tibble(), pangrams = tibble(), total_points = 0, breaks = NA)
    )
  }
  
  solution = 
    words |>
    mutate(
      length = 
        words |>
        apply(
          1, 
          \(word){
            word |>
              str_split(pattern = '') |>
              unlist() |>
              length()
          }
        ),
      unique_letters = 
          words |>
          apply(
            1, 
            \(word){
              word |>
                str_split(pattern = '') |>
                unlist() |>
                unique() |>
                length()
            }
          ),
      pangram = word %in% pangrams$word,
      points = ifelse(length == 4, 1, length + ifelse(pangram == TRUE, 7, 0))
    ) 
  
  total_points =
    solution |>
    pull(points) |>
    sum()
  
  breaks = round(status_thresholds / 100 * total_points)
  
  initials = 
    words |> 
    mutate(
      initial = toupper(str_sub(word, end = 1)),
      length = str_count(word)
    ) |>
    mutate(
      points = ifelse(length == 4, 1, length),
      points = points + 7 * (word %in% pangrams$word)
    )
  
  stats_initial = 
    initials |>
    count(initial)
  
  stats_initial_breakdown = 
    initials |> 
    count(initial, length) |>
    mutate(length = as.character(length)) |>
    bind_rows(
      stats_initial |>
        mutate(length = 'total')
    ) |>
    pivot_wider(
      names_from = length,
      values_from = n
    ) %>%
    replace(is.na(.), 0)
  
  stats_L = 
    initials |>
    count(length)
  
  
  return(
    list(
      solution = solution, 
      pangrams = pangrams, 
      total_points = total_points, 
      breaks = breaks,
      initial_summary = stats_initial,
      initial_breakdown = stats_initial_breakdown
    )
  )
}

# function to print options
print_options = \(){
  writeLines("Enter .rules to see the rules of the game")
  writeLines("Enter .found to see the words you have already found")
  writeLines("Enter .levels to see the levels and their breakpoints")
  writeLines("Enter .hints to see the word count per initial letter")
  writeLines("Enter .breakdown to see the word count + letter count per initial letter")
  writeLines("Enter .solution to see the full solution and end the game")
  writeLines("Enter .default to play with the default hive")
  writeLines("Enter .custom to play with a custom hive")
  writeLines("Enter .newhive to generate a new hive")
  writeLines("Enter .options to see these options again")
  writeLines('')
  writeLines("Enter END to end the game")
}

# print the initial greeting
print_greeting = \(){
  writeLines("Welcome to the Spelling Bee Knockoff!")
  writeLines('')
  writeLines("Enter .rules to see the rules of the game")
  writeLines("Enter .options to see options")
  writeLines('')
}

# print the rules of the game
print_rules = \(){
  writeLines("Rise to Genius level by creating words using letters from the hive.")
  writeLines("Words must contain at least 4 letters and include the center letter.")
  writeLines("Hyphenated words, proper nouns, and cuss words are not included.")
  writeLines("4-letter words earn 1 point, longer words earn 1 point per letter.")
  writeLines("The puzzle includes at least one 'pangram', which uses all 7 letters. These are worth 7 extra points!")
}

# functions to create hives
create_default_hive = \(){
  required_letter = default_required_letter
  hive_letters = default_hive_letters
  
  soltn =
    find_solution(
      .dictionary = dictionary,
      .required_letter = required_letter,
      .hive_letters = default_hive_letters
    )
  breaks = soltn$breaks
  
  hive_plot = 
    plot_function(
      hive_letters, 
      required_letter, 
      status = "Beginner",
      genius = breaks[length(breaks) - 1], 
      current_points = 0,
      found_words = NULL
    )
  show(hive_plot)
  
  return(
    list(
      hive_letters = hive_letters,
      required_letter = required_letter,
      solution = soltn$solution,
      pangrams = soltn$pangrams,
      total_points = soltn$total_points,
      breaks = soltn$breaks,
      initial_summary = soltn$initial_summary,
      initial_breakdown = soltn$initial_breakdown,
      found_words = NULL,
      accumulated_points = 0,
      current_status = 'Beginner'
    )
  )
}

create_custom_hive = \(){
  pangrams = tibble()
  while(nrow(pangrams) == 0){
    writeLines('\nEnter your letters as a 7+ letter string - no quotes needed (END to quit)')
    entered_word = tolower(readline())
    if (entered_word %in% c('end', '.end')) return(0)
    hive_letters = unique(unlist(str_split(entered_word, pattern = '')))
    while ('s' %in% hive_letters){
      writeLines("\nHive cannot contain 'S'!")
      writeLines('\nEnter your letters as a 7+ letter string - no quotes needed (END to quit)')
      entered_word = tolower(readline())
      if (entered_word %in% c('end', '.end')) return(0)
      hive_letters = unique(unlist(str_split(entered_word, pattern = '')))
    }
    while (length(hive_letters) != 7){
      writeLines('Need 7 different letters!')
      writeLines('\nEnter your letters as a 7+ letter string - no quotes needed (END to quit)')
      entered_word = tolower(readline())
      if (entered_word %in% c('end', '.end')) return(0)
      hive_letters = unique(unlist(str_split(entered_word, pattern = '')))
    }
    writeLines('Enter the required letter')
    required_letter = readline()
    if (required_letter %in% c('end', '.end')) return(0)
    while (!required_letter %in% hive_letters){
      writeLines('Required letter must be in the Hive!') 
      required_letter = readline()
      if (required_letter %in% c('end', '.end')) return(0)
    }
    
    soltn = 
      find_solution(
        .dictionary = dictionary, 
        .required_letter = required_letter,
        .hive_letters = hive_letters
      )
    solution = soltn$solution
    pangrams = soltn$pangrams
    total_points = soltn$total_points
    breaks = soltn$breaks
    
    if(nrow(pangrams) == 0){
      writeLines('Your hive has no pangram! Try again')
    }
  }
  
  hive_plot = 
    plot_function(
      hive_letters, 
      required_letter, 
      status = "Beginner",
      genius = breaks[length(breaks) - 1], 
      current_points = 0,
      found_words = NULL
    )
  show(hive_plot)
  
  return(
    list(
      hive_letters = hive_letters,
      required_letter = required_letter,
      solution = soltn$solution,
      pangrams = soltn$pangrams,
      total_points = soltn$total_points,
      breaks = soltn$breaks,
      initial_summary = soltn$initial_summary,
      initial_breakdown = soltn$initial_breakdown,
      found_words = NULL,
      accumulated_points = 0,
      current_status = 'Beginner'
    )
  )
} 

create_new_hive = \(reset, seed){
  set.seed(seed)
  total_points = 1000
  while(total_points > 400){
    .pangram = sample(possible_pangrams$word, 1)
    hive_letters = 
      .pangram |>
      str_split(pattern = '') |>
      unlist() |>
      unique()
    required_letter = sample(hive_letters, 1)
    soltn = 
      find_solution(
        .dictionary = dictionary, 
        .required_letter = required_letter,
        .hive_letters = hive_letters
      )
    breaks = soltn$breaks
    total_points = soltn$total_points
  }
  
  if(reset == TRUE){
    
    hive_plot = 
      plot_function(
        hive_letters, 
        required_letter, 
        status = "Beginner",
        genius = breaks[length(breaks) - 1], 
        current_points = 0,
        found_words = NULL
      )
    show(hive_plot)
    
    return(
      list(
        hive_letters = hive_letters,
        required_letter = required_letter,
        solution = soltn$solution,
        pangrams = soltn$pangrams,
        total_points = soltn$total_points,
        breaks = soltn$breaks,
        initial_summary = soltn$initial_summary,
        initial_breakdown = soltn$initial_breakdown,
        found_words = NULL,
        accumulated_points = 0,
        current_status = 'Beginner'
      )
    )
  } 
  
  if(reset == FALSE) {
    hive_plot = 
      plot_function(
        hive_letters, 
        required_letter, 
        status = current_status,
        genius = breaks[length(breaks) - 1], 
        current_points = accumulated_points,
        found_words = found_words
      )
    show(hive_plot)
    
    return(
      list(
        hive_letters = hive_letters,
        required_letter = required_letter,
        solution = soltn$solution,
        pangrams = soltn$pangrams,
        total_points = soltn$total_points,
        breaks = soltn$breaks,
        initial_summary = soltn$initial_summary,
        initial_breakdown = soltn$initial_breakdown
      )
    )
  }
  
}

# function to plot the hive
plot_function = 
  \(hive_letters, required_letter, status, genius, current_points, found_words){
    # Function to generate hexagon vertices
    hex_vertices = 
      \(center_x, center_y, size = 1) {
        tibble(
          x = center_x + size * cos(seq(0, 2 * pi, length.out = 7)),
          y = center_y + size * sin(seq(0, 2 * pi, length.out = 7))
        )
      }
    
    # Hexagon center positions
    hex_centers = 
      tibble(
        x = c(0, sqrt(3), sqrt(3), 0, -sqrt(3), -sqrt(3), 0),
        y = c(2, 1, -1, -2, -1, 1, 0),
        id = toupper(c(setdiff(hive_letters, required_letter), required_letter))
      )
    
    # Create data for plotting
    hex_data = 
      map2_df(hex_centers$x, hex_centers$y, ~hex_vertices(.x, .y)) |>
      mutate(id = rep(hex_centers$id, each = 7))
    
    cols = c(rep('lightgrey', 7 * 6), rep("gold", 7))
    
    if(length(found_words) != 1){
      plot_title = 
        paste0(
          status,
          '!\n',
          "You've found ",
          length(found_words),
          ' words     Your points: ', 
          current_points
        )
    } else{
      plot_title = 
        paste0(
          status,
          '!\n',
          "You've found 1 word      Your points: ", 
          current_points
        )
    }
    
    plot_caption = 
      paste0(
        'Genius: ', 
        genius,
        '    (', max(0, genius - current_points), ' points to Genius)'
      )
    
    # Plotting
    plot = 
      ggplot(hex_data, aes(x = x, y = y, group = id)) +
      geom_polygon(fill = cols, color = "black", linewidth = 1.5) +
      coord_fixed(ratio = 1) +
      geom_text(aes(x , y , label = id), data = hex_centers, size = 20) +
      theme_void() +
      labs(
        title = plot_title,
        caption = plot_caption
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = 'bold', size = 20),
        plot.caption = element_text(hjust = 0.5, size = 20)
      )
    
    return(plot)
  }


# read lexicon
if (!lexicon_size %in% c('short', 'medium', 'long'))
  stop('Lexicon must be one of "short", "medium", or "long"')


# shorter lexicon - for pangrams -- 10k most common words (minus swear words, 9k words)
# contains 1,546 possible pangrams
dictionary_short =
  read.table(
    url(
      'https://github.com/rafaeldandrea/spelling_bee/raw/main/google-10000-english-usa-no-swears.txt'
    )
  ) |>
  as_tibble() |>
  filter(str_count(V1) >= min_length) |>
  rename(word = V1) |>
  arrange(word)


# longer lexicon (368k words)
if (lexicon_size == 'long') {
  dictionary =
    read.table(
      url(
        'https://github.com/rafaeldandrea/spelling_bee/raw/main/words_alpha.txt'
      )
    ) |>
    as_tibble() |>
    filter(str_count(V1) >= min_length) |>
    rename(word = V1)
}

# medium lexicon (83k words) --
if (lexicon_size == 'medium') {
  dictionary =
    read.table(
      url(
        'https://github.com/rafaeldandrea/spelling_bee/raw/main/engmix.txt'
      )
    ) |>
    as_tibble() |>
    filter(str_count(V1) >= min_length) |>
    rename(word = V1) |>
    arrange(word)
}

# shorter lexicon -- 10k most common words (minus swear words, 9k words)
if (lexicon_size == 'short') {
  dictionary = dictionary_short  
}

# find all possible pangrams: words in the short lexicon with 7 unique letters and no "s"
possible_pangrams = 
  dictionary_short |>
  mutate(
    unique = apply(dictionary_short, 1, \(word) length(unique(unlist(str_split(word, pattern = '')))))
  ) |>
  filter(
    unique == 7,
    str_detect(word, 's') == FALSE
  ) |>
  inner_join(dictionary, by = 'word')


# start the game
print_greeting()

if(all(exists('found_words'), exists('accumulated_points'), exists('current_status'))){
  hive = create_new_hive(reset = FALSE, seed = as.integer(format(Sys.Date(), "%Y%m%d")))  
  list2env(hive, envir = .GlobalEnv)
  
  illegal_letters = 
    found_words |>
    sapply(
      \(word) 
      unlist(str_split(word, pattern = ''))) |> 
    unlist() |> 
    unique() |> 
    setdiff(hive_letters)
  
  if(length(illegal_letters) > 0){
    found_words = NULL
    accumulated_points = 0
    current_status = 'Beginner'
  } 
} else{
  hive = create_new_hive(reset = TRUE, seed = as.integer(format(Sys.Date(), "%Y%m%d")))  
  list2env(hive, envir = .GlobalEnv)
}

writeLines('\nEnter words, one at a time. No quotes needed.')

while (1) {
  writeLines('')
  entry = tolower(readline())
  
  if (entry %in% c('end', '.end'))
    break
  if (entry == '.newhive'){
    writeLines('Enter a numerical seed for the new hive')
    writeLines('')
    answer = as.numeric(readline())
    hive = create_new_hive(reset = TRUE, seed = answer)  
    list2env(hive, envir = .GlobalEnv)
    writeLines('\nEnter words, one at a time. No quotes needed.')
    writeLines('')
    next
  }
  if (entry == '.custom'){
    hive = create_custom_hive()
    if(hive != 0){
      list2env(hive, envir = .GlobalEnv)
      writeLines('\nEnter words, one at a time. No quotes needed.')
      writeLines('')
    } else{
      writeLines('\nCustom hive not created.')
      writeLines('')
    }
    next
  }
  if (entry == '.default'){
    hive = create_default_hive()
    list2env(hive, envir = .GlobalEnv)
    writeLines('\nEnter words, one at a time. No quotes needed.')
    writeLines('')
    next
  }
  if (entry == '.rules'){
    print_rules()
    next
  } 
  if (entry == '.hive') {
    cat(noquote('The hive letters are '))
    cat(noquote(toupper(sample(hive_letters))))
    cat(noquote(' and the core letter is '))
    cat(noquote(toupper(required_letter)))
    writeLines('')
    next
  }
  if(entry == '.found'){
    if(is.null(found_words)){
      writeLines('No words found yet!')
      next
    } 
    print(noquote(sort(found_words)))
    next
  }
  if(entry == '.levels'){
    writeLines('The levels are')
    tibble(
      category = status_list,
      points = breaks
    ) |>
      filter(category != 'Queen Bee') |>
      print()
    next
  }
  if(entry == '.hints'){
    view(initial_summary)
    if(nrow(pangrams == 1)) writeLines('There is 1 pangram')
    else writeLines(paste('There are', nrow(pangrams), 'pangrams'))
    writeLines(
      paste('There are', nrow(solution), 'words and', total_points, 'total points')
    )
    next
  }
  if(entry == '.breakdown'){
    view(initial_breakdown)
    if(nrow(pangrams == 1)) writeLines('There is 1 pangram')
    else writeLines(paste('There are', nrow(pangrams), 'pangrams'))
    writeLines(
      paste('There are', nrow(solution), 'words and', total_points, 'total points')
    )
    next
  }
  if(entry == '.options'){
    print_options()
    next
  }
  if(entry == '.solution'){
    if(nrow(pangrams) == 1)
      writeLines('The pangram is')
    else  
      writeLines('The pangrams are')
    print(noquote(pangrams$word))
    writeLines('\nThe full word list is:')
    print(noquote(solution$word))
    writeLines('\nof which you missed')
    print(noquote(setdiff(solution$word, found_words)))
    break
  }
  if(entry %in% found_words){
    writeLines('Word already found')
    next
  }
  if(str_length(entry) < 4){
    writeLines('Too short!')
    next
  }
  if(length(setdiff(unique(unlist(str_split(entry, pattern = ''))), hive_letters)) > 0){
    writeLines('Illegal letter!')
    next
  }
  if(!required_letter %in% unique(unlist(str_split(entry, pattern = '')))){
    writeLines('Must contain the core letter!')
    next
  }
  if(entry %in% solution$word) {
    found_words = c(found_words, entry)
    length = str_length(entry)
    if (entry %in% pangrams$word) {
      points =  length + 7
      greeting = 'Pangram!'
    } else{
      points = ifelse(length == 4, 1, length)
      if (length(found_words) == 1)
        greeting = 'You found your first word!'
      else
        greeting = 'You found another word!'
    }
    accumulated_points = accumulated_points + points
    status = status_list[findInterval(accumulated_points, breaks)]
    writeLines(paste(greeting, ' ', points, ' points'))
    if (status != current_status) {
      current_status = status
      if (status == 'Genius')
        writeLines("Congratulations, you've reached Genius!")
      else if (status == 'Queen Bee'){
        writeLines("You've found them all! Queen Bee!")
        break
      } else
        writeLines(paste0(current_status, '!'))
    }
    
    hive = 
      plot_function(
        hive_letters, 
        required_letter, 
        status = current_status,
        genius = breaks[length(breaks) - 1], 
        current_points = accumulated_points,
        found_words = found_words
      )
    show(hive)
    
  } else
    writeLines(paste0(entry, ' is not on the solution list :('))
}
