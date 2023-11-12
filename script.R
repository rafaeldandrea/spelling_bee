# required library
library(tidyverse)

# fixed parameters
min_length = 4 # minimum number of letters acceptable
set.seed(0) # seed for arranging letters on the screen
lexicon_size = 'medium' # options are short, medium, and long

default_pangram = 'divulging'
default_required_letter = 'd'
default_hive_letters = unique(unlist(str_split(default_pangram, pattern = '')))

status_thresholds = 
  c(0, 2, 5, 10, 15, 25, 45, 55, 75, 100) # percentage of the total possible points

print_options = \(){
  writeLines("Type 'print guesses' to see your guesses so far")
  writeLines("Type 'print breaks' to see the category breakpoints")
  writeLines("Type 'print hive' to see the hive")
  writeLines("Type 'print hints' to see the word count per initial letter")
  writeLines("Type 'print detailed hints' to see the word + letter count per initial letter")
  writeLines("Type 'end' to end the game")
  writeLines("Type 'print options' to see those options again")
}

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
      pangram = word %in% pangrams$word,
      length = str_length(word),
      points = ifelse(length == 4, 1, ifelse(pangram, length + 7, length))
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


# read lexicon
if (!lexicon_size %in% c('short', 'medium', 'long'))
  stop('Lexicon must be one of "short", "medium", or "long"')

## longer lexicon (368k words)
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
  dictionary =
    read.table(
      url(
        'https://github.com/rafaeldandrea/spelling_bee/raw/main/google-10000-english-usa-no-swears.txt'
      )
    ) |>
    as_tibble() |>
    filter(str_count(V1) >= min_length) |>
    rename(word = V1) |>
    arrange(word)
}

# start the game
pangrams = tibble()
writeLines("Welcome to the Spelling Bee Knockoff!")
writeLines('')
print_options()
writeLines('\nUse default pangram?')
answer = tolower(readline())
if(answer == 'yes'){
  required_letter = default_required_letter
  hive_letters = default_hive_letters
  soltn = 
    find_solution(
      .dictionary = dictionary, 
      .required_letter = required_letter,
      .hive_letters = default_hive_letters
    )
  solution = soltn$solution
  pangrams = soltn$pangrams
  total_points = soltn$total_points
  breaks = soltn$breaks
} else{
  writeLines('Use custom hive letters?')
  answer = tolower(readline())
  if(answer == 'yes'){
    pangrams = tibble()
    while(nrow(pangrams) == 0){
      writeLines('Type your letters as a 7-letter string - no quotes needed')
      entered_word = tolower(readline())
      hive_letters = unique(unlist(str_split(entered_word, pattern = '')))
      if (length(hive_letters) != 7)
        stop('Need 7 different letters!')
      
      writeLines('Type the required letter')
      required_letter = readline()
      if (!required_letter %in% hive_letters)
        stop('Required letter must be in the Hive!')  
      
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
  } else{
    set.seed(as.integer(format(Sys.Date(), "%Y%m%d")))
    pangrams = tibble()
    while(nrow(pangrams) == 0){
      hive_letters = sample(setdiff(letters, 's'), 7)
      required_letter = sample(hive_letters, 1)
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
    }
  }
}

found_words = NULL
accumulated_points = 0
current_status = 'Beginner'

cat(noquote('The hive letters are '))
cat(noquote(toupper(sample(hive_letters))))
cat(noquote(' and the core letter is '))
cat(noquote(toupper(required_letter)))

writeLines('\n\nGo ahead and type your guesses, one at a time. No quotes needed.')

while (1) {
  writeLines('')
  guess = tolower(readline())
  if (guess == 'end')
    break
  if (guess == 'print hive') {
    cat(noquote('The hive letters are '))
    cat(noquote(toupper(sample(hive_letters))))
    cat(noquote(' and the core letter is '))
    cat(noquote(toupper(required_letter)))
    writeLines('')
    next
  }
  if(guess == 'print guesses'){
    if(is.null(found_words)){
      writeLines('No words found yet!')
      next
    } 
    writeLines(sort(found_words))
    next
  }
  if(guess == 'print breaks'){
    writeLines('The category breaks are')
    tibble(
      category = status_list,
      points = breaks
    ) |>
      filter(category != 'Queen Bee') |>
      print()
    next
  }
  if(guess == 'print hints'){
    view(soltn$initial_summary)
    if(nrow(pangrams == 1)) writeLines('There is 1 pangram')
    else writeLines(paste('There are', nrow(pangrams), 'pangrams'))
    writeLines(
      paste('There are', nrow(solution), 'words and', total_points, 'total points')
    )
    next
  }
  if(guess == 'print detailed hints'){
    view(soltn$initial_breakdown)
    if(nrow(pangrams == 1)) writeLines('There is 1 pangram')
    else writeLines(paste('There are', nrow(pangrams), 'pangrams'))
    writeLines(
      paste('There are', nrow(solution), 'words and', total_points, 'total points')
    )
    next
  }
  if(guess == 'print options'){
    print_options()
    next
  }
  if(guess %in% found_words){
    writeLines('Word already found')
    next
  }
  if(str_length(guess) < 4){
    writeLines('Too short!')
    next
  }
  if(length(setdiff(unique(unlist(str_split(guess, pattern = ''))), hive_letters)) > 0){
    writeLines('Illegal letter!')
    next
  }
  if(!required_letter %in% unique(unlist(str_split(guess, pattern = '')))){
    writeLines('Must contain the core letter!')
    next
  }
  if(guess %in% solution$word) {
    found_words = c(found_words, guess)
    length = str_length(guess)
    if (guess %in% pangrams) {
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
    writeLines(paste(greeting, '  Accumulated points: ', accumulated_points))
    if (status != current_status) {
      current_status = status
      if (status == 'Genius')
        writeLines("Congratulations, you reached Genius!")
      else if (status == 'Queen Bee'){
        writeLines("You found them all! Queen Bee!")
        break
      } else
        writeLines(paste0(current_status, '!'))
    }
  } else
    writeLines(paste0(guess, ' is not on the solution list :('))
}
