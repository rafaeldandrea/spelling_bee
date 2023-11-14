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
  writeLines("Type 'print guesses' to see your guesses so far")
  writeLines("Type 'print breaks' to see the category breakpoints")
  writeLines("Type 'print hive' to see the hive")
  writeLines("Type 'print hints' to see the word count per initial letter")
  writeLines("Type 'print detailed hints' to see the word + letter count per initial letter")
  writeLines("Type 'print solution' to see the full solution")
  writeLines("Type 'end' to end the game")
  writeLines("Type 'print options' to see those options again")
}

# function to plot the hive
plot_function = 
  \(hive_letters, required_letter, status, genius, current_points){
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
    
    cols = c(rep('white', 7 * 6), rep("gold", 7))
    
    if(!is.na(status)){
      plot_title = 
        paste0(
          status,
          '!    ',
          'Your points: ', 
          current_points, 
          '    Genius: ', 
          genius,
          '    (', max(0, genius - current_points), ' points to Genius)'
        )
    } else{
      plot_title = 
        paste0(
          'Your points: ', 
          current_points, 
          '    Genius: ', 
          genius,
          '    (', max(0, genius - current_points), ' points to Genius)'
        )
      }
      
    
    # Plotting
    plot = 
      ggplot(hex_data, aes(x = x, y = y, group = id)) +
      geom_polygon(fill = cols, color = "black", linewidth = 1.5) +
      coord_fixed(ratio = 1) +
      geom_text(aes(x , y , label = id), data = hex_centers, size = 20) +
      theme_void() +
      ggtitle(plot_title)
    
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
pangrams = tibble()
writeLines("Welcome to the Spelling Bee Knockoff!")
writeLines('')
print_options()
writeLines("\nWhat will it be today? \n 1 Today's hive \n 2 Default hive \n 3 Your custom hive")
answer = tolower(readline())
while(!answer %in% 1:3){
  writeLines('\nAcceptable options are 1, 2, or 3')
  writeLines("\nWhat will it be today? \n 1 Today's hive \n 2 Default hive \n 3 Your custom hive")
  answer = tolower(readline())
}
if(answer == 2){
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
} else if(answer == 3){
    pangrams = tibble()
    while(nrow(pangrams) == 0){
      writeLines('\nType your letters as a 7-letter string - no quotes needed')
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
    total_points = 1000
    set.seed(as.integer(format(Sys.Date(), "%Y%m%d")))
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
      solution = soltn$solution
      pangrams = soltn$pangrams
      total_points = soltn$total_points
      breaks = soltn$breaks
    }
  }

hive = 
  plot_function(
    hive_letters, 
    required_letter, 
    status = NA,
    genius = breaks[length(breaks) - 1], 
    current_points = 0
  )
show(hive)


found_words = NULL
accumulated_points = 0
current_status = 'Beginner'

cat(noquote('\nThe hive letters are '))
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
  if(guess == 'print solution'){
    writeLines('The full word list in the solution is:')
    print(noquote(solution$word))
    writeLines('of which you missed')
    print(noquote(setdiff(solution$word, found_words)))
    break
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
    
    hive = 
      plot_function(
        hive_letters, 
        required_letter, 
        status = current_status,
        genius = breaks[length(breaks) - 1], 
        current_points = accumulated_points
      )
    show(hive)
    
  } else
    writeLines(paste0(guess, ' is not on the solution list :('))
}
