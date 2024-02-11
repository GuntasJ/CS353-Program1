#lang racket
;///////////////////////////////////////////////////////////
;Terms

;bowling-entry: The entry in a bowling game that shows up on the sheet. For example, X / 5 would be bowling-entries.

;bowling-entry-value: The value of a bowling-entry. ex) X -> 10, / -> prev - 10, 1-9 -> 1-9

;score-string: A single string containing the bowling-entries.
;ex) "X 7 2  4 5  8 /  3 6  X  X  5 / 9 / 1 8"

;bowling-entries: A list of many bowling-entry. The bowling-entry can be a string or a number.
;ex) ("X") (7) (2)  (4) (5)  (8) ("/")  (3) (6)  ("X") ("X")  (5) ("/") (9) ("/") (1) (8)

;score: The score that a bowling-entry yields in total. This is different from the bowling-entry-value for strikes and spares.
;The bowling-entry-value of a strike is 10, while its score is 10 + next two bowling-entry-value

;game-score: Total score of a game.

;player-score: Total score of a player. This is eq to the total score of a player-game

;team-score; Total score of a team. 

;team: Consists of the team name and all the players' games in a team.
;ex) ("Oddballs" (Frosty Snoman X 7 2  4 5  8 /  3 6  X  X  5 / 9 / 1 8) (Anakin Starstomper 6 / 7 2 7 / 8 / X 6 3 7 / 6 3 7 1 8 0)...)

;game: Consists of the name of the player along with a score-string

;player-game: A list containing all of the games played by a given player.

;player-result: A list like the following: (player-name (game-score1 game-score2 game-score3))

;player-stats: A list like the following:  (player-name (game-score1 game-scrore2 game-score3) player-score)
;Essentially a list of player-result and player-score

;grouped-team: A list of all the player-games in a team. May also be written as teamX-grouped
;///////////////////////////////////////////////////////////

;checks if bowling-entry is a spare
(define (spare? string)
  (equal? "/" string))

;checks if bowling-entry is a strike
(define (strike? string)
  (equal? "X" string))


;Converts the bowling-entry to its bowling-entry-value. 1-9 have their same values but X and / have different values that need to be found by looking ahead
(define (get-bowling-entry-value entry [previous-entry 0] [current-box 0])
  (cond
    [(strike? entry) 10]
    [(spare? entry) (- 10 previous-entry)]
    [(>= current-box 21) 0]
    [(and (= current-box 20) (strike? previous-entry)) 0]
    [else entry]))

;Converts the score-string to the bowling entries
(define (score-string->bowling-entries score-string)
  (define score-list (string-split score-string))
  (define (convert-score-string-to-bowling-entries string)
    (cond
      [(spare? string) "/"]
      [(strike? string) "X"]
      [else (string->number string)]))
  (map convert-score-string-to-bowling-entries score-list))

;Calculates the total score the strike yields. It checks the next values to get the right score. 
(define (calculate-strike-score bowling-entries current-box)
  (cond
    [(>= current-box 20) 0]
    [(equal? (length bowling-entries) 1) 10]
    [(equal? (length bowling-entries) 2) (+ 10 (get-bowling-entry-value (second bowling-entries)))]
    [else
     (+ 10
        (get-bowling-entry-value (first (rest bowling-entries)))
        (get-bowling-entry-value (third bowling-entries) (second bowling-entries)))])) ;impossible for spare to be on first in box, gets previous value for spare calc

;Calculates the total score the spare yields. It checks the next value to get the right score
(define (calculate-spare-score bowling-entries previous-value current-box)
  (define 10-remainder-value (- 10 previous-value))
  (cond
    [(>= current-box 21) 0]
    [(equal? (length bowling-entries) 1) 10-remainder-value]
    (else (+ 10-remainder-value (get-bowling-entry-value (second bowling-entries))))))

;Calculates the total score of the bowling-entries. 
(define (calculate-bowling-entries-score bowling-entries [current-box 1] [previous-entry 0] [total-score 0])
  (cond
    [(empty? bowling-entries) total-score]
    [(strike? (first bowling-entries))
     (define box-increase
       (if (>= current-box 19)
           1
           2))
     (calculate-bowling-entries-score (rest bowling-entries) (+ current-box box-increase) (first bowling-entries) (+ total-score (calculate-strike-score bowling-entries current-box)))]

    [(spare? (first bowling-entries)) (calculate-bowling-entries-score (rest bowling-entries) (add1 current-box) (first bowling-entries) (+ total-score (calculate-spare-score bowling-entries previous-entry current-box)))]
    
    [else (calculate-bowling-entries-score (rest bowling-entries) (add1 current-box) (first bowling-entries) (+ total-score (get-bowling-entry-value (first bowling-entries) previous-entry current-box)))]))


;Reads the file and retures a list of the teams. Each team contains all the games played in that team along with the team's name
(define (bowling-file-data->teams file-name)
  (define file-contents (port->lines (open-input-file file-name)))
  
  (define (team-name? line)
    (define word-count (length (string-split line)))
    (= word-count 1))
  
  (define (get-teams first-team remaining-list [found-team-name? #f])
    (define line (first remaining-list))
    (cond
      [(team-name? line) (if found-team-name?
                             (list first-team (append (list line) (rest remaining-list)))
                             (get-teams (append first-team (list line) ) (rest remaining-list) #t))]
      [else
       (define new-first-team (append first-team (list (first remaining-list))))
       (get-teams new-first-team (rest remaining-list) found-team-name?)]))

  (get-teams '() file-contents))

;Gets the team name from a team
(define (get-team-name team)
  (first team))

;Gets the games played from a team. This will get rid of the name in the return. 
(define (get-team-games team)
  (rest team))

;Gets the name of the player from the game
(define (get-player-name game)
    (define split-game (string-split game))
    (string-append (first split-game) " " (second split-game)))

;Gets the score-string from the game
(define (get-player-score-string game)
  (define split-game (string-split game))
  (define split-score-string (rest (rest split-game)))
  (string-join split-score-string))

;Groups the games played in a team by the player. The end result is a list of a list where the inner list contains different player-games of the same name
(define (group-games-by-players team)
  (group-by get-player-name (get-team-games team)))

;Calculates the total score of a game
(define (calculate-game-score game)
  (calculate-bowling-entries-score (score-string->bowling-entries (get-player-score-string game))))

;Calculates the score of each game in the player-game. 
(define (calculate-each-game-score player-game)
  (map calculate-game-score player-game))


;Converts the player-game to a player-result
(define (player-game->player-result player-game)
  (define player-name (get-player-name (first player-game)))
  (define calculated-each-game-score (calculate-each-game-score player-game))
  (list player-name calculated-each-game-score))

;Calculates the total score of a player-game. This means that it returns the combined sum of every game in the player-game
(define (calculate-player-score player-game)
  (foldl + 0 (calculate-each-game-score player-game)))

;Gets the players stats in a list includes their name, each game score, and their total score.
(define (player-game->player-stats player-game)
  (define player-name (get-player-name (first player-game)))
  (define calculated-each-game-score (calculate-each-game-score player-game))
  (define player-score (calculate-player-score player-game))
  (list player-name calculated-each-game-score player-score))

;calculates the total score of a team
(define (calculate-team-score team)
  (define games (get-team-games team))
  (foldl + 0 (map calculate-game-score games)))


(define (find-highest-scoring-players all-player-stats)
  (define (iterate all-player-stats highest-scoring-player-stats)
    (cond
      [(empty? all-player-stats) highest-scoring-player-stats]
      [(> (third (first all-player-stats)) (third (first highest-scoring-player-stats))) (iterate (rest all-player-stats) (list (first all-player-stats)))]
      [(= (third (first all-player-stats)) (third (first highest-scoring-player-stats))) (iterate (rest all-player-stats) (append highest-scoring-player-stats (first all-player-stats)))]
      [else (iterate (rest all-player-stats) highest-scoring-player-stats)]))
  (iterate (rest all-player-stats) (list (first all-player-stats))))

(define (calculate-and-display-final-results)
  (displayln "Bowling Results")
  (displayln "")

  (define teams (bowling-file-data->teams "scores.txt"))  
  (define team1-name (get-team-name (first teams)))
  (define team2-name (get-team-name (second teams)))

  (define team1-grouped (group-games-by-players (first teams)))
  (define team2-grouped (group-games-by-players (second teams)))

  (define (displayln-player-stats player-stats)
    (displayln (~a "Player Name: " (first player-stats)))
    (displayln (~a "Game #1: " (first (second player-stats))))
    (displayln (~a "Game #2: " (second (second player-stats))))
    (displayln (~a "Game #3: " (third (second player-stats))))
    (displayln (~a "Total Score: " (third player-stats) "\n")))

  (define (displayln-team-score team)
    (displayln (~a (get-team-name team) "'s Total Points: " (calculate-team-score team) "\n")))

  (define (displayln-best-player highest-scoring-player-stats-list)
    (for-each
     (λ (highest-scoring-player-stats) (displayln (~a "The highest scoring player was " (first highest-scoring-player-stats) " with a score of " (third highest-scoring-player-stats))))
     highest-scoring-player-stats-list))

  (for-each displayln-player-stats (map player-game->player-stats team1-grouped))
  (displayln-team-score (first teams))

  (for-each displayln-player-stats (map player-game->player-stats team2-grouped))
  (displayln-team-score (second teams))

  (if (> (calculate-team-score (first teams)) (calculate-team-score (second teams)))
      (displayln (~a (get-team-name (first teams)) " won\n"))
      (displayln (~a (get-team-name (second teams)) " won\n")))

  
  (define all-player-stats (map player-game->player-stats (append team1-grouped team2-grouped))) 
  (displayln-best-player (find-highest-scoring-players all-player-stats)))
 
                               
;/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
(calculate-and-display-final-results)


