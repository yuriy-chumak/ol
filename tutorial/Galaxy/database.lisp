#!/usr/bin/ol

,load "sqlite.lisp"

; account идентифицирует пользователя. один пользователь - один аккаунт
(db:query "CREATE TABLE accounts (
   id INTEGER PRIMARY KEY
,  username TEXT
,  usermail TEXT -- временно не принимаем
,  password TEXT

,  session TEXT -- сеансовый ключ
,  address TEXT -- адрес, с которого зашли в аккаунт (todo: завести отдлельную табличку, и позволить множественные заходы с разных адресов?)
)")
; пользователь "никто" имеет номер 0
(for-each (lambda (values)
   (apply db:query (cons "INSERT INTO accounts (id, username, password) VALUES (?,?,?)" values))) '(
      (0 "noname" "023456")
      (1 "user@1" "123456")
      (2 "user@2" "223456")
      (3 "user@3" "323456")
      (4 "user@4" "423456")
      (5 "user@5" "523456")
      (6 "user@6" "623456")
      (7 "user@7" "723456")
      (8 "user@8" "823456")
      (9 "user@9" "923456")
))


; в играх участвуют расы
; каждый пользователь может завести себе столько рас, сколько захочет
(db:query "CREATE TABLE races (
   id INTEGER PRIMARY KEY
,  name TEXT -- название расы
,  account REFERENCES accounts(id)

,  prts INTEGER DEFAULT 0 -- Primary Racial Traints
,  lrts INTEGER DEFAULT 0 -- Lesser Racial Traits
)")
; есть несколько "предустановленных" рас, owner для них должен быть "0"
(for-each (lambda (values)
   (apply db:query (cons
"INSERT INTO races (id, name, account) VALUES (?,?,?)" values))) '(
      (1  "Humanoids"   0)
      (2  "Rabbitoids"  0)
      (3  "Insectoids"  0)
      (4  "Nucleotids"  1)
      (5  "Silicanoids" 1)
      (6  "Antetherals" 1)
))


; список всех игр - законченных и текущих
(db:query "CREATE TABLE games (
   id INTEGER PRIMARY KEY
,  name TEXT -- название игры
,  state INTEGER DEFAULT 0 -- текущее состояние игры (0 - создается, 1 - ждет других игровок, 2 - закончена, // 3 - просчитывается, 4 - ждет ходов)
-- параметры игры
,  difficulty INTEGER -- сложность (1 - easy, 2 - standard, 3 - harder, 4 - expert)
,  universe_size INTEGER -- размер мира (1 - tiny, 2 - small, 3 - medium, 4 - large, 5 - huge)
,  player_positions INTEGER -- ... (1 - close, 2 - moderate, 3 - farther, 4 - distant)
,  density    INTEGER -- ... (1 - sparse, 2 - normal, 3 - dense, 4 - packed)
,  options    INTEGER -- настройки (битовое поле)

)")
(db:query "CREATE TABLE game_players (
   id INTEGER PRIMARY KEY
,  game REFERENCES games(id)
,  race REFERENCES races(id)
)")

; сразу создам парочку игр
(for-each (lambda (values)
   (apply db:query (cons
"INSERT INTO games (id, name, state) VALUES (?,?,?)" values))) '(
      (1  "A Walk in the Park" 0)
      (2  "A Barefoot JayWalk" 0)
      (3  "Roller Ball"        0)
      (4  "Blade Runner"       0)))

(for-each (lambda (values)
   (apply db:query (cons
"INSERT INTO game_players (id, game, race) VALUES (?,?,?)" values))) '(
      (1  1 1)
      (2  1 3)
      (3  2 2)
      (4  2 4)
      (5  3 5)
      (6  3 6)
      (7  4 1)
      (8  4 3)
      (9  4 4)
))


;(db:query "CREATE TABLE users (
;   id INTEGER PRIMARY KEY -- comment
;)")
;
;(db:query "CREATE TABLE scrolls (
;   id INTEGER PRIMARY KEY
;,  title TEXT
;)")
