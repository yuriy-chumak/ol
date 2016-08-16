#!/usr/bin/ol

,load "sqlite.lisp"

; account идентифицирует пользователя. один пользователь - один аккаунт
(sqlite:query "CREATE TABLE accounts (
   id INTEGER PRIMARY KEY
,  username TEXT
,  email TEXT
)")
; пользователь "никто" имеет номер 0
(sqlite:query "INSERT INTO accounts (id, username) VALUES (?,?)"
   0 "noname")
(sqlite:query "INSERT INTO accounts (id, username) VALUES (?,?)"
   1 "user#1")


; в играх участвуют расы
; каждый пользователь может завести себе столько рас, сколько захочет
(sqlite:query "CREATE TABLE races (
   id INTEGER PRIMARY KEY
,  name TEXT -- название расы
,  account REFERENCES accounts(id)

,  primary_trait INTEGER
)")
; есть несколько "предустановленных" рас, owner для них должен быть "0"
(for-each (lambda (values)
   (apply sqlite:query (cons "INSERT INTO races (id, name, account) VALUES (?,?,?)" values))) '(
      (1  "Humanoid"   0)
      (2  "Rabbitoid"  0)
      (3  "Insectoid"  0)
      (4  "Nucleotid"  1)
      (5  "Silicanoid" 1)
      (6  "Antetheral" 1)
))


; список всех игр - законченных и текущих
(sqlite:query "CREATE TABLE games (
   id INTEGER PRIMARY KEY
,  name TEXT -- название игры
,  state INTEGER -- текущее состояние игры (0 - создается, 1 - ждет ходов, 2 - просчитывается)
-- параметры игры
,  difficulty INTEGER -- сложность (1 - easy, 2 - standard, 3 - harder, 4 - expert)
,  universe_size INTEGER -- размер мира (1 - tiny, 2 - small, 3 - medium, 4 - large, 5 - huge)
,  player_positions INTEGER -- ... (1 - close, 2 - moderate, 3 - farther, 4 - distant)
,  density    INTEGER -- ... (1 - sparse, 2 - normal, 3 - dense, 4 - packed)
,  options    INTEGER -- настройки (битовое поле)

)")
(sqlite:query "CREATE TABLE game_players (
   id INTEGER PRIMARY KEY
,  game REFERENCES games(id)
,  race REFERENCES races(id)
)")

; сразу создам парочку игр
(for-each (lambda (values)
   (apply sqlite:query (cons "INSERT INTO games (id, name, state) VALUES (?,?,?)" values))) '(
      (1  "A Walk in the Park" 0)
      (2  "A Barefoot JayWalk" 0)
      (3  "Roller Ball"        0)
      (4  "Blade Runner"       0)))

(for-each (lambda (values)
   (apply sqlite:query (cons "INSERT INTO game_players (id, game, race) VALUES (?,?,?)" values))) '(
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


(sqlite:query "CREATE TABLE users (
   id INTEGER PRIMARY KEY -- comment
)")

(sqlite:query "CREATE TABLE scrolls (
   id INTEGER PRIMARY KEY
,  title TEXT
)")

