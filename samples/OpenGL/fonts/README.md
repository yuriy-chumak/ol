# How to generate font atlas

Go to https://evanw.github.io/font-texture-generator (github: https://github.com/evanw/font-texture-generator)
 * Change font name, font size.
 * Select JSON as data format.
 * Generate font atlas and save into png file
 * Save JSON into json
 * Don't forget use of "none" for background
 * Add to the json line `"file": "fonts/font-64.png"` with your font png file
 * That's all

; 0123456789
; ABCDEFGHIJKLMNOPQRSTUVWXYZ
; abcdefghijklmnopqrstuvwxyz
; АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ
; абвгдежзийклмнопрстуфхцчшщъыьэюя
; !?@#$%^&_=\|([{}])`"';:+-/*<>~.,
; ==>

0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzАБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯабвгдежзийклмнопрстуфхцчшщъыьэюя!?@#$%^&_=\|([{}])`"';:+-/*<>~.,