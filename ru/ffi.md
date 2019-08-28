---
layout: page
title:  "Foreing function Interface"
date: 2016-11-28 15:51:55 UTC
categories: ru
---
> Внимание, эта статья находится в процессе создания; ее содержание может (и будет) меняться, пока полностью не удовлетворит автора. А до тех пор я не ручаюсь за стопроцентную достоверность приведенной информации.

> Внимание, в версии 1.2 PInvoke был заменен на Ffi. Соответственно, вся информация качающаяся PInvoke является устаревшей.

   Пример использования FFI механизма можно найти в туториале по [OpenGL](?ru/opengl) а также [исходных кодах](https://github.com/yuriy-chumak/ol/tree/master/tutorial/OpenGL) к нему (и не только).

#### Общие сведения

   Механизм FFI (Foreign Function Interface) предназначен для вызова системных и библиотечных функций, написанных на иных языках программирования (таких как С), без написания специального кода-прокладки (как JNI для Java). Все необходимое для вызова описывается прямо в lisp коде - виртуальная машина сама приведет данные к нужному формату, вызовет системную функцию и вернет из нее корректно приведенное к формату ol значение.

   Для примера давайте рассмотрим Windows функцию MessageBox (FFI механизм, понятно, специфичен для каждой ОС).

   1) подключим библиотеку otus/ffi
   <pre><code data-language="ol">
   (import (otus ffi))
   </code></pre>

   2) так как эта функция находится в системной библиотеке user32.dll, попытаемся загрузить ее
   <pre><code data-language="ol">
   (define user32 (load-dynamic-library "user32"))
   </code></pre>

   3) теперь мы можем получить адрес нужной нам функции в библиотеке
   <pre><code data-language="ol">
   (define MessageBox (user32 ffi-int "MessageBoxA"
              type-vptr
              type-string
              type-string
              fft-int))
   </code></pre>

   Этот шаг требует отдельных объяснений. Для начала пойдем [на MSDN](https://msdn.microsoft.com/en-us/library/windows/desktop/ms645505.aspx) и посмотрим объявление нужной нам функции. Вот как оно выглядит:
   <pre><code data-language="с">
   int WINAPI MessageBox(
      _In_opt_ HWND    hWnd,
      _In_opt_ LPCTSTR lpText,
      _In_opt_ LPCTSTR lpCaption,
      _In_     UINT    uType
   );
   </code></pre>

   Нам надо похоже описать функцию в аргументах вызова dlsym: fft-int мы используем, когда аргумент - число (предпочтительный вариант для всех числовых параметров); type-string - строка; type-vptr - указатель на данные). Название функции "MessageBoxA", так как есть два варианта этой функции в Windows, обычный и юникодный.

   Для красоты объявим еще несколько переменных
   <pre><code data-language="ol">
   (define IDOK 1)
   (define IDCANCEL 2)

   (define MB_OK 0)
   (define MB_OKCANCEL 1)
   (define MB_ICONASTERISK 64)
   </code></pre>

   И теперь можем использовать всплывающее окно с сообщением в нашей программе
   <pre><code data-language="ol">
   (if (eq? (MessageBox NULL "Please, press OK for test pass!"
               (c-string "PInvoke sample use")
               (+ MB_OKCANCEL MB_ICONASTERISK)) IDOK)
      (print "OK")
      (print "CANCEL"))
   </code></pre>

   ![message box](assets/messagebox.png)

   Вот полный список "родных" типов и соответствующих им типов pinvoke:

  * int <- fft-int
  * short <- fft-int16
  * int32 <- fft-int32
  * long long <- fft-int64
  * void <- type-void (функции, возвращающие type-void будут возвращать #true, если вызов был успешен)
  * void* <- type-vptr или fft-void*
  * float <- fft-float
  * double <- fft-double
  * char* <- type-string

