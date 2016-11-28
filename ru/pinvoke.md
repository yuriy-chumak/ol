---
layout: page
title:  "Platform Invoke"
date: 2016-11-28 15:51:55 UTC
categories: ru
---

   Пример использования PInvoke механизма можно найти в туториале по [OpenGL](?ru/opengl) а также [исходных кодах](https://github.com/yuriy-chumak/ol/tree/master/tutorial/OpenGL) к нему.
   
#### Общие сведения

   Механизм Platform Invoke (pinvoke) предназначен для вызова системных и библиотечных функций, написанных на "родных" языках программирования (таких как С), без написания специального кода-прокладки (как JNI для Java). Все необходимое для вызова описывается прямо в lisp коде - виртуальная машина сама приведет данные к нужному формату, вызовет системную функцию и вернет из нее корректно приведенное к формату ol значение.

   Для примера давайте рассмотрим Windows функцию MessageBox (pinvoke механизм, понятно, специфичен для каждой ОС).

   1) подключим библиотеку otus/pinvoke
   <pre><code data-language="scheme">
   (import (otus pinvoke))
   </code></pre>

   2) так как эта функция находится в системной библиотеке user32.dll, попытаемся загрузить ее
   <pre><code data-language="scheme">
   (define user32 (dlopen "user32"))
   </code></pre>

   3) теперь мы можем получить адрес нужной нам функции в библиотеке
   <pre><code data-language="scheme">
   (define MessageBox (dlsym user32 type-int+ "MessageBoxA"
              type-int+
              type-string
              type-string
              type-int+))
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

   Нам надо похоже описать функцию в аргументах вызова dlsym: type-int+ мы используем, когда аргумент - число (предпочтительный вариант для всех числовых параметров); type-string - строка). Название функции "MessageBoxA", так как есть два варианта этой функции в Windows, обычный и юникодный.

   Для красоты объявим еще несколько переменных
   <pre><code data-language="scheme">
   (define IDOK 1)
   (define IDCANCEL 2)

   (define MB_OK 0)
   (define MB_OKCANCEL 1)
   (define MB_ICONASTERISK 64)
   </code></pre>

   И теперь можем использовать всплывающее окно с сообщением в нашей программе
   <pre><code data-language="scheme">
   (if (eq? (MessageBox 0 "Please, press OK for test pass!"
               (c-string "PInvoke sample use")
               (+ MB_OKCANCEL MB_ICONASTERISK)) IDOK)
      (print "OK")
      (print "CANCEL"))
   </code></pre>

   ![message box](assets/messagebox.png)
   
   Вот полный список "родных" типов и соответствующих им типов pinvoke:
   
  * int <- type-int+ (если достоверно известно, что результат будет маленьким, то можно использовать type-fix+)
  * short <- type-int16
  * int32 <- type-int32
  * long long <- type-int64
  * void <- type-void (функции, возвращающие type-void будут возвращать #true, если вызов был успешен)
  * void* <- type-void*
  * float <- type-float
  * double <- type-double
  * char* <- type-string
  
   Кроме того существует возможность возврата в ol значений из функции "по указателю". Для этого к передаваемому типу надо добавить #x80. Пример такого поведения можно посмотреть в библиотеке (lib newton) у функции NewtonBodyGetMatrix - эта функция возвращает в ol метрицу чисел с плавающей запятой и используется в примерах.
  * long <- type
