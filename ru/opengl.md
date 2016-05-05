---
layout: page
title:  OpenGL tutorials
date: 2016-05-05 12:03:10 UTC
categories: ru
---

   Для демонстрации возможностей Otus Lisp (ol) вполне подойдет OpenGL. Я приведу пример полного цикла разработки своего мультиплатформенного приложения на базе Ol без привлечения других языков и/или инструментов. Сами примеры можно взять в [официальном репозитарии](https://github.com/yuriy-chumak/OL/tree/master/tutorial/OpenGL) ol.

   Демонстрация будет разбита на несколько последовательных частей, в результате которых мы научимся создавать трехмерный график некоторой функции.
   
   Итак, относящаяся к "интерфейсной" работе часть :
   
 * [Создание окна](#window) ([Linux](#window-linux), [Windows](#window-win32))
 * [Инициализация OpenGL контекста](#context) ([Linux](#context-linux), [Windows](#context-win32))
 * [Цикл обработки сообщений и рендеринг](#loop) ([Linux](#loop-linux), [Windows](#loop-win32))
  * [Объединение всего вышеперечисленного в один платформонезависимый код](#all)
  
   Непосредственно работа с графикой:
  
 * [Рисуем треугольник в плоскости](#triangle)


#### <a name="window"></a>Создание окна

   Для вывода чего-либо на дисплей, надо для этого попросить у операционной системы место, куда мы сможем выводить свое творчество. Такое место во всех поддерживаемых операционных системах называется "окно" (window). После того, как система его нам выделит, надо разместить это окно на экране. Данный процесс, хотя и является принципиально общим для всех операционных систем, специфичен для каждой из них в отдельности и требует вызова разных функций. Далее будет продемонстрировано, как это можно сделать в каждом конкретном случае.

##### <a name="window-linux"></a>Linux (X11)

   Для линукса и юникса есть несколько оконных систем ([X11](https://en.wikipedia.org/wiki/X_Window_System), [Wayland](https://en.wikipedia.org/wiki/Wayland_(display_server_protocol)), [Mir](https://en.wikipedia.org/wiki/Mir_(software))), для примера мы будем использовать, наверное, самую распространенную - X11.
   
   Биндинги для Linux X11 сложены в библиотеку lib/x11, подключим ее:
<pre><code data-language="scheme">(import (lib x11))
</code></pre>

   Для начала надо выбрать дисплей, с которым мы хотим работать (в примере это будет доступный по-умолчанию дисплей) и рабочий стол, на котором мы будем размещать наше окно (в Linux можно размещать на одном дисплее разные рабочие пространства).

   После чего создаем простое окно, размещаем его на экране и сообщаем системе, что оно будет себя отрисовывать. 

<pre><code data-language="scheme">
(define display (XOpenDisplay 0))
(define screen (XDefaultScreen display))

(define width 320)
(define height 240)

(define window (XCreateSimpleWindow display (XRootWindow display screen)
   0 0 width height 1
   (XBlackPixel display screen) (XWhitePixel display screen)))
(XMapWindow display window)
(XSelectInput display window ExposureMask)
</code></pre>

   Детальное описание всех функций можно найти в официальной документации по [Xlib](http://www.x.org/wiki/ProgrammingDocumentation/).

##### <a name="window-win32"></a>Windows

   Биндинги для Windows API сложены в библиотеку lib/winapi, подключим ее:
<pre><code data-language="scheme">(import (lib winapi))
</code></pre>

   В отличие от Linux, нам не надо выбирать дисплей и рабочий стол, Windows все сделает вместо нас - мы можем сразу приступить к созданию простого окна.

<pre><code data-language="scheme">
(define width 320)
(define height 240)

(define window (CreateWindowEx
   (OR WS_EX_APPWINDOW WS_EX_WINDOWEDGE) "#32770" "OL OpenGL Sample 0" ; #32770 is for system classname for DIALOG
   (OR WS_OVERLAPPEDWINDOW WS_CLIPSIBLINGS WS_CLIPCHILDREN)
   0 0 width height ; x y width height
   null ; no parent window
   null ; no menu
   null ; instance
   null)) ; todo: override as '(INTEGER . 0)
</code></pre>

   Детальное описание всех функций Windows можно найти в официальной документации [Windows API](https://msdn.microsoft.com/en-us/library/windows/desktop/ff818516.aspx).


#### <a name="context"></a>Инициализация OpenGL контекста

   Вторым нашим шагом будет создание OpenGL контекста, который содержит в себе набор важнейших параметров, на основании которых графическая система выберет правильное, удовлетворяющее нас функционирование. В эти параметры входит двойная буферизация вывода, количество бит на цвет пикселя, существование буфера глубины и количество бит на элемент а также другие параметры, которые выбираются один раз и не меняются на протяжении существования контекста.
    
   Для этого урока мы выберем 24-канальный цвет (по 8 бит на красный, зеленый и синий каналы), двойную буферизацию (для более плавной смены картинки) и 24-битный буфер глубины.

   Итак, создаем контекст выполнения графической подсистемы:
   
##### <a name="context-linux"></a>Linux

   Это довольно простая операция. Первым шагом создаем структуру с нужными параметрами, вторым создаем контекст.

<pre><code data-language="scheme">
(define vi (glXChooseVisual display screen
   (raw type-vector-raw '(
      4 0 0 0 ; GLX_RGBA
      5 0 0 0 ; GLX_DOUBLEBUFFER
      8 0 0 0  1 0 0 0 ; GLX_RED_SIZE
      9 0 0 0  1 0 0 0 ; GLX_GREEN_SIZE
     10 0 0 0  1 0 0 0 ; GLX_BLUE_SIZE
     12 0 0 0 24 0 0 0 ; GLX_DEPTH_SIZE

      0 0 0 0)))); None
(define cx (glXCreateContext display vi 0 1))
</code></pre>

##### <a name="context-win32"></a>Windows

   В Windows все несколько сложнее. Первым шагом нам надо попросить контекст устройства для окна, потом выбрать для полученного контекста нужный нам набор параметров и уже тогда создавать контекст OpenGL.
   
   Структура с параметрами называется [PIXELFORMATDESCRIPTOR structure](https://msdn.microsoft.com/en-us/library/windows/desktop/dd368826.aspx), и именно там можно посмотреть как она кодируется. Напомню только что BYTE в Windows занимает 1 байт, WORD 2 байта и DWORD 4 соответственно. Размер структуры - #x28 байт.

<pre><code data-language="scheme">
(define pfd (raw type-vector-raw '(#x28 00  1  00  #x25 00 00 00 00 #x10 00 00 00 00 00 00
                                               00 00 00 00 00 00 00 #x10 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00)))
(define hDC (GetDC window))
(SetPixelFormat hDC (ChoosePixelFormat hDC pfd) pfd)

(define hRC (wglCreateContext hDC))
</code></pre>


#### <a name="loop"></a>Цикл обработки сообщений и рендеринг

   Все, окно создано, сконфигурировано и готово к работе. Теперь можно запускать основной цикл рендеринга.

   Но рендеринг нам обязательно надо совместить с циклом обработки сообщений окна, так как иначе окно у нас "зависнет" и по прошествии некоторого времени будет убито системой.
   
   Этот цикл мы оформим рекурсивной лябдой с простым названием loop. Напомню, что ol превосходно справляется с хвостовой рекурсией и никаких переполнений стека, даже если наш цикл будет работать непрерывно неделями, не будет.
   
   Разобъем цикл на две части. В первой части будем обрабатывать оконные сообщения до тех пор, пока они есть в очереди (что оформим вложенной рекурсивной лямбдой, естественно). После чего очистим окно с помощью OpenGL и попросим его вывести на дисплей.

##### <a name="loop-linux"></a>Linux

   Функция XPending сообщит нам, есть ли в очереди окна ожидающие на обработку сообщения. XNextEvent сложит это сообщение в XEvent переменную. glXMakeCurrent активирует контекст OpenGL позволяя нам рисовать (в данном случае очистить окно с помощью заданного по умолчанию цвета).

<pre><code data-language="scheme">
(let ((XEvent (raw type-vector-raw (repeat 0 192))))
(let loop ()
   (let process-events ()
      (if (> (XPending display) 0)
         (begin
            (XNextEvent display XEvent)
            (process-events))))

   (glXMakeCurrent display window cx)
   (glClear GL_COLOR_BUFFER_BIT)

   (glXSwapBuffers display window)
   (glXMakeCurrent display null null)
(loop)))
</code></pre>

##### <a name="loop-win32"></a>Windows

   В отличие от Linux кода мы не можем проигнорировать полученные с помощью PeekMessage оконные сообщения. Их в обязательном порядке надо обработать с помощью TranslateMessage и отправить возможным клиентам через DispatchMessage. А в остальном код идентичен.

<pre><code data-language="scheme">
(let ((MSG (raw type-vector-raw (repeat 0 28))))
(let loop ()
   (let process-events ()
      (if (= 1 (PeekMessage MSG '() 0 0 PM_REMOVE))
         (begin
            (TranslateMessage MSG)
            (DispatchMessage MSG)
            (process-events))))

   (wglMakeCurrent hDC hRC)
   (glClear GL_COLOR_BUFFER_BIT)

   (SwapBuffers hDC)
   (wglMakeCurrent '() '())
(loop)))
</code></pre>

   Все, на этом платформозависимая часть работы закончена.


#### <a name="all"></a>Объединение всего вышеперечисленного в один платформонезависимый код

   Вышеприведенный код, для упрощения дальнейшего туториала, оформлен отдельной билиотекой и помещен в lib/opengl.scm. Это универсальная мультиплатформенная библиотека, которая позволит писать код примеров не оглядываясь на операционную систему и одинаково запускать их в любом окружении.

   Код библиотеки несколько усложнен в сравнении с вышеприведенными врезками, но это усложнение не касается принципиальной части, но только импорта нужных функций в зависимости от окружения, в котором библиотека запускается.
   
   Таким образом, платформонезависимый код, который выполняет всю вышеперечисленную работу (создает окно и очищает его цветом "по умолчанию"), а также начальную инициализацию OpenGL параметров (цвет, используемый для очистки окна) будет выглядеть так:

<pre><code data-language="scheme">(import (lib opengl))
(gl:run "1. Creating an OpenGL Window"

; init
(lambda ()
   (glShadeModel GL_SMOOTH)
   (glClearColor 0.11 0.11 0.11 1))

; draw
(lambda ()
   (glClear GL_COLOR_BUFFER_BIT)))
</code></pre>

   Функция gl:run принимает в параметры имя окна, функцию начальных параметров OpenGL и цикл рендерера.


#### <a name="triangle></a>Рисуем треугольник в плоскости

   Самый простой способ вывода геометрии в OpenGL - вывод поверхности в виде треугольников с помощью glBegin и glEnd. Вот как у нас будет выглядеть вывод одного плоского треугольника в окно:

<pre><code data-language="scheme">(import (lib opengl))
(gl:run "2. Drawing simple triangle"

; init
(lambda ()
   (glShadeModel GL_SMOOTH)
   (glClearColor 0.11 0.11 0.11 1))

; draw
(lambda ()
   (glClear GL_COLOR_BUFFER_BIT)

   (glColor3f 0.2 0.5 0.2)
   (glBegin GL_TRIANGLES)
      (glVertex2f -0.2 -0.2)
      (glVertex2f +0.2 -0.2)
      (glVertex2f -0.0 +0.3)
   (glEnd)))
</code></pre>


#### Добавляем настоящий 3D

   Но если мы хотим получить настойщий 3D - то придется чуточку поднапрячься. Надо освоить матрицы проекции и трансформации. Научиться их расчитывать и модифицировать.
   
   К счастью, есть путь существенно легче - это библиотека glu и понятие "камеры" (camera). Камера - это, как ни странно, камера. А ваш монитор - киноэкран. И для того, что бы получить нужное нам кино - надо просто правильно разместить камеру.

   Функция первая, (gluPerspective fov aspect np fp), позволяет нам сконфигурировать камеру - задать ее широкоугольность (fov, в градусах), пропорции (aspect, как правило ширина/высота окна), плоскости отсечения (np, ближняя и fp, дальняя) - расстояния при выходе за которые камера не будет видеть предмет. В нашем случае это будет 0.1 и 100 метров соответственно.
   
   Функция вторая, (gluLookAt pos3 eye3 up3), задает где и как сейчас расположена камера - это наша основная функция. Если нам надо по другому рассмотреть сцену - мы просто двигаем по ней камеру (перемещаем ее в нужное нам место). pos3 параметр - это тройка чисел x,y,z координаты положения камеры. eye3 - x,y,z места, куда камера направлена. И up3 - x,y,z вектора, который указывает расположение "верха" камеры; этот вектор не обязательно должен быть единичным и не обязательно ортогональным вектору камера-куда-смотрим, главное - не коллинеарным. А математика вектор сама автоматически поправит.
   
   Итак, вот полный код следующего примера:
   

#### Добавляем вращение сцены

...


#### Финальная фаза - рисуем  график функции

...


#### OpenGL 2.0

   И в качестве заготовки для тех, кто хочет использовать современные возможности OpenGL, такие как шейдеры, пример, который рисует на экране процедурно сгенерированные "Deep Sky" объекты (глубокий космос).

<pre><code data-language="scheme">(import (lib opengl))
(import (OpenGL version-2-0))

(define width 640)
(define height 480)

(gl:run "7. OpenGL 2.0" 640 480

; init
(lambda ()
   (glShadeModel GL_SMOOTH)
   (glClearColor 0.11 0.11 0.11 1)

(let ((po (glCreateProgram))
      (vs (glCreateShader GL_VERTEX_SHADER))
      (fs (glCreateShader GL_FRAGMENT_SHADER)))
   (print "po: " po)
   (print "vs: " vs)

   ; пример, как можно передать в функцию массив указателей на строки:
   ; vertex shader:
   (glShaderSource vs 2 (list (c-string "#version 120 // OpenGL 2.1\n")
                              (c-string "
      void main() {
         gl_Position = gl_Vertex;
      }")) null)
   (glCompileShader vs)
   (let ((isCompiled (raw type-vector-raw '(0))))
      (glGetShaderiv vs GL_COMPILE_STATUS isCompiled)

      (if (= (ref isCompiled 0) 0)
         (let*((maxLength "??")
               (_ (glGetShaderiv vs GL_INFO_LOG_LENGTH maxLength))
               (maxLengthValue (+ (ref maxLength 0) (* (ref maxLength 1) 256)))
               (errorLog (make-string maxLengthValue 0))
               (_ (glGetShaderInfoLog vs maxLengthValue maxLength errorLog)))
            (runtime-error errorLog))))
   (glAttachShader po vs)

   ; fragment shader:
   (glShaderSource fs 2 (list (c-string "#version 120 // OpenGL 2.1")
                              (c-string "
      // http://glslsandbox.com/e#19102.0
      uniform float time;

      #define iterations 14
      #define formuparam 0.530

      #define volsteps 18
      #define stepsize 0.2

      #define zoom   0.800
      #define tile   0.850
      #define speed  0.0001

      #define brightness 0.0015
      #define darkmatter 0.400
      #define distfading 0.760
      #define saturation 0.800

      void main(void) {
         vec2 viewport = vec2(1280, 720);

         //get coords and direction
         vec2 uv=gl_FragCoord.xy / viewport.xy - .5;
         uv.y*=viewport.y/viewport.x;
         vec3 dir=vec3(uv*zoom,1.);

         float a2=speed+.5;
         float a1=0.0;
         mat2 rot1=mat2(cos(a1),sin(a1),-sin(a1),cos(a1));
         mat2 rot2=rot1;//mat2(cos(a2),sin(a2),-sin(a2),cos(a2));
         dir.xz*=rot1;
         dir.xy*=rot2;

         vec3 from=vec3(-0.05, 0.05, 0);
         //from.x-=time; <- movement

         from.z = time / 20000.0;

         from.x-=0.2;//mouse.x;
         from.y-=0.7;//mouse.y;

         from.xz*=rot1;
         from.xy*=rot2;

         //volumetric rendering
         float s=.4,fade=.2;
         vec3 v=vec3(0.4);
         for (int r=0; r<volsteps; r++) {
      	vec3 p=from+s*dir*.5;
      	p = abs(vec3(tile)-mod(p,vec3(tile*2.))); // tiling fold
      	float pa,a=pa=0.;
      	for (int i=0; i<iterations; i++) {
      		p=abs(p)/dot(p,p)-formuparam; // the magic formula
      		a+=abs(length(p)-pa); // absolute sum of average change
      		pa=length(p);
      	}
      	float dm=max(0.,darkmatter-a*a*.001); //dark matter
      	a*=a*a*2.; // add contrast
      	if (r>3) fade*=1.-dm; // dark matter, don't render near
      	//v+=vec3(dm,dm*.5,0.);
      	v+=fade;
      	v+=vec3(s,s*s,s*s*s*s)*a*brightness*fade; // coloring based on distance
      	fade*=distfading; // distance fading
      	s+=stepsize;
         }
         v=mix(vec3(length(v)),v,saturation); //color adjust
         gl_FragColor = vec4(v*.01,1.);

      }")) null)
   (glCompileShader fs)
   (let ((isCompiled (raw type-vector-raw '(0))))
      (glGetShaderiv fs GL_COMPILE_STATUS isCompiled)

      (if (= (ref isCompiled 0) 0)
         (let*((maxLength "??")
               (_ (glGetShaderiv fs GL_INFO_LOG_LENGTH maxLength))
               (maxLengthValue (+ (ref maxLength 0) (* (ref maxLength 1) 256)))
               (errorLog (make-string maxLengthValue 0))
               (_ (glGetShaderInfoLog fs maxLengthValue maxLength errorLog)))
            (runtime-error errorLog))))
   (glAttachShader po fs)

   (glLinkProgram po)
   (glDetachShader po fs)
   (glDetachShader po vs)


   (glShadeModel GL_SMOOTH)
   (glClearColor 0.11 0.11 0.11 1)

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)

   (list po)))

; draw
(lambda (po)
   (let ((time (glGetUniformLocation po (c-string "time")))
         (resolution (glGetUniformLocation po (c-string "resolution"))))

   (glClear GL_COLOR_BUFFER_BIT)
   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)

   (glUseProgram po)
   (let* ((ss ms (clock)))
      (glUniform1f time (+ (/ ms 1000) (mod ss 3600)))) ; раз в час будем сбрасывать период
   (if (> resolution 0)
      (glUniform2f resolution width height))

   (glBegin GL_TRIANGLE_STRIP)
      (glVertex2f -1 -1)
      (glVertex2f +1 -1)
      (glVertex2f -1 +1)
      (glVertex2f +1 +1)
   (glEnd))

(list po)))
</code></pre>
