---
layout: page
title:  OpenGL tutorials
date:   пт, 27-лис-2015 19:39:47 +0200
categories: ru
---
   Для демонстрации возможностей Ol вполне подойдет OpenGL. Я приведу пример полного цикла разработки своего мультиплатформенного приложения на базе Ol без привлечения других языков и/или инструментов. Сами примеры можно взять в [официальном репозитарии](https://github.com/yuriy-chumak/OL/tree/master/tutorial/OpenGL){:target="_blank"} Ol.

   Демонстрация будет разбита на несколько последовательных частей, в результате которых мы научимся создавать трехмерный график некоторой функции.


#### Создание окна

   Для вывода чего-либо на дисплей нам, естественно, надо для этого попросить у операционной системы место. Такое место называется "окно". После того, как система его нам выделит, надо разместить это окно на экране. Этот процесс, хотя и является общим для всех операционных систем, все же специфичен для каждой из них и требует вызова разных функций. Далее продемонстрировано, как это можно сделать.

##### Linux

   Биндинги для Linux X11 сложены в библиотеку lib/x11, подключим ее:

<pre><code data-language="scheme">
(import (lib x11))
</code></pre>

   Теперь нам надо выбрать дисплей, с которым мы хотим работать (в примере это будет доступный по-умолчанию дисплей) и рабочий стол, на котором мы будем размещать наше окно (в Linux можно размещать на одном дисплее разные рабочие пространства).

<pre><code data-language="scheme">
(define display (XOpenDisplay 0))
(define screen (XDefaultScreen display))

(define window (XCreateSimpleWindow display (XRootWindow display screen)
   0 0 width height 1
   (XBlackPixel display screen) (XWhitePixel display screen)))
(XMapWindow display window)
(XSelectInput display window ExposureMask)
</code></pre>

   Детальное описание всех функций можно найти в официальной документации по X11.

   Теперь нам надо выбрать конфигурацию оборудования для дальнейшего рендеринга. Здесь мы зададим 24-канальный цвет и двойную буферизацию. Создадим "контекст" выполнения графической подсистемы.

<pre><code data-language="scheme">
(define vi (glXChooseVisual display screen
   (raw type-vector-raw '(
      4 0 0 0 ; GLX_RGBA
      5 0 0 0  1 0 0 0 ; GLX_DOUBLEBUFFER
      8 0 0 0  1 0 0 0 ; GLX_RED_SIZE
      9 0 0 0  1 0 0 0 ; GLX_GREEN_SIZE
     10 0 0 0  1 0 0 0 ; GLX_BLUE_SIZE

      0 0 0 0)))); None
(define cx (glXCreateContext display vi 0 1))
</code></pre>

   Все, окно создано и сконфигурировано. Теперь надо проинициализировать "дефолтные" параметры OpenGL, в примере это будет минимум - модель окрашивания примитивов и цвет для очистки окна.

<pre><code data-language="scheme">
(glXMakeCurrent display window cx)

(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

(glXMakeCurrent display null null)
</code></pre>

   На этом вся подготовка закончилась. Теперь надо запустить цикл отрисовки - цикл, в котором мы будем рендерить кадры нашей сцены. В этом примере весь рендеринг - это очистка окна цветом, заданным выше.

<pre><code data-language="scheme">
;(loop)
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

   Этот цикл завершится, когда пользователь закроет окно. Способы обработки клавиатуры, событий мыши выходят за рамки базового примера.

##### Windows
<pre><code data-language="scheme">
(import (OpenGL version-1-0)
   (lib winapi))

(define width 640)
(define height 480)

(define OR (lambda list (fold bor 0 list)))

;(main)
(define window (CreateWindowEx
   (OR WS_EX_APPWINDOW WS_EX_WINDOWEDGE) "#32770" "OL OpenGL Sample 0" ; #32770 is for system classname DIALOG
   (OR WS_OVERLAPPEDWINDOW WS_CLIPSIBLINGS WS_CLIPCHILDREN)
   0 0 width height ; x y width height
   null ; no parent window
   null ; no menu
   null ; instance
   null)) ; todo: override as '(INTEGER . 0)

; PIXELFORMATDESCRIPTOR
(define pfd (raw type-vector-raw '(#x28 00  1  00  #x25 00 00 00 00 #x10 00 00 00 00 00 00
                                                   00 00 00 00 00 00 00 #x10 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00)))
(define hDC (GetDC window))
(define PixelFormat (ChoosePixelFormat hDC pfd))
(print "PixelFormat = " PixelFormat)
(print "SetPixelFormat = "
(SetPixelFormat hDC PixelFormat pfd))

(define hRC (wglCreateContext hDC))

;(init)
(print "wglMakeCurrent = "
(wglMakeCurrent hDC hRC))

(print "OpenGL version: " (glGetString GL_VERSION))
(print "OpenGL vendor: " (glGetString GL_VENDOR)).
(print "OpenGL renderer: " (glGetString GL_RENDERER))

(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

(wglMakeCurrent '() '())

;(show)
(ShowWindow window SW_SHOW)
(SetForegroundWindow window)
(SetFocus window)

;(loop)
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

;(done)
(wglDeleteContext hRC)
(ReleaseDC window hDC)
(DestroyWindow window)

(print "Ok.")
</code></pre>



#### Инициализация OpenGL

...


#### Базовый вывод OpenGL в окно

...


#### Объединение всего вышеперечисленного в один платформонезависимый код

...


#### Рисуем треугольник в плоскости

...


#### Добавляем настоящий 3D

...


#### Добавляем вращение сцены

...


#### Финальная фаза - рисуем  график функции

...


#### OpenGL 2.0

   И в качестве заготовки для тех, кто хочет использовать современные возможности OpenGL, такие как шейдеры, пример, который рисует на экране процедурно сгенерированные "Deep Sky" объекты (глубокий космос).

<pre><code data-language="scheme">
#!/usr/bin/ol

(import (OpenGL version-2-0)
   (lib x11) (owl io))

(define width 640)
(define height 480)

;(main)
(define display (XOpenDisplay 0))
(define screen (XDefaultScreen display))

(define vi (glXChooseVisual display screen
   (raw type-vector-raw '(
      4 0 0 0 ; GLX_RGBA
      5 0 0 0  1 0 0 0 ; GLX_DOUBLEBUFFER
      8 0 0 0  1 0 0 0 ; GLX_RED_SIZE
      9 0 0 0  1 0 0 0 ; GLX_GREEN_SIZE
     10 0 0 0  1 0 0 0 ; GLX_BLUE_SIZE

      0 0 0 0)))); None
(define cx (glXCreateContext display vi 0 1))

(define window (XCreateSimpleWindow display (XRootWindow display screen)
   0 0 width height 1
   (XBlackPixel display screen) (XWhitePixel display screen)))
   
(XSelectInput display window ExposureMask)
(XMapWindow display window)


;(init)
(glXMakeCurrent display window cx)

(define po (glCreateProgram))
(print "po: " po)

(define vs (glCreateShader GL_VERTEX_SHADER))
(print "vs: " vs)

; пример, как можно передать в функцию массив указателей на строки:
(glShaderSource vs 2 (list (c-string "#version 120 // OpenGL 2.1\n")
                           (c-string "
    void main() {
        gl_Position = gl_Vertex; // - vec4(1.0, 1.0, 0.0, 0.0); // gl_ModelViewMatrix * gl_Vertex
    }")) null)
(glCompileShader vs)
  (define isCompiled (raw type-vector-raw '(0)))

  (glGetShaderiv vs GL_COMPILE_STATUS isCompiled)

  (if (= (ref isCompiled 0) 0)
    (begin
      (define maxLength "??")
      (glGetShaderiv vs GL_INFO_LOG_LENGTH maxLength)
      (define maxLengthValue (+ (ref maxLength 0) (* (ref maxLength 1) 256)))
      (define errorLog (make-string maxLengthValue 0))
      (glGetShaderInfoLog vs maxLengthValue maxLength errorLog)
      (print errorLog)
      (exit-owl 0)))
(glAttachShader po vs)


(define fs (glCreateShader GL_FRAGMENT_SHADER))
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
  (define isCompiled "word")
  (glGetShaderiv fs GL_COMPILE_STATUS isCompiled)

  (if (= (ref isCompiled 0) 0)
    (begin
      (define maxLength "word")
      (glGetShaderiv fs GL_INFO_LOG_LENGTH maxLength)
      (define maxLengthValue (+ (ref maxLength 0) (* (ref maxLength 1) 256)))
      (define errorLog (make-string maxLengthValue 0))
      (glGetShaderInfoLog fs maxLengthValue maxLength errorLog)
      (print errorLog)
      (print "@")
      (exit-owl 0)))
(glAttachShader po fs)

(glLinkProgram po)
(glDetachShader po fs)
(glDetachShader po vs)

  (define time (glGetUniformLocation po (c-string "time")))
  (define resolution (glGetUniformLocation po (c-string "resolution")))


(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

(glMatrixMode GL_PROJECTION)
(glLoadIdentity)

(glXMakeCurrent display null null)

;(loop)
(let ((XEvent (raw type-vector-raw (repeat 0 192))))
(let loop ()
   (let process-events ()
      (if (> (XPending display) 0)
         (begin
            (XNextEvent display XEvent)
            (process-events))))

      (glXMakeCurrent display window cx)
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
      (glEnd)


   (glXSwapBuffers display window)
   (glXMakeCurrent display null null)

(loop)))

;(done)
(print "Ok.")
</code></pre>

