---
layout: page
title:  OpenGL tutorials
date:   пт, 27-лис-2015 19:39:47 +0200
categories: ru
---
   Для демонстрации возможностей Ol вполне подойдет OpenGL. Я приведу пример полного цикла разработки своего мультиплатформенного приложения на базе Ol без привлечения других языков и/или инструментов. Сами примеры можно взять в [официальном репозитарии](https://github.com/yuriy-chumak/OL/tree/master/tutorial/OpenGL){:target="_blank"} Ol.

   Демонстрация будет разбита на несколько последовательных частей, в результате которых мы научимся создавать трехмерный график некоторой функции.


#### Создание окна

   Для вывода чего-либо на дисплей нам, естественно, надо для этого попросить у операционной системы место, куда выводить свое творчество. Такое место называется "окно". После того, как система его нам выделит, надо разместить это окно на экране. Этот процесс, хотя и является общим для всех операционных систем, все же специфичен для каждой из них и требует вызова разных функций. Далее продемонстрировано, как это можно сделать.

##### Linux

   Биндинги для Linux X11 сложены в библиотеку lib/x11, подключим ее:
<pre><code data-language="scheme">(import (lib x11))
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

##### Windows

   Биндинги для Windows API сложены в библиотеку lib/winapi, подключим ее:
<pre><code data-language="scheme">(import (lib winapi))
</code></pre>

   Теперь мы создаем окно (в отличие от Linux, нам не надо выбирать дисплей и рабочий стол, Windows все сделает вместо нас).

<pre><code data-language="scheme">
(define window (CreateWindowEx
   (OR WS_EX_APPWINDOW WS_EX_WINDOWEDGE) "#32770" "OL OpenGL Sample 0" ; #32770 is for system classname for DIALOG
   (OR WS_OVERLAPPEDWINDOW WS_CLIPSIBLINGS WS_CLIPCHILDREN)
   0 0 width height ; x y width height
   null ; no parent window
   null ; no menu
   null ; instance
   null)) ; todo: override as '(INTEGER . 0)
</code></pre>

   Детальное описание всех функций Windows можно найти в официальной документации (MSDN).


#### Инициализация OpenGL контекста

   Теперь нам надо выбрать конфигурацию оборудования для дальнейшего рендеринга. Здесь мы зададим 24-канальный цвет, двойную буферизацию и 24-битный буфер глубины.

   Создадим "контекст" выполнения графической подсистемы:

##### Linux

<pre><code data-language="scheme">
(define vi (glXChooseVisual display screen
   (raw type-vector-raw '(
      4 0 0 0 ; GLX_RGBA
      5 0 0 0  1 0 0 0 ; GLX_DOUBLEBUFFER
      8 0 0 0  1 0 0 0 ; GLX_RED_SIZE
      9 0 0 0  1 0 0 0 ; GLX_GREEN_SIZE
     10 0 0 0  1 0 0 0 ; GLX_BLUE_SIZE
     12 0 0 0  1 0 0 0 ; GLX_DEPTH_SIZE

      0 0 0 0)))); None
(define cx (glXCreateContext display vi 0 1))
</code></pre>

##### Windows

<pre><code data-language="scheme">
(define pfd (raw type-vector-raw '(#x28 00  1  00  #x25 00 00 00 00 #x10 00 00 00 00 00 00
                                               00 00 00 00 00 00 00 #x10 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00)))
(define hDC (GetDC window))
(SetPixelFormat hDC (ChoosePixelFormat hDC pfd) pfd)

(define hRC (wglCreateContext hDC))
</code></pre>


#### Цикл обработки сообщений и рендеринг

   Все, окно создано и сконфигурировано. Теперь можно запускать цикл рендеринга.

   Рендеринг нам надо совместить с циклом обработки сообщений окна, так как иначе окно у нас "зависнет" и по прошествии некоторого времени будет убито системой, этот цикл мы поместим в лямбду (loop).

##### Linux

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

##### Windows

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

   Как видите, циклы обработки довольно похожи. Все, на этом платформозависимая часть работы закончена.


#### Объединение всего вышеперечисленного в один платформонезависимый код

   Вышеприведенный код, для упрощения дальнейшего туториала, оформлен отдельной билиотекой и помещен в lib/linux/opengl.scm для Linux и lib/windows/opengl.scm для Windows. В дальнейшем коде надо воспринимать (import lib opengl) как (import lib linux opengl) или (import lib windows opengl) соответственно.

   Таким образом, платформонезависимый пустой код, который создает окно и очищает его цветом "по умолчанию" (то-есть все то, что описано выше) будет выглядеть так:

<pre><code data-language="scheme">(import (lib opengl))
(gl:run "1. Creating an OpenGL Window" 640 480

; init
(lambda ()
   (glShadeModel GL_SMOOTH)
   (glClearColor 0.11 0.11 0.11 1))

; draw
(lambda ()
   (glClear GL_COLOR_BUFFER_BIT)))
</code></pre>

   Функция gl:run принимает в параметры имя окна, его размеры, функцию начальных параметров OpenGL и цикл рендерера.


#### Рисуем треугольник в плоскости

   Вот как у нас будет выглядеть вывод треугольника в окно:

<pre><code data-language="scheme">(import (lib opengl))
(gl:run "2. Drawing simple triangle" 640 480

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

...


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

