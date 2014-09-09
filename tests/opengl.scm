;!
(define (load-library name flag) (sys-prim 30 (c-string name) flag #false))
(define (get-proc-address  type dll name) ; todo: переименовать в get-proc-address ?
   (let ((function (cons type (sys-prim 31 dll (c-string name) #false)))) ; todo: избавиться от (c-string)
      (lambda args
         (sys-prim 32 (cdr function) (car function) args))))
(define (get-proc-address-c type dll name) ; todo: переименовать в get-proc-address ?
; todo: отправлять тип функции третим параметром (sys-prim 31) и в виртуальной машине
; возвращать структуру с (byte-vector адрес-функции адрес-вызыватора-с-соответвующей-конвенцией) 
   (let ((function (cons (bor type 64) (sys-prim 31 dll (c-string name) #false)))) ; todo: переделать 64 во что-то поприятнее
      (lambda args
         (sys-prim 32 (cdr function) (car function) args))))
(define type-handle 45)

; вспомогательный макрос для собрать в кучку все bor
(define OR (lambda list (fold bor 0 list)))

; todo: определить константы возвращаемого типа и использовать их в описании возврата функций
; что-то вроде (define rt-int 1)

; todo: тип для (get-proc-address) - всегда число, добавить в проверку

; для результата, что превышает x00FFFFFF надо использовать type-int+ (?)


; my temporary stubs for opengl (у меня пока ж нет структур и т.д.)
; пример, как можно получить свои собственные функции (если они экспортируются, конечно)
(define kernel32_dll (load-library "kernel32" 0))
  (define GetModuleHandle (get-proc-address type-handle kernel32_dll "GetModuleHandleA"))

;(define _exe (GetModuleHandle 0))
;(define CreateGLWindow (get-proc-address-c type-fix+ _exe "CreateGLWindow"))

; вспомогательные константы (временно, пока не научусь работать с флоатами)
(define FLOAT=1 #x3F800000)
(define FLOAT=0 #x00000000)
(define FLOAT-1 #xBF800000)
(define FLOAT0 #x00000000)
(define FLOAT1 #x3F800000)
(define FLOAT2 #x40000000)
         
; real code
(define user32 (load-library "user32" 0))
  (define IDOK 1)
  (define IDCANCEL 2)

  (define MessageBox (get-proc-address type-fix+ user32 "MessageBoxA"))
    (define MB_OK 0)
    (define MB_OKCANCEL 1)
    (define MB_ICONASTERISK 64)
  (define PeekMessage      (get-proc-address type-fix+ user32 "PeekMessageA"))
    (define PM_REMOVE 1)
  (define TranslateMessage (get-proc-address type-fix+ user32 "TranslateMessage"))
  (define DispatchMessage  (get-proc-address type-fix+ user32 "DispatchMessageA"))
  (define PostQuitMessage  (get-proc-address type-fix+ user32 "PostQuitMessage"))
  ;; давление юры 06/09/2014 в 13:43 - 125/ 91
  ;;                           14.07 - 130/101 (после чашки кофе, голова пре-болеть перестала)
  (define GetKeyState      (get-proc-address type-fix+ user32 "GetKeyState"))
  (define GetAsyncKeyState (get-proc-address type-fix+ user32 "GetAsyncKeyState"))
  (define GetKeyboardState (get-proc-address type-fix+ user32 "GetKeyboardState"))
  
  ;; функции работы с win32 окнами
  (define CreateWindowEx   (get-proc-address type-handle user32 "CreateWindowExA")) ; ANSI version
    (define WS_EX_APPWINDOW      #x00040000)
    (define WS_EX_WINDOWEDGE     #x00000100)
    (define WS_OVERLAPPEDWINDOW  (OR #x00000000 #x00C00000 #x00080000 #x00040000 #x00020000 #x00010000))
    (define WS_CLIPSIBLINGS      #x04000000)
    (define WS_CLIPCHILDREN      #x02000000)
  (define DestroyWindow    (get-proc-address type-fix+   user32 "DestroyWindow"))
    
  (define GetDC               (get-proc-address type-handle user32 "GetDC"))
  (define ReleaseDC           (get-proc-address type-fix+   user32 "ReleaseDC"))
  (define ShowWindow          (get-proc-address type-fix+   user32 "ShowWindow"))
    (define SW_SHOW 5)
  (define SetForegroundWindow (get-proc-address type-fix+   user32 "SetForegroundWindow"))
  (define SetFocus            (get-proc-address type-fix+   user32 "SetFocus"))
  
  
  
(define gdi32 (load-library "gdi32" 0))
  (define ChoosePixelFormat (get-proc-address type-fix+ gdi32 "ChoosePixelFormat"))
  (define SetPixelFormat    (get-proc-address type-fix+ gdi32 "SetPixelFormat"))
  (define SwapBuffers       (get-proc-address type-fix+ gdi32 "SwapBuffers"))


(define opengl32 (load-library "opengl32" 0))
  (define wglCreateContext  (get-proc-address type-handle opengl32 "wglCreateContext"))
  (define wglMakeCurrent    (get-proc-address type-fix+   opengl32 "wglMakeCurrent"))
  (define wglDeleteContext  (get-proc-address type-fix+   opengl32 "wglDeleteContext"))
  (define wglGetProcAddress (get-proc-address type-handle opengl32 "wglGetProcAddress"))
    (define (wgl-proc-address type name)
      (let ((function (cons type (wglGetProcAddress (c-string name)))))
        (lambda args
          (sys-prim 32 (cdr function) (car function) args))))

  
  (define glClear           (get-proc-address type-fix+ opengl32 "glClear"))
    (define GL_COLOR_BUFFER_BIT #x00004000)
    (define GL_DEPTH_BUFFER_BIT #x00000100)
  (define glLoadIdentity    (get-proc-address type-fix+ opengl32 "glLoadIdentity"))
  (define glViewport        (get-proc-address type-fix+ opengl32 "glViewport"))
  (define glMatrixMode      (get-proc-address type-fix+ opengl32 "glMatrixMode"))
    (define GL_PROJECTION #x1701)
    (define GL_MODELVIEW  #x1700)
  (define glTranslatef      (get-proc-address type-fix+ opengl32 "glTranslatef"))

  (define glShadeModel      (get-proc-address type-fix+ opengl32 "glShadeModel"))
    (define GL_SMOOTH #x1D01)
  (define glClearColor      (get-proc-address type-fix+ opengl32 "glClearColor"))
  (define glHint            (get-proc-address type-fix+ opengl32 "glHint"))
    (define GL_PERSPECTIVE_CORRECTION_HINT #x0C50)
    (define GL_NICEST #x1102)


  ; https://www.opengl.org/sdk/docs/man2/xhtml/glColor.xml
  (define glColor3i         (get-proc-address type-fix+ opengl32 "glColor3i"))
  (define glColor3ub        (get-proc-address type-fix+ opengl32 "glColor3ub"))
  (define glVertex2i        (get-proc-address type-fix+ opengl32 "glVertex2i"))
  (define glVertex3i        (get-proc-address type-fix+ opengl32 "glVertex3i"))
  (define glBegin           (get-proc-address type-fix+ opengl32 "glBegin"))
    (define GL_TRIANGLES      #x0004)
    (define GL_TRIANGLE_STRIP #x0005) ; http://www.uraldev.ru/articles/35/page/4
  (define glEnd             (get-proc-address type-fix+ opengl32 "glEnd"))

; проверка, что все запустилось.

;(if (=
;  (MessageBox 0 "Please, press OK for test pass!" (c-string "load-library test")
;    (bor MB_OKCANCEL MB_ICONASTERISK))
;  IDOK)
;    (print "OK")
;    (print "CANCEL"))
; todo: вроде бы все строки и так заканчиваются на '\0' - проверить
;(define echo "echo server")

; в момент импорта сделать все нужные привязки
; export (MessageBox)  и т.д.

(define width 1280)
(define height 720)
(define window (CreateWindowEx
    (OR WS_EX_APPWINDOW WS_EX_WINDOWEDGE) "#32770" "OL OpenGL Sample 0" ; #32770 is for system classname for DIALOG
    (OR WS_OVERLAPPEDWINDOW WS_CLIPSIBLINGS WS_CLIPCHILDREN)
    0 0 width height ; x y width height
    0 ; no parent window
    0 ; no menu
    0 ; instance
    0)) ; don't pass anything to WM_CREATE
    
; PIXELFORMATDESCRIPTOR
(define pfd (list->vector '(#x28 00  1  00  #x25 00 00 00 00 #x10 00 00 00 00 00 00
                                              00 00 00 00 00 00 00 #x10 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00)))                        
(define hDC (GetDC window))
(define PixelFormat (ChoosePixelFormat hDC pfd))
(SetPixelFormat hDC PixelFormat pfd)
(define hRC (wglCreateContext hDC))

(wglMakeCurrent hDC hRC)

;(sys-prim 33 #false #false #false)

;  ; opengl 1.2 https://www.opengl.org/registry/api/GL/glext.h
  (define glCreateShader    (wgl-proc-address type-fix+ "glCreateShader"))
    (define GL_VERTEX_SHADER   #x8B31)
    (define GL_FRAGMENT_SHADER #x8B30)
  (define glShaderSource    (wgl-proc-address type-fix+ "glShaderSource"))
  (define glCompileShader   (wgl-proc-address type-fix+ "glCompileShader"))
  (define glCreateProgram   (wgl-proc-address type-fix+ "glCreateProgram"))
  (define glAttachShader    (wgl-proc-address type-fix+ "glAttachShader"))
  (define glDetachShader    (wgl-proc-address type-fix+ "glDetachShader"))
  (define glLinkProgram     (wgl-proc-address type-fix+ "glLinkProgram"))
  (define glUseProgram      (wgl-proc-address type-fix+ "glUseProgram"))
  (define glGetShaderiv     (wgl-proc-address type-fix+ "glGetShaderiv"))
    (define GL_COMPILE_STATUS #x8B81)
  (define glGetUniformLocation (wgl-proc-address type-fix+ "glGetUniformLocation"))
    (define glUniform1i     (wgl-proc-address type-fix+ "glUniform1i"))
  (define glEnableVertexAttribArray (wgl-proc-address type-fix+ "glEnableVertexAttribArray"))
  (define glVertexAttribPointer (wgl-proc-address type-fix+ "glVertexAttribPointer"))
    (define GL_FLOAT #x1406)
  (define glDrawArrays         (wgl-proc-address type-fix+ "glDrawArrays"))


(define po (glCreateProgram))
;(print "po: " po)

(define vs (glCreateShader GL_VERTEX_SHADER))
;(print "vs: " vs)
(glShaderSource vs 1 (tuple (c-string "#version 120 // OpenGL 2.1
	void main()
	{
		gl_Position = gl_Vertex - vec4(1.0, 1.0, 0.0, 0.0); // gl_ModelViewMatrix * gl_Vertex
	}")) 0)
(glCompileShader vs)
(glAttachShader po vs)

;; полезные шейдеры:
;;  http://glslsandbox.com/e#19171.3 - цифровое табло
(define fs (glCreateShader GL_FRAGMENT_SHADER))
;(print "fs: " fs)
(glShaderSource fs 1 (tuple (c-string "#version 120 // OpenGL 2.1
	// http://glslsandbox.com/e#19291.0
	uniform int time2;
	
/*	const int MAXITER = 80;
	vec3 field(vec3 p) {
		p *= 0.1;
		float f = 0.1;
		for (int i = 0; i < 5; i++) {
			p = p.yzx * mat3(0.8, 0.6, 0.0,-0.6, 0.8, 0.0, 0.0, 0.0, 1.0);
			p += vec3(0.123, 0.456, 0.789) * float(i);
			p = abs(fract(p) - 0.5);
			p *= 2.0;
			f *= 2.0;
		}
		p *= p;
		return sqrt(p + p.yzx) / f - 0.002;
	}
	
	void main(void) {
		vec2 viewport = vec2(800.0, 600.0);
//		vec2 position = gl_FragCoord.xy / viewport.xy;
		float time = time2 / 1000.0;
	
		vec3 dir = normalize(vec3((gl_FragCoord.xy - viewport * 0.5) / viewport.x, 1.0));
		vec3 pos = vec3(0.5, 0.0, time);
		vec3 color = vec3(0.0);
		for (int i = 0; i < MAXITER; i++) {
			vec3 f2 = field(pos);
			float f = min(min(f2.x, f2.y), f2.z);
			
			pos += dir * f;
			color += float(MAXITER - i) / (f2 + 0.001);
		}
		vec3 color3 = vec3(0.17 / (color * (0.09 / float (MAXITER*MAXITER))));
		color3 *= color3;
		gl_FragColor = 1.0 - vec4(color3.zyx, 0.0);*/
		
//	http://glslsandbox.com/e#19102.0
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
		float time = time2 / 10000000.0;
		
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
		
		from.z = time;
		
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

	}")) 0)
(glCompileShader fs)
(glAttachShader po fs)

(glLinkProgram po)
(glDetachShader po fs)
(glDetachShader po vs)

  (define time (glGetUniformLocation po "time2"))

;(print "glGetUniformLocation: " (glGetUniformLocation po "color"))
;(sys-prim 32 (cdr function) (car function) args))))

  ; todo: проверить возвращаемый результат


(ShowWindow window SW_SHOW)
(SetForegroundWindow window)
(SetFocus window)

; ResizeGLScene
(glViewport 0 0 width height)

(glMatrixMode GL_PROJECTION)
(glLoadIdentity)

(glMatrixMode GL_MODELVIEW)
(glLoadIdentity)
;(glTranslatef FLOAT-1 FLOAT-1 0)

(glShadeModel GL_SMOOTH)
(glClearColor 0 0 0 FLOAT=1)
;(glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST)

;(glClearColor 0 0 FLOAT=1 0)

;(WinMain 0 0 0 0)
(define vertexPositions (list->vector '(
;        (glVertex2i 2 0)
  00 00 #x00 #x40    0 0 #x00 #x00    0 0 0 0    00 00 #x80 #x3F
;        (glVertex2i 1 2)
  00 00 #x80 #x3F    0 0 #x00 #x40    0 0 0 0    00 00 #x80 #x3F
;        (glVertex2i 0 0)
  00 00 #x00 #x00    0 0 #x00 #x00    0 0 0 0    00 00 #x80 #x3F
)))

(define MSG (make-vector 28 0)) ; sizeof(MSG)=28
;(call/cc (lambda (return)
(define (cycle)   ;MSG
  (if (= 1 (PeekMessage MSG 0 0 0 PM_REMOVE))
    (begin  
      (TranslateMessage MSG)
      (DispatchMessage MSG))
      
    (begin ; DrawGLScene
      (glClear GL_COLOR_BUFFER_BIT)

      (glUseProgram po)
      (let* ((ss ms (clock)))
        ;(glUniform1i time (+ ms (* 1000 (mod ss 3600))))) ; раз в час будем сбрасывать период
        (glUniform1i time (+ ms (* 1000 (mod ss 3600))))) ; раз в час будем сбрасывать период
      
      (glBegin GL_TRIANGLE_STRIP)
        (glVertex3i 0 0 0)
        (glVertex3i 2 0 0)
        (glVertex3i 0 2 0)
        (glVertex3i 2 2 0)
      (glEnd)

;      (glEnableVertexAttribArray 0)
;      (glVertexAttribPointer 0 4 GL_FLOAT 0 0 vertexPositions)
;      (glDrawArrays GL_TRIANGLES 0 3)

      (glUseProgram 0)
      
      (SwapBuffers hDC)))
  (if (= (GetAsyncKeyState 27) 0) (cycle)))
(cycle)

; KillGLWindow
(wglMakeCurrent 0 0)
(wglDeleteContext hRC)
(ReleaseDC window hDC)
(DestroyWindow window)

(print "@")
