#!/bin/ol

;(import (owl primop))
;(exec "curl http://google.com")

;(define sx12 (lambda () (if (syscall 2000 0 0 0) 1 2)))
;(syscall 2000 3 3 3)

;(syscall 2000 (sx12) type size)
(define width 640)
(define height 480)

(import (owl pinvoke) (owl io)
   (lib x11)
   (OpenGL version-2-0))

(define OR (lambda args (fold bor 0 args)))

;(main)
(define dpy (XOpenDisplay 0))
(define s (XDefaultScreen dpy))

(define vi (glXChooseVisual dpy s
   (raw type-vector-raw '(
      4 0 0 0 ; GLX_RGBA
      5 0 0 0  1 0 0 0 ; GLX_DOUBLEBUFFER
      8 0 0 0  1 0 0 0 ; GLX_RED_SIZE
      9 0 0 0  1 0 0 0 ; GLX_GREEN_SIZE
     10 0 0 0  1 0 0 0 ; GLX_BLUE_SIZE
   
      0 0 0  0  )))); None
(define cx (glXCreateContext dpy vi 0 1))

(define win (XCreateSimpleWindow dpy (XRootWindow dpy s)
   0 0 width height 1
   (XBlackPixel dpy s) (XWhitePixel dpy s)))
   
(XSelectInput dpy win (OR ExposureMask KeyPressMask))
(XMapWindow dpy win)

;(help)
(define (file->string path)
   (bytes->string
      (vec-iter
         (let ((vec (file->vector path)))
            (if vec vec
               (error "Unable to load: " path))))))


;(init)
(glXMakeCurrent dpy win cx)

(define po (glCreateProgram))
(print "po: " po)

(define vs (glCreateShader GL_VERTEX_SHADER))
(print "vs: " vs)


; пример, как можно передать в функцию массив указателей на строки:
(glShaderSource vs 2 (tuple (c-string "#version 120 // OpenGL 2.1\n")
                            (c-string "
    void main() {
        gl_Position = gl_Vertex; // - vec4(1.0, 1.0, 0.0, 0.0); // gl_ModelViewMatrix * gl_Vertex
    }")) null)
(glCompileShader vs)
  (define isCompiled "word")
  (glGetShaderiv vs GL_COMPILE_STATUS isCompiled)

  (if (= (ref isCompiled 0) 0)
    (begin
      (define maxLength "word")
      (glGetShaderiv vs GL_INFO_LOG_LENGTH maxLength)
      (define maxLengthValue (+ (ref maxLength 0) (* (ref maxLength 1) 256)))
      (define errorLog (make-string maxLengthValue 0))
      (glGetShaderInfoLog vs maxLengthValue maxLength errorLog)
      (print errorLog)
      (exit-owl 0)))
(glAttachShader po vs)

(define fs (glCreateShader GL_FRAGMENT_SHADER))
(glShaderSource fs 1 (tuple (c-string "
    #version 120 // OpenGL 2.1
    //	http://glslsandbox.com/e#19102.0
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
(glClearColor 0 0 0 1.0)

(glXMakeCurrent dpy null null)



(define XEvent (raw type-vector-raw (repeat 0 192)))

(let loop ()
   (let process-events ()
      (if (> (XPending dpy) 0)
         (begin
            (print "xevent got")
            (XNextEvent dpy XEvent)
            (process-events))))

      (glXMakeCurrent dpy win cx)
      (glClear GL_COLOR_BUFFER_BIT)

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

      (glUseProgram 0)
      (glXSwapBuffers dpy win)
      (glXMakeCurrent dpy null null)
   (loop))

(print s)
(print "Ok.")
