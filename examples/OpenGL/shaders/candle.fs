// http://glslsandbox.com/e#24121.0
//420blaz&

#ifdef GL_ES
precision mediump float;
#endif

uniform float time;
uniform vec2 mouse;
uniform vec2 resolution;

void main( void ) {

   vec2 pos = ( gl_FragCoord.xy / resolution.xy )*8.-vec2(4.,5.);// + mouse / 4.0;
   pos.x = (pos.x/.88);
   if(pos.y>-2.*4.20){
      for(float baud = 1.; baud < 32.; baud += 1.){
         pos.y += 0.2*sin(16.*4.555*time/(1.+baud))/(1.+baud*32.);
         pos.x += 0.03*cos(2.*time/(1.+0.01*baud*cos(baud*baud)+pos.y/4e3))/(1.+baud);
      }
      //0.13*cos(time*2.+0.6)+.1*sin(time*3.+0.4)+
      
      pos.y += 0.04*fract(sin(time*60.));
   }

   pos.x = abs(pos.x)+0.0667; //left side was missing
   vec3 color = vec3(0.,0.,0.0);
   
   float p =.004;
   float y = -pow(pos.x,4.20)/(4.20*p)*4.20;
   float dir = length(pos-vec2(pos.x,y))*sin(0.3);//*(0.01*sin(time)+0.07);
   if(dir < 0.7){
      color.rg += smoothstep(0.0,1.,.75-dir);
      color.g /=2.4;
   }
   color *= (0.2+abs((pos.y/4.2+4.2))/4.2);
   color += 0.88*pow(color.r,1.88);
   color *= sin(1.7+abs(pos.y)*0.4);
   
   pos.y += 1.5;
   vec3 dolor = vec3(0.,0.,0.0);
   y = -pow(pos.x,4.20)/(4.20*p)*4.20;
   dir = length(pos-vec2(pos.x,y))*sin(0.3);
   if(dir < 0.7){
      dolor.bg += smoothstep(0.0,1.,.75-dir);
      dolor.g /=1.88;
   }
   dolor *= (0.2+abs((pos.y/4.2+4.2))/4.2);
   dolor += pow(color.b,1.1);
   dolor *= cos(-0.6+pos.y*0.4);
   //dolor.rgb -= pow(length(dolor)/16., 0.5);
   color = (color*0.5+dolor)/1.5;
   
   gl_FragColor = vec4(vec3(color) , 1.0 );

}