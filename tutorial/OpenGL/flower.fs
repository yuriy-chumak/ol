// http://glslsandbox.com/e#28513.0
#ifdef GL_ES
precision mediump float;
#endif

uniform float time;
uniform vec2 mouse;
uniform vec2 resolution;

#define pi 3.1415926535897932384626433832795
#define flyCount 30.


float testFuncFloor(float v){

    const float amplitude=1.;
    const float t=pi*2.;
    float k=4.*amplitude/t;
    float r=mod( v  ,t);
    float d=floor(v /(.5* t) );
    
   return mix(k* r-amplitude ,  amplitude*3.-k* r ,mod(d,2.)  );
}

float getRad(vec2 q){
   return atan(q.y,q.x); 
}

vec2 noise(vec2 tc){
    //return (2.*texture2D(iChannel0, tc).xy-1.).xy;
    return vec2(fract(sin(tc.x) ),fract(sin(tc.y) ) );
}

float firefly(vec2 p,float size){
    
   //return smoothstep(0.,size,dot(p,p)*200. );
   return smoothstep(0.,size,length(p) );

}

const float pow=1.;
const float flySpeed=0.1;


void main( void ) {

    float pow=1.;
    const float duration=1.;
    float t=duration*(1.+sin(3.* time ) );
   vec2 p= gl_FragCoord.xy / resolution.xy;
   
   float ratio= resolution.y/resolution.x;
    
     vec2 uv=p;
    uv.y*=ratio;
    
    
    vec2 flowerP=vec2(.618,0.518);
    vec2 q=p-flowerP-vec2( pow*.008*cos(3.*time) ,pow*.008*sin(3.*time) ) ;
    vec2 rootP=p-+flowerP-vec2( pow*.02*cos(3.*time)*p.y ,-0.48+pow*.008*sin(3.*time) );
   
   q.y*=ratio;
   
    //sky
    vec3 col=mix( vec3(0.1,0.6,0.5), vec3(0.2,0.1,0.2), sqrt(p.y)*.6 );
    

      //draw stem 
    float width=0.01;
    float h=.5;
    float w=.0005;
    col=mix(vec3(.5,.7,.4),col, 1.- (1.- smoothstep(h,h+width, abs(rootP.y ) )  ) * (1.- smoothstep(w,w+width, abs(rootP.x-0.1*sin(4.*rootP.y+pi*.35) ) )  ) );
      
    //draw flower 
    vec3 flowerCol=mix(vec3(.7,.7,.2),vec3(.7,.9,.7), smoothstep( .0,1.,length(q)*10. ) ) ;

    const float edge=.02;
    float r= .1+0.05*( testFuncFloor( getRad( q ) *7.  + 2.*q.x*(t-duration)  )  );

   col=mix(flowerCol,col, smoothstep(r,r+edge,  length( q )  ) );
    
   //draw buds
    float r1=0.;
    r1=.04;
    vec3 budCol=mix (vec3(.3,.4,0.),vec3(.9,.8,0.), length(q)*10. );
   col=mix(budCol,col, smoothstep(r1,r1+0.01,  length( q )  ) );
    
    //draw firefly
   //vec3 flyCol=mix (vec3(.1,.4,0.1),vec3(.1,1.,1.), length(q)*10. );
   
    for (float i=0.;i<flyCount;i++){
      
        float seed=i/flyCount;
   float seed2=fract(i/flyCount*5.);
        float t1=1.*(1.+sin(noise(vec2(seed) ).x* time ) );
         vec2 fireflyP=uv- 
          vec2(noise(vec2(seed2) ).x+noise(vec2(seed2) ).x*t1*flySpeed,
          noise(vec2(seed) ).y+noise(vec2(seed) ).y*t1*flySpeed);
       
      float fly= firefly( fireflyP,.002+.008*seed );
      vec3 flyCol=mix(vec3(0.1,0.9,0.1)*t1,vec3(0.), fly );
         col+=flyCol;
    }
   
    gl_FragColor=vec4(col,1.);
   /*
   vec2 position = ( gl_FragCoord.xy / resolution.xy ) + mouse / 4.0;

   float color = 0.0;
   color += sin( position.x * cos( time / 15.0 ) * 80.0 ) + cos( position.y * cos( time / 15.0 ) * 10.0 );
   color += sin( position.y * sin( time / 10.0 ) * 40.0 ) + cos( position.x * sin( time / 25.0 ) * 40.0 );
   color += sin( position.x * sin( time / 5.0 ) * 10.0 ) + sin( position.y * sin( time / 35.0 ) * 80.0 );
   color *= sin( time / 10.0 ) * 0.5;

   gl_FragColor = vec4( vec3( color, color * 0.5, sin( color + time / 3.0 ) * 0.75 ), 1.0 );
   */
}