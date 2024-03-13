// http://glslsandbox.com/e#31171.0
// PlayingMarble.glsl
// original code from https://www.shadertoy.com/view/MtX3Ws
// simplified edit: Robert 25.11.2015
// see also https://www.shadertoy.com/view/Mlj3zWprecision mediump float;
// modified color calculation by I.G.P.

#ifdef GL_ES
precision mediump float;
#endif

uniform float time;
uniform vec2 resolution;
uniform vec2 mouse;

vec3 roty(vec3 p,float a)
{ return mat3(cos(a),0,-sin(a), 0,1,0, sin(a),0,cos(a)) * p; }

float map(in vec3 p) 
{
   vec3 c=p; float res=0.;
   for (int i=0; i < 4; i++) 
   {
      p= abs(p)/dot(p,p) -.7;
      p.yz= vec2(p.y*p.y-p.z*p.z,2.*p.y*p.z);
      res += exp(-20. * abs(dot(p,c)));
   }
   return res*0.5;
}

vec3 raymarch(vec3 ro, vec3 rd)
{
   float t=5.0;
   vec3 col=vec3(0); float c=0.;
   for( int i=0; i < 32; i++ )
   {
      t+= exp(c*-2.0) *0.02;
      c= map(t *rd +ro);               
      col= vec3(c*c, c, c*c*c) *0.16 + col *0.96; //green
      col= vec3(c*c*c, c*c, c) *0.16 + col *0.96; //blue
      col= vec3(c, c*c*c, c*c) *0.16 + col *0.96; //red

   }
   return col;
}

void main()
{
    vec2 p= (gl_FragCoord.xy - resolution*0.5) / resolution.y;
    vec3 ro= roty(vec3(3.), time*0.1 + mouse.x);
    vec3 uu= normalize(cross(ro, vec3(0.0, 1.0, 0.0)));
    vec3 vv= normalize(cross(uu, ro));
    vec3 rd= normalize(p.x*uu + p.y*vv - ro*0.5 );
    gl_FragColor.rgb= log(raymarch(ro,rd) +1.0) *0.5;
    gl_FragColor.a= 1.0;
}
