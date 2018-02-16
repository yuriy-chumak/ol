// http://glslsandbox.com/?page=89
#ifdef GL_ES
precision highp float;
#endif

uniform float time;
uniform vec2 mouse;
uniform vec2 resolution;

//Show the number of steps taken (green = ~1, red = ~MAX_STEPS)
//#define SHOW_SHADOW_COST

//Enable soft shadows
#define SOFT_SHADOWS

#define MAX_STEPS 96
#define STEP_MULT 0.2
#define MIN_DIST 0.0005
#define SHADOW_HARDNESS 16.0

float tau = atan(1.0)*8.0;

vec2 angleRep(float a, vec2 p)
{
   float pa = atan(p.y,p.x);
   float pl = length(p);
   
   pa = mod(pa - a/2., a) - a/2.;
   
   return vec2(cos(pa),sin(pa))*pl;
}

float opU( float d1, float d2 )
{
    return min(d1,d2);
}

float opS( float d1, float d2 )
{
    return max(-d1,d2);
}

float opI( float d1, float d2 )
{
    return max(d1,d2);
}

float sdCircle(float r, vec2 p)
{
   return length(p) - r;
}

float sdBox(vec2 s, vec2 p)
{
    p = abs(p) - s / 2.0;
    return max(p.x,p.y);
}

float map(vec2 p)
{
   float dist = 1e6;
   
   vec2 p1 = mod(p, vec2(0.25)) - 0.125;
   vec2 p2 = angleRep(tau/12.,p);
   
   float ring = opS(sdCircle(0.275, p), sdCircle(0.325, p));
   ring = opS(sdBox(vec2(0.2,0.075), p2 - vec2(0.3,0.0)), ring);
   
   
   dist = opU(dist, sdCircle(0.1, p));
   dist = opU(dist, sdCircle(0.05, p2 - vec2(0.5,0.0)));
   dist = opU(dist, ring);
   
   return dist;
}

vec3 normal(vec2 p)
{
   vec2 offs = vec2(0.001,0);
   float a = -map(p + offs.xy);
   float b = -map(p - offs.xy);
   float c = -map(p + offs.yx);
   float d = -map(p - offs.yx);
   
   return normalize(cross(vec3(offs.xy*2.0,a-b), vec3(offs.yx*2.0,c-d)));
}

float steps = 0.0;

float shadow(vec2 p, vec2 l)
{
   vec2 dir = normalize(l - p);
   float dist = distance(p, l);
   float t = 0.0;
   float s = 1.0;
   
   for(int i = 0;i < MAX_STEPS;i++)
   {
      float sd = map(p + dir * t);
      
      #ifdef SOFT_SHADOWS
      t += sd * STEP_MULT;
      #else
      t += sd;
      #endif
      
      s = min(s, SHADOW_HARDNESS*sd/t);
      
      steps++;
      
      if(sd < MIN_DIST || t > dist)
      {
         break;   
      }
   }
   
   #ifdef SOFT_SHADOWS
   return (t < dist) ?  0.0 : s;
   #else
   return (t < dist) ?  0.0 : 1.0;
   #endif
}

vec3 PointLight(vec3 col, float bright, vec3 pos, float dist, vec2 p)
{
   float shad = shadow(p, pos.xy);
   float falloff = bright / pow(length(vec3(p, 0) - pos), 2.0);
   float shade = shad;
   if(dist < 0.0) //Only apply normalmapping to objects.
   {
      shade = max(0.0,dot(normal(p), normalize(pos- vec3(p, 0)))) ;
   }
   shade *= falloff;
   return col * shade;
}

void main( void ) 
{
   vec2 aspect = resolution.xy / resolution.y;
   vec2 uv = ( gl_FragCoord.xy / resolution.y ) - aspect/2.0;
   vec2 mo = mouse * aspect - aspect/2.0;
   
   vec3 color = vec3(0.0);
   
   float d = map(uv);
   
   vec3 li1 = vec3(cos(time),sin(time), 0.15);
   li1.xy *= 0.15;
   
   vec3 li2 = vec3(cos(-time),sin(-time), 0.15);
   li2.xy *= 0.4;
   
   vec3 shade = vec3(0.0);
   shade += PointLight(vec3(1,0.6,0.3), 0.03, li1, d, uv);
   shade += PointLight(vec3(0.5,0.7,1), 0.02, li2, d, uv);
   shade += 0.125;
   
   
   vec3 bg = vec3(0.75);
   bg *= smoothstep(-0.02,0.02,d);
   bg *= shade;
   
   vec3 fg = vec3(1.0,0.5,0.0) * smoothstep(0.000,0.003,-map(uv));
   fg *= shade;
   
   color = mix(fg, bg, smoothstep(0.000,0.001,map(uv)));
   
   #ifdef SHOW_SHADOW_COST
   color = mix(vec3(0,1,0),vec3(1,0,0),steps/float(MAX_STEPS));
   #endif
   
   gl_FragColor = vec4( vec3( color ), 1.0 );

}
