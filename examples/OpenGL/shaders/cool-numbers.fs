// http://glslsandbox.com/e#28898.1
#ifdef GL_ES
precision highp float;
#endif

uniform float time;
uniform vec2 mouse;
uniform vec2 resolution;

//WIP Nixie tube like digits.
//Designed using AutoCAD and converted to glsl using netDXF and C#.

/*
TODO:
-Optimisations
*/

#define IS_THUMBNAIL (resolution == vec2(200,100))

float pi = atan(1.0)*4.0;
float tau = atan(1.0)*8.0;

const float scale = 1.0 / 3.0;

vec2 digitSize = vec2(1.0,1.5) * scale;
vec2 digitSpacing = vec2(1.1,1.6) * scale;

//Distance to a line segment,
float dfLine(vec2 start, vec2 end, vec2 uv)
{
   start *= scale;
   end *= scale;
   vec2 line = end - start;
   float frac = dot(uv - start,line) / dot(line,line);
   return distance(start + line * clamp(frac, 0.0, 1.0), uv);
}

//Distance to the edge of a circle.
float dfCircle(vec2 origin, float radius, vec2 uv)
{
   origin *= scale;
   radius *= scale;
   return abs(length(uv - origin) - radius);
}

//Distance to an arc.
float dfArc(vec2 origin, float start, float sweep, float radius, vec2 uv)
{
   origin *= scale;
   radius *= scale;
   uv -= origin;
   uv *= mat2(cos(start), sin(start),-sin(start), cos(start));
   
   float offs = (sweep / 2.0 - pi);
   float ang = mod(atan(uv.y, uv.x) - offs, tau) + offs;
   ang = clamp(ang, min(0.0, sweep), max(0.0, sweep));
   
   return distance(radius * vec2(cos(ang), sin(ang)), uv);
}

//Distance to the digit "d" (0-9).
float dfDigit(vec2 origin, float d, vec2 uv)
{
   uv -= origin;
   d = floor(d);
   float dist = 1e6;
   
   if(d == 0.0)
   {
      dist = min(dist, dfLine(vec2(1.000,1.000), vec2(1.000,0.500), uv));
      dist = min(dist, dfLine(vec2(0.000,1.000), vec2(0.000,0.500), uv));
      dist = min(dist, dfArc(vec2(0.500,1.000),0.000, 3.142, 0.500, uv));
      dist = min(dist, dfArc(vec2(0.500,0.500),3.142, 3.142, 0.500, uv));
      return dist;
   }
   if(d == 1.0)
   {
      dist = min(dist, dfLine(vec2(0.500,1.500), vec2(0.500,0.000), uv));
      return dist;
   }
   if(d == 2.0)
   {
      dist = min(dist, dfLine(vec2(1.000,0.000), vec2(0.000,0.000), uv));
      dist = min(dist, dfLine(vec2(0.388,0.561), vec2(0.806,0.719), uv));
      dist = min(dist, dfArc(vec2(0.500,1.000),0.000, 3.142, 0.500, uv));
      dist = min(dist, dfArc(vec2(0.700,1.000),5.074, 1.209, 0.300, uv));
      dist = min(dist, dfArc(vec2(0.600,0.000),1.932, 1.209, 0.600, uv));
      return dist;
   }
   if(d == 3.0)
   {
      dist = min(dist, dfLine(vec2(0.000,1.500), vec2(1.000,1.500), uv));
      dist = min(dist, dfLine(vec2(1.000,1.500), vec2(0.500,1.000), uv));
      dist = min(dist, dfArc(vec2(0.500,0.500),3.142, 4.712, 0.500, uv));
      return dist;
   }
   if(d == 4.0)
   {
      dist = min(dist, dfLine(vec2(0.700,1.500), vec2(0.000,0.500), uv));
      dist = min(dist, dfLine(vec2(0.000,0.500), vec2(1.000,0.500), uv));
      dist = min(dist, dfLine(vec2(0.700,1.200), vec2(0.700,0.000), uv));
      return dist;
   }
   if(d == 5.0)
   {
      dist = min(dist, dfLine(vec2(1.000,1.500), vec2(0.300,1.500), uv));
      dist = min(dist, dfLine(vec2(0.300,1.500), vec2(0.200,0.900), uv));
      dist = min(dist, dfArc(vec2(0.500,0.500),3.142, 5.356, 0.500, uv));
      return dist;
   }
   if(d == 6.0)
   {
      dist = min(dist, dfLine(vec2(0.067,0.750), vec2(0.500,1.500), uv));
      dist = min(dist, dfCircle(vec2(0.500,0.500), 0.500, uv));
      return dist;
   }
   if(d == 7.0)
   {
      dist = min(dist, dfLine(vec2(0.000,1.500), vec2(1.000,1.500), uv));
      dist = min(dist, dfLine(vec2(1.000,1.500), vec2(0.500,0.000), uv));
      return dist;
   }
   if(d == 8.0)
   {
      dist = min(dist, dfCircle(vec2(0.500,0.400), 0.400, uv));
      dist = min(dist, dfCircle(vec2(0.500,1.150), 0.350, uv));
      return dist;
   }
   if(d == 9.0)
   {
      dist = min(dist, dfLine(vec2(0.933,0.750), vec2(0.500,0.000), uv));
      dist = min(dist, dfCircle(vec2(0.500,1.000), 0.500, uv));
      return dist;
   }

   return dist;
}

//Distance to a number
float dfNumber(vec2 origin, float num, vec2 uv)
{
   uv -= origin;
   float dist = 1e6;
   float offs = 0.0;
   
   for(float i = 5.0;i > -3.0;i--)
   {  
      float d = mod(num / pow(10.0,i),10.0);
      
      vec2 pos = digitSpacing * vec2(offs,0.0);

      if(i == 0.0)
      {
         dist = min(dist, dfCircle(vec2(offs+0.9,0.1)*1.1, 0.04,uv));
      }
      
      if(num > pow(10.0,i) || i == 0.0)
      {
         dist = min(dist, dfDigit(pos, d, uv));
         offs++;
      }
      

      
   }
   return dist;   
}

//Length of a number in digits
float numberLength(float n)
{
   return floor(max(log(n) / log(10.0), 0.0) + 1.0) + 2.0;
}

void main( void ) 
{
   vec2 aspect = resolution.xy / resolution.y;
   vec2 uv = gl_FragCoord.xy / resolution.y - aspect/2.0;
   vec2 m = mouse * aspect - aspect/2.0;
   
   float n = abs(sin(time / 6.6) * 6.67);
   
   float nsize = numberLength(n);
   
   vec2 pos = -digitSpacing * vec2(nsize,1.0)/2.0;

   float dist = 1e6;
   dist = min(dist, dfNumber(pos, n, uv));
   
   vec3 color = vec3(0);
   
   float shade = 0.0;
   
   //Make the thumbnail look a bit nicer.
   if(IS_THUMBNAIL)
   {
      shade = 0.008 / (dist);
   }
   else
   {
      shade = 0.003 / (dist);
   }
   
   color += vec3(mix(0.01, 0.1, n * 10.0),1.0,-mix(0.01, 0.1, n)) * shade;
   
   
   float grid = 0.5-max(abs(mod(uv.x*64.0,1.0)-0.5), abs(mod(uv.y*64.0,1.0)-0.5));
    
      color *= 0.25+vec3(smoothstep(0.0,64.0 / resolution.y,grid))*0.75;
   
   gl_FragColor = vec4( color , 1.0 );
}