// http://glslsandbox.com/e#30653.1
#ifdef GL_ES
precision mediump float;
#endif

#extension GL_OES_standard_derivatives : enable

//ulam spiral
//could be better, but it seems to work - sphinx

uniform float time;
uniform vec2 mouse;
uniform vec2 resolution;

float extract_bit(float n, float b);
float sprite(float n, vec2 p);
float digit(float n, vec2 p);
float print_index(float index, vec2 position);
float ulam_spiral(vec2 field);
float little_sieve(float n);

void main( void ) 
{
   //pixel coordinate
   vec2 coordinate            = gl_FragCoord.xy-resolution*.5;
   
   
   //ulam spiral
   float scale          = 16.;      
   vec2 field           = floor(coordinate/scale);    
   float ulam           = ulam_spiral(field);

   
   //fun
//    vec2 mouse_coordinate   = floor((mouse*resolution-resolution*.5)/scale);
//    float mouse_ulam        = ulam_spiral(field - mouse_coordinate);
//    ulam              = floor(abs(ulam+mouse_ulam));
   
   
   
   //visualization stuff - colors for the quadrants, printed numbers, grid, prime zieve
   float grid           = float(mod(coordinate.x, scale) < 1.) + float(mod(coordinate.y+.5, scale) < 1.);
   
   bvec4 quadrant             =  bvec4(false);
   quadrant.x           =  field.x >= 0. && abs(field.x) >= abs(field.y);
   quadrant.y           =  field.y >= 0. && abs(field.x) <  abs(field.y);
   quadrant.z           =  field.x  < 0. && abs(field.x) >= abs(field.y);
   quadrant.w           =  field.y  < 0. && abs(field.x) <  abs(field.y);
   
   vec2 print_coordinate      = coordinate;
   print_coordinate.x   = mod(print_coordinate.x, scale);
   print_coordinate.y   = mod(print_coordinate.y, scale);
   print_coordinate     -= scale * .5;
   
   vec3 print           = vec3(0.);
   print.xyz            += print_index(ulam, print_coordinate - vec2(4., -2.));     
   
   float seive          = .5+little_sieve(ulam);
   
   
   //combined results for display
   vec4 result          = vec4(0.);
   
   result.x             += float(quadrant.x);
   result.y             += float(quadrant.y);
   result.z             += float(quadrant.z);
   result.xy            += float(quadrant.w);

   result               += grid/4.;
   result               += abs(floor(mod(time*128., 2048.))-ulam) < 1. ? 1. : 0.;
   result.xyz           += print;

   result               *= seive;
   result               *= .5;
   result.w             = .5 + seive;
   
   gl_FragColor         = result;
}//sphinx

float ulam_spiral(vec2 p)
{
   float x        = abs(p.x);
   float y        = abs(p.y);
   bool q         = x > y;
   
   x        = q ? x : y;
   y        = q ? p.x + p.y : p.x - p.y;
   y        = abs(y) + 4. * x * x + 1.;
   x        *= 2.;
   
   return q ? (p.x > 0. ? y - x - x : y) 
       : (p.y > 0. ? y - x : y + x);      
}


float little_sieve(float n)
{     
   bool prime = n != 0.;
   const float factors = 32.;
   for(float i = 2.; i < factors; i++)
   {
      if(prime)
      {
         prime = (sqrt(2.+n) <= i || mod(n, i) > 0.);;
      }
   }
   return float(prime);
}

float extract_bit(float n, float b)
{
   n = floor(n);
   b = floor(b);
   b = floor(n/pow(2.,b));
   return float(mod(b,2.) == 1.);
}

float sprite(float n, vec2 p)
{
   p = floor(p);
   float bounds = float(all(lessThan(p, vec2(3., 5.))) && all(greaterThanEqual(p,vec2(0,0))));
   return extract_bit(n, (2. - p.x) + 3. * p.y) * bounds;
}

float digit(float n, vec2 p)
{
   n = mod(floor(n), 10.0);
   if(n == 0.) return sprite(31599., p);
   else if(n == 1.) return sprite( 9362., p);
   else if(n == 2.) return sprite(29671., p);
   else if(n == 3.) return sprite(29391., p);
   else if(n == 4.) return sprite(23497., p);
   else if(n == 5.) return sprite(31183., p);
   else if(n == 6.) return sprite(31215., p);
   else if(n == 7.) return sprite(29257., p);
   else if(n == 8.) return sprite(31727., p);
   else if(n == 9.) return sprite(31695., p);
   else return 0.0;
}

float print_index(float index, vec2 position)
{     
   float result   = 0.;
   result         += index < 0. ? sprite(24., position+vec2(4., 0.)) : 0.;          
   for(int i = 0; i < 8; i++)
   {
      float place = pow(10., float(i));
      if(index >= place || float(i) < 1.)
      {
         result         += digit(abs(index/place), position);
         position.x     += 4.;
      }
   }
   return result;
}