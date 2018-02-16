// http://glslsandbox.com/e#27642.0
#ifdef GL_ES
precision mediump float;
#endif

uniform float time;
uniform vec2 mouse;
uniform vec2 resolution;

// Scale factor for blob fields
const float scale = 1.534212;

const mat3 dottransform = mat3(
0.11651722197701599,0.3544947103881083,0.9277700347012776,
-0.10673931989767256,-0.9242528744643419,0.3665560552387368,
0.9874363040574016,-0.14173963578072568,-0.06985285304270773
)*scale;

// Maximum tracing distance
const float maxdist = 100.;

// Maximum number of raymarch steps
const int maxiter = 200;



float meteor(vec3 pos) {
   vec3 position = pos;
   
   // I create 3 sums here, 2 for craters, one for blobs.
   // For 2 of them I don't simply sum but sum squares and later take the square root. This is to suppress undesired accumulation of details.
   // You can observe the outcome at the craters: large craters suppress smaller ones. Similar things happen with blobs.
   // If you are curious you can remove the *abs(dots) part as well as the sqrt(craters) resp. sqrt(blobs) at the outcome.
   // Example:
   // * sqrt(0^2 (no big crater) + 0^2) (no small crater) = 0
   // * sqrt(4^2 (   big crater) + 0^2) (no small crater) = 4
   // * sqrt(0^2 (no big crater) + 1^2) (   small crater) = 1
   // * sqrt(4^2 (   big crater) + 1^2) (   small crater) = 4.12... (suppressed small crater)
   // Just 1 of the "crater" sums actually suppresses small craters. This is to reflect what happens on
   // real asteroids: big impacts destroy previous craters but new impacts still create additional craters.

   float craters = 0.0;
   float craters2 = 0.0;
   float blobs = 0.0;
   float f = .1;  // initial scale
   position *= f;
   for (int i = 0; i < 15; i++) {
      // This creates regularly spaced blobs, one per unit cell
      vec3 v = fract(position)-.5;        // distance vector from unit cell center
      float dots = max(0.,.15-dot(v,v));  // simple blob around the unit cell center
      dots = dots*dots*dots/f;            // 3rd power to smooth out the border of the unit cell

      // Opposite sign for every other unit cell
      vec3 signv = sign(fract(position*.5)-.5);
      float signf = signv.x*signv.y*signv.z;

      // Add blobs to either of the sums, depending on sign. Note that I add the square of the blob.
      craters += max(0.,dots*signf)*abs(dots);
      craters2 += min(0.,dots*signf);
      blobs += min(0.,dots*signf)*abs(dots);
      
      // Rotate and scale the view (and thus the next series of blobs)
      f *= scale;
      position *= dottransform;
   }
   // Isosurface function. < 0 = inside blob, > 0 = in outer space. Ideally any value > 0 should be the distance to the nearest spot where the value is <= 0.
   return (craters2*.5+sqrt(craters)-sqrt(-blobs))*55.1+1.;
}

void main( void ) {
   // Compute camera position and ray based on time and mouse
   float x = (mouse.x-.5)*6.;
   float y = (mouse.y-.5)*3.;
   mat3 viewmat = mat3(1,0,0,0,cos(y),sin(y),0,-sin(y),cos(y)) * mat3(cos(x),0, sin(x),0,1,0,-sin(x),0,cos(x));
   
   vec3 pos = vec3(0,0,time);
   vec3 dir = normalize(vec3((gl_FragCoord.xy-resolution.xy*0.5)/resolution.x,.5))*viewmat;
   
   // raymarch
   float dist = 0.;
   for (int i = 0; i < maxiter; i++) {
      float h = meteor(pos);
      pos += dir*h*.8;
      dist += h;
      if (h < .0001 || dist > maxdist) break;
   }
   
   // compute surface normal
   float h = meteor(pos);
   float d = .01;
   vec3 hd = (vec3(meteor(pos+vec3(d,0,0)),meteor(pos+vec3(0,d,0)),meteor(pos+vec3(0,0,d)))-h)/d;
   
   // Dot product to compute luminance (scaled to 0..1).
   float c = (dot(vec3(1,0,0),normalize(vec3(hd)))+1.)*.5;
   
   float distAttenuate = max(0.,1.-dist/maxdist);
   float attenuate = distAttenuate*distAttenuate*distAttenuate*max(0.,1.-h);
   // Final color is computed by using pow on the dot product - the higher the power, the smaller the spot is. 1 looks stone-like, 10 to 100 look like plastic.
   vec3 color = vec3(pow(c,1.),pow(c,2.),pow(c,4.));
   
   gl_FragColor = vec4(attenuate*color,1.);
}