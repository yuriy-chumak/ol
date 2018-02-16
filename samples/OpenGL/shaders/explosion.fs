// http://glslsandbox.com/e#29149.10
//---------------------------------------------------------
// Explosion.glsl
// original:   https://www.shadertoy.com/view/Xd3GWn
//---------------------------------------------------------

#ifdef GL_ES
precision mediump float;
#endif

uniform float time;
uniform vec2 mouse;
uniform vec2 resolution;

/* Explosion 
Created by Roman Komary 2014-2015
License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US

Based on huwb's Anatomy of an explosion https://www.shadertoy.com/view/Xss3DS
and on iq's Hell https://www.shadertoy.com/view/MdfGRX

The background fun parts base on clouds of nimitz's Xyptonjtroz https://www.shadertoy.com/view/4ts3z2
and on bjarkeck's Simple star field https://www.shadertoy.com/view/lsc3z4


Nice to play with: MAX_MULT_EXPLOSIONS, SHOW_BOUNDS, LOW_Q, WITH_FUN, ALTERNATE_MOUSE

If you are on a mobile device, switch on LOW_Q to 1, set steps to about 25,
MAX_MULT_EXPLOSIONS to 3. This should give acceptable results. 


Many thanks to huwb and his Anatomy of an explosion shader, I got a great start for an explosion effect
that I was desiring so much.

Extending it over time though did not result in a believable explosion effect. It looks like
a still cloud with more parts appearing instead of a moving, growing explosion.

As a start, I just made a growing sphere that eats up the density from inside.
(The introductory "den" calculation in densityFn with ballness, growth and fade).
It looks nice for the fading-away of an explosion, but it is not enough yet.

Many thanks to iq and his mathematical frameworks which brought so many shaders to live and
revealed the beauty of mathematics.

It's especially his beautiful Hell shader (one of my favourites) and his idea of space inversion
which gave me the final idea for the explosion effect.
The space inversion actually converts a directional moving cloud space into a growing cloud effect.

It is the nature of this effect that it grows on one end but shrinks in on the opposite.
To compensate on that, I could only realize composing multiple explosion balls like such to hide the
shrinking-in hemispheres as much as possible.

Unfortunately, I only found a way to superpose these balls by having them independently
merged in color. That is, their animations do not interfere with each other, which is absolutely not
a realistic effect. It is not even a physical approximation.
Not that there would be anything in this shader near physical correctness, but it can be seen
that the balls are independent.

I tried to fiddle with space inversion and cloud movements in multiple directions at once (instead of just one)
on just one explosion ball. It would melt the explosion to one cloud peace instead of being multiple independent
balls, but it always built up undesirable seams and stretched parts of the cloud which makes
the explosion not be an explosion at all anymore.
(Although it looks quite fancy on its own which goes in the direction of a bent sun bursts similar effect).

Another disadvantage with the multiple balls is the need to sample noise space for each ball
separately, which eats up speed power rather quickly.
But I could not find a way around that.

The fog I did add just for fun. I like this algorithm so much. It just needs a hand full of iterations
and yet the effect looks convincing even with a moving and rotating camera without any jumps or glitches.
Thanks to nimitz and his Xyptonjtroz shader (https://www.shadertoy.com/view/4ts3z2).

Out of lazyness and for the reason that the main purpose of this shader is just the explosion,
I draw the explosion over the fog, always, no matter if fog layers actually should be before the explosion.

I would have liked the explosion effect to suffice on just such a small number of iterations.

But well, the explosion looks relatively believable and I still like to look at it,
so I am not unsatisfied.

I also added the stars just for fun because I like bjarkeck's simple algorithm with the so believable result.

And when playing around with night scenery setting, I discovered this great look of corridor light
by accident, so I had to keep it. :-) Just have a look on the nice shine the corridor light spreads a bit
around the top of the big columns and near the stairs. Pure luck.
*/



// In calcDens(), description mentions a bug which appeared with the old coloring.
//#define OLD_COLORING

// if not defined, mouse y will move camera
// if defined, mouse y will override animation time stamp
//#define ALTERNATE_MOUSE

// for (slight) speed improvement, use low quality fbm and noise and compensate with some settings adjustments
// if not defined, high quality
// if 1, medium quality. acceptable.
// if 2, low quality. not acceptable anymore.
// Notice, 1 and 2 have approximately the same speed when putting also the compensation adjustments. But compared to high quality, they are indeed faster.
//#define LOW_Q 1

// some approximation to show the inner and outer bounds of the volumes. the y center plane is removed (transparent)
// to give a better look and feel on the inside.
//#define SHOW_BOUNDS

// Disable this if you want purely procedural noise instead of using a lookup texture.
// The procedural is a bit slower but useful if you do not have easy access to textures.
#define NOISE_LUT

#define CAM_ROTATION_SPEED 11.7
#define CAM_TILT .15                      // put 0. if you do not want to animate camera vertically
#define CAM_DIST 3.8

#define MAX_MULT_EXPLOSIONS 5

// the bounding sphere of the explosion. this is less general but means that
// ray cast is only performed for nearby pixels, and raycast can begin from the sphere
// (instead of walking out from the camera)
float expRadius = 1.75;
float explosion_seed = 0.0;               // keep this constant for a whole explosion, but when differing from explosion to the next one, you get non-identical looking ones
float downscale = 1.25;                   // how much smaller (than expRadius) one explosion ball should be. bigger value = smaller. 1.0 = no scale down.
const int steps = 64;                     // iterations when marching through cloud noise. default = 64. 40 might still suffice. When putting higher, explosion becomes too dense, so make colBottom and colTop more transparent.
float grain = 2.0;                              // increase for more detailed explosions, but then you should also increase iterations (and decrease step, which is done automatically)
float speed = 0.3;                              // total animation speed (time stretch). nice = 0.5, default = 0.4
float ballness = 2.0;                     // lower values makes explosion look more like a cloud. higher values more like a ball.
float growth = 2.2;                             // initial growth to explosion ball. lower values makes explosion grow faster
float fade = 1.6;                         // greater values make fade go faster but later. Thus greater values leave more smoke at the end.
float thinout_smooth = 0.7;               // smoothed thinning out of the outer bounding sphere. 1.0 = no smoothening, 0.0 = heavy thinning, nice = 0.65 to 0.75
float density = 1.35;                     // higher values make sharper difference between dark and bright colors. low values make more blurry, less color spread and more transparent. default = 1.25 or 1.35
vec2 brightness = vec2(3.0, 2.2);   // x = constant offset, y = time-dependent factor
vec2 brightrad = vec2(1.3, 1.0);    // adds some variation to the radius of the brightness falloff. x = constant offset, y = density-dependent factor
vec4 colBottom = vec4(1.2,0.94,0.42,0.7);
vec4 colTop = vec4(0.15,0.15,0.15,0.1);
float color_low = 0.25;                   // the lower the value, the more black spots appear in the explosion. the higher, the more even the explosion looks like.
float contrast = 1.0;                     // final color contrast. higher values make ligher contrast. default = 1.0
float rolling_init_damp = 0.3;            // rolling animation initial damping. 0.0 = no damping. nice = 0.2, default = 0.15
float rolling_speed = 2.0;                // rolling animation speed (static over time). default = 1.0
const int mult_explosions = MAX_MULT_EXPLOSIONS;      // how many explosion balls to draw
float variation_seed = 0.0;               // influences position variation of the different explosion balls
float delay_seed = 0.0;                   // influences the start delay variation of the different explosion balls
const float delay_range = 0.25;                 // describes the maximum delay for explosion balls to start up. Notice, this delay is relative to one explosion ball duration, so actually before speed is applied.
float ball_spread = 1.0;                  // how much to spread ball starting positions from the up vector. 0.0 = all on up vector, 1.0 = any direction between up and down vector.

/* for up-moving explosion similar to explosion mushroom, put
   downscale = 1.75;
   grain = 2.7;
   rolling_init_damp = 0.2;
   ball_spread = 0.4;
*/

/* for mobile device, for faster rendering but with less quality, put
   LOW_Q 1
   turn off FOG
   MAX_MULT_EXPLOSIONS 3
   steps = 25;
*/

// Now come some fun effects which have nothing to do with the explosion effect.
// You can switch them all off completely by commenting WITH_FUN.
#define WITH_FUN
   // The fog is just for fun and has nothing to do with the explosion.
   #define FOG
   // Same with the stars. Just for fun.
   #define STARS
      #define STARDISTANCE 250.
      #define STARBRIGHTNESS 0.3
      #define STARDENCITY 0.05
   // Night scenery settings, again just for fun.
   #define DAY_NIGHT_CYCLE_TIME 20.
   #define NIGHT_COLORING vec3(.92,.95,1.)
   #define CORRIDOR_LIGHT vec3(1.,1.,.9)
   #define ENLIGHTEN_PASSAGE .75
   // explosion enlightening the floor (faked)
   #define FLOOR_LIGHT_STRENGTH 1.



struct Ball
{
   vec3 offset;
    vec3 dir;
    float delay;
};

Ball balls[MAX_MULT_EXPLOSIONS];

float tmax = 1.0 + delay_range;
float getTime()
{
   return fract(time * speed / tmax) * tmax;
}

const float pi=3.14159265;

float hash( float n )
{
   return fract(cos(n)*41415.92653);      //https://www.shadertoy.com/view/4sXGRM
    //return fract(sin(n)*753.5453123);   //https://www.shadertoy.com/view/4sfGzS
}

vec2 hash2( float n )
{
    //return fract(cos(n)*vec2(10003.579, 37049.7));  //https://www.shadertoy.com/view/XtsSWs
    return fract(sin(vec2(n,n+1.0))*vec2(13.5453123,31.1459123));
}

vec3 hash3( float n )
{
    return fract(sin(vec3(n,n+1.0,n+2.0))*vec3(13.5453123,31.1459123,37.3490423));
}

float hash13(vec3 p3)
{
   p3  = fract(p3 * vec3(.1031,.11369,.13787));
    p3 += dot(p3, p3.yzx + 19.19);
    return fract((p3.x + p3.y) * p3.z);
}

float noise( in vec3 x )
{
    vec3 f = fract(x);
    vec3 p = x - f; // this avoids the floor() but doesnt affect performance for me.
    f = f*f*(3.0-2.0*f);
   
    float n = p.x + p.y*157.0 + 113.0*p.z;
    return mix(mix(mix( hash(n+  0.0), hash(n+  1.0),f.x),
                   mix( hash(n+157.0), hash(n+158.0),f.x),f.y),
               mix(mix( hash(n+113.0), hash(n+114.0),f.x),
                   mix( hash(n+270.0), hash(n+271.0),f.x),f.y),f.z);
}

float fbm( vec3 p, vec3 dir )
{
    float f;
#ifndef LOW_Q
    vec3 q = p - dir; f  = 0.50000*noise( q );
   q = q*2.02 - dir; f += 0.25000*noise( q );
   q = q*2.03 - dir; f += 0.12500*noise( q );
   q = q*2.01 - dir; f += 0.06250*noise( q );
   q = q*2.02 - dir; f += 0.03125*noise( q );
#elif LOW_Q == 1
    // in low quality setting, for speed, we try to live with a lower-quality fbm. we compensate with higher grain.
    vec3 q = p - dir; f  = 0.50000*noise( q );
   q = q*2.02 - dir; f += 0.25000*noise( q );
   q = q*2.03 - dir; f += 0.12500*noise( q );
   q = q*2.04 - dir; f += 0.08250*noise( q );
#elif LOW_Q == 2
    vec3 q = p - dir; f  = 0.50000*noise( q );
   q = q*2.14 - dir; f += 0.29000*noise( q );
   q = q*2.25 - dir; f += 0.16500*noise( q );
#endif
   return f;
}


float tri(in float x){return abs(fract(x)-.5);}
vec3 tri3(in vec3 p){return vec3( tri(p.z+tri(p.y*1.)), tri(p.z+tri(p.x*1.)), tri(p.y+tri(p.x*1.)));}

float triNoise3d(in vec3 p, in float spd, float ti)
{
    float z=1.1;
   float rz = 0.;
    vec3 bp = p*1.5;
   for (float i=0.; i<=3.; i++ )
   {
        vec3 dg = tri3(bp);
        p += (dg+spd);
        bp *= 1.9;
      z *= 1.5;
      p *= 1.3;
        
        rz+= (tri(p.z+tri(p.x+tri(p.y))))/z;
        bp += 0.14;
   }
   return rz;
}

float fogmap(in vec3 p, in float d, float ti)
{
    p.x *= .4;
    p.x += ti*1.5;
    p.z += sin(p.x*.5);
    p.z *= .4;
    return max(triNoise3d(p*.3/(d+20.),0.2, ti)-.4, 0.)*(smoothstep(0.,25.,p.y));
    //return triNoise3d(p*1.2/(d+20.),0.2, ti)*(1.25-smoothstep(0.,25.,p.y));
}
// Thanks to nimitz for the fast fog/clouds idea...
// https://www.shadertoy.com/view/4ts3z2
vec3 clouds(in vec3 col, in vec3 ro, in vec3 rd, in float mt, float ti, in vec3 cloudcolor)
{
    float d = 1.5;      //.5
    for(int i=0; i<7; i++)
    {
        if (d>mt)break;
        vec3  pos = ro + rd*d;
        float rz = fogmap(pos, d, ti);
      //float grd =  clamp((rz - fogmap(pos+.8-float(i)*0.1,d, ti))*3., 0.1, 1. );
        //vec3 cloudcolor = (vec3(.1,0.8,.5)*.5 + .5*vec3(.5, .8, 1.)*(1.7-grd))*0.55;
        //vec3 cloudcolor = (2.*vec3(.4,0.4,.4) + .5*vec3(.5)*(1.7-grd))*0.55;
        //vec3 cloudcolor = 2.*(vec3(.4,0.4,.4));
        col = mix(col,cloudcolor,clamp(rz*smoothstep(d-0.4,2.+d*1.75,mt),0.,1.) );
        //col = mix(col,cloudcolor,clamp(rz*smoothstep(d,d*1.86,mt),0.,1.) );
        d *= 1.5+0.3;
    }
    return col;

}


// Thanks to bjarkeck for the fast star field implementation...
// https://www.shadertoy.com/view/lsc3z4
float stars(vec3 ray)
{
    vec3 p = ray * STARDISTANCE;
    float brigtness = smoothstep(1.0 - STARDENCITY, 1.0, hash13(floor(p)));
    return smoothstep(STARBRIGHTNESS, 0., length(fract(p) - 0.5)) * brigtness;
}


// assign colour to the media
vec4 computeColour( float density, float radius, float bright )
{
   // colour based on density alone. gives impression of occlusion within
   // the media
   //vec4 result = vec4( mix( vec3(1.0,0.9,0.8), vec3(.7,0.3,0.2), density ), density );
   //vec4 result = vec4( mix( vec3(1.0,0.9,0.8), vec3(0.4,0.15,0.1), density ), density );
   vec4 result = vec4( vec3(mix( 1.0, color_low, density )), density );
    //vec4 result = vec4( mix( 1.1*vec3(1.0,0.9,0.8), 0.9*vec3(0.4,0.15,0.1), density ), density );
    //vec4 result = vec4(1.,1.,1.,density);

   
   // colour added for explosion
    //result *= mix( colBottom * bright, colTop * bright, min( (radius+0.5)*0.588, 1.0 ) );
   result *= mix( colBottom, colTop, min( (radius+0.5)*0.588, 1.0 ) ) * bright;
    //result *= mix( colBottom, colTop, radius ) * bright;
   //result.rgb *= mix( colBottom * bright, colTop, smoothstep( 0., 1., (radius-0.5)*0.6+0.5 ) );
   //result *= mix( colBottom * bright, colTop, clamp( radius * 1.7-.2, 0.0, 1.0 ) );
    //result.a*=density*1.5;
   //result.a *= mix( 1.0, 0.0, min( (radius / expRadius + 0.2)*0.5, 1.0 ) );
    //result.a *= mix( 1.0, 0.2, min( (radius+0.5)/1.7, 1.0 ) );
   //result.a *= mix( 0.0, 1.0, 1.0-radius*0.25 );
   //if(radius<1.0-mouseY) result.a=0.0;
   // make central hole
   //result.a *= clamp((radius/expRadius-0.5*mouseIn)*15.0, 0.0, 1.0);
   //result.xyz *= mix( 3.1*vec3(1.0,0.5,0.05), vec3(0.48,0.53,0.5), min( radius*.76, 1.0 ) );
   
    //result = mix( colBottom * bright * vec4(1.0,0.9,0.8,1.0), colTop*vec4(0.4,0.15,0.1,1.0), min( (radius+0.5)/1.7, 1.0 ) );
    //result.a *= density;
    
   return result;
}

// maps 3d position to density
float densityFn( in vec3 p, in float r, float t, in vec3 dir, float seed )
{
    //const float pi = 3.1415926;
   float den = ballness + (growth+ballness)*log(t)*r;
    den -= (2.5+ballness)*pow(t,fade)/r;
    //den = -1.7 - p.y;
   //den *= 1.+smoothstep(0.75,1.,r);
    
    //if ( den <= -4. || den > -1. ) return -1.;
    //if ( den <= -2.8 ) return -1.;
    if ( den <= -3. ) return -1.;
    //if ( den > -1. ) return -1.;
    
#ifdef SHOW_BOUNDS
    p = 0.5 * normalize(p);
    return abs(p.y);
    //return 0.8;
#endif
    
   // offset noise based on seed
   // plus a time based offset for the rolling effect (together with the space inversion below)
    //float s = seed-(rolling_speed/(t+rolling_init_damp));
    float s = seed-(rolling_speed/(sin(min(t*3.,1.57))+rolling_init_damp));
   //if( iMouse.z > 0.0 ) t += iMouse.y * 0.02;
    //vec3 dir = vec3(0.,1.,0.);
   //vec3 dir = -0.5*(p - expCenter);
    //vec3 dir = normalize( vec3( noise(p.xyz), noise(p.yxz), noise(p.zyx) ) );
    dir *= s;

    // invert space
    p = -grain*p/(dot(p,p)*downscale);

    // participating media
    float f = fbm( p, dir );
    //f=clamp(f,.1,.7);
   
   // add in noise with scale factor
   den += 4.0*f;
    //den -= r*r;
   
   //den *= density;    // we do that outside
   //den *= 1.25;
    //den *= .8;

   return den;
}

// rad = radius of complete mult explosion (range 0 to 1)
// r = radius of the explosion ball that contributes the highest density
// rawDens = non-clamped density at the current maching location on the current ray
// foffset = factor for offset how much the offsetting should be applied. best to pass a time-based value.
void calcDens( in vec3 pos, out float rad, out float r, out float rawDens, in float t, in float foffset, out vec4 col, in float bright )
{
    float radiusFromExpCenter = length(pos);
    rad = radiusFromExpCenter / expRadius;

    r = 0.0;
    rawDens = 0.0;
    col = vec4(0.0);

    for ( int k = 0; k < mult_explosions; ++k )
    {
        float t0 = t - balls[k].delay;
        if ( t0 < 0.0 || t0 > 1.0 ) continue;

        vec3 p = pos - balls[k].offset * foffset;
        float radiusFromExpCenter0 = length(p);

        float r0 = downscale* radiusFromExpCenter0 / expRadius;
        if( r0 > 1.0 ) continue;
      // BUG: Skipping for r0 > 1.0 gives some artefacts on later smoke where the inside of sphere
        // is more transparent than the outside (for the parts where other expl balls contribute density in).
        // I can't figure yet what the problem is. Inside the sphere near border, densities should be
        // practically 0.0 which also does not contribute (almost) anything to sum in contributeDens.
        // So what's the problem then?
        // Notice, the same bug happens with skipping for t0 > 1.0, just there slight jumps can be seen near
        // end of animation for certain angle views.
        // Reason for the bug: Below, we pass r0 as r. If a density is not skipped but becomes in final color
        // actually transparent, r0 is still passed as r. Outside the r0, the r gains a value from another
        // explosion ball and thus gains also its rawDens0. Inside our r0, the other's ball's density gets
        // skipped, which is producing the jump.
      // Fix would be to intermengle all densities altogether without
        // skipping any. But how? Especially how to intermengle all the r0's?
      // Actually the problem comes from color calculation which makes the final color near transparent the
      // higher the density value.
      // So maybe the fix would be to put the transparency information into the density somehow before
      // selecting one radius. Actually we could add up all the densities, but the one which was the
      // highest could be that one who's r0 we will use as r. Maybe.
        // FIX: The bug is only with OLD_COLORING. New coloring should not have this bug anymore.
        
        float rawDens0 = densityFn( p, r0, t0, balls[k].dir, explosion_seed + 33.7*float(k) ) * density;

#ifndef SHOW_BOUNDS
      // thin out the volume at the far extends of the bounding sphere to avoid
      // clipping with the bounding sphere
      rawDens0 *= 1.-smoothstep(thinout_smooth,1.,r0);
#endif

#ifndef OLD_COLORING
      float dens = clamp( rawDens0, 0.0, 1.0 );

        //vec4 col0 = computeColour(dens, r0*(.9+.5*dens)/1.75, bright);      // also adds some variation to the radius
        //vec4 col0 = computeColour(dens, r0*(1.4+rawDens0), bright);         // also adds some variation to the radius
      vec4 col0 = computeColour(dens, r0*(brightrad.x+brightrad.y*rawDens0), bright);     // also adds some variation to the radius

#ifndef SHOW_BOUNDS
        // uniform scale density
        //col0.a *= 0.8;
        //col0.a *= col0.a + .4;
        col0.a *= (col0.a + .4) * (1. - r0*r0);

        // colour by alpha
        col0.rgb *= col0.a;
#else
      col0.a *= 5.;
#endif

        col += col0;

      rawDens = max(rawDens, rawDens0);
        //rawDens+=max(rawDens0,0.);

        /*if ( rawDens0 > rawDens )
        {
            rawDens = rawDens0;
            r = r0;
            col = col0;
        }*/
#else
        if ( rawDens0 > rawDens )
        {
            rawDens = rawDens0;
            r = r0;
        }
#endif
    }

#ifdef SHOW_BOUNDS
    col /= float(mult_explosions);
#endif
    
   //rawDens *= density;
}

#ifdef OLD_COLORING
// rad = radius of complete mult explosion (range 0 to 1)
// r = radius of the explosion ball that contributes the highest density
// rawDens = non-clamped density at the current maching location on the current ray
void contributeDens( in float rad, in float r, in float rawDens, in float bright, out vec4 col, inout vec4 sum )
{
    //float dens = clamp( rawDens, 0.0, 1.0 );
    float dens = min( rawDens, 1.0 );     // we expect already rawDens to be positive

    //col = computeColour(dens, r*(.9+.5*dens)/1.75, bright);     // also adds some variation to the radius
    //col = computeColour(dens, r*(1.4+rawDens), bright);   // also adds some variation to the radius
    col = computeColour(dens, r*(brightrad.x+brightrad.y*rawDens), bright);   // also adds some variation to the radius

#ifndef SHOW_BOUNDS
    // uniform scale density
    //col.a *= 0.8;
    //col.a *= col.a + .4;
    col.a *= (col.a + .4) * (1. - r*r);

    // colour by alpha
    col.rgb *= col.a;

    // alpha blend in contribution
    sum = sum + col*(1.0 - sum.a);
    sum.a+=0.15*col.a;
#else
    col.a *= 5.;
      sum = max(sum, col);
#endif
}
#endif

#ifndef OLD_COLORING
void contributeColor( in vec4 col, inout vec4 sum )
{
#ifndef SHOW_BOUNDS
    // alpha blend in contribution
    sum = sum + col*(1.0 - sum.a);
    sum.a+=0.15*col.a;
#else
      sum = max(sum, col);
#endif
}
#endif

vec4 raymarch( in vec3 rayo, in vec3 rayd, in vec2 expInter, in float t, out float d )
{
    vec4 sum = vec4( 0.0 );
    
    float step = 1.5 / float(steps);
     
    // start iterating on the ray at the intersection point with the near half of the bounding sphere
   //vec3 pos = rayo + rayd * (expInter.x + step*texture2D( iChannel2, gl_FragCoord.xy/iChannelResolution[0].x ).x);          // dither start pos to break up aliasing
   //vec3 pos = rayo + rayd * (expInter.x + 1.0*step*fract(0.5*(gl_FragCoord.x+gl_FragCoord.y)));     // regular dither
   vec3 pos = rayo + rayd * (expInter.x); // no dither

    float march_pos = expInter.x;
    d = 4000.0;
    
    // t goes from 0 to 1 + mult delay. that is 0 to 1 is for one explosion ball. the delay for time distribution of the multiple explosion balls.
    // t_norm is 0 to 1 for the whole animation (incl mult delay).
    float t_norm = t / tmax;
    float smooth_t = sin(t_norm*2.1);     //sin(t*2.);

   //float bright = 6.1;
   float t1 = 1.0 - t_norm;   // we use t_norm instead of t so that final color is reached at end of whole animation and not already at end of first explosion ball.
    //float bright = 3.1 + 18.0 * t1*t1;
   //float bright = 3.1 + 1.4 * t1*t1;
   //float bright = 3.1 + 4.4 * t1*t1;
   float bright = brightness.x + brightness.y * t1*t1;
   //float bright = smoothstep(0.0, 30.1, 1.0);
   //float bright = smoothstep(20.0, 3.1, 1.0);
    //float bright = 10.;

    for( int i=0; i<steps; i++ )
    {
        if( sum.a >= 0.98 ) { d = march_pos; break; }
        if ( march_pos >= expInter.y ) break;
        
        float rad, r, rawDens;
        vec4 col;
        calcDens( pos, rad, r, rawDens, t, smooth_t, col, bright );

        if ( rawDens <= 0.0 )
        {
            float s = step * 2.0;
            pos += rayd * s;
            march_pos += s;
            continue;
        }
        
#ifdef OLD_COLORING
        contributeDens( rad, r, rawDens, bright, col, sum );
#else
        contributeColor( col, sum );
#endif
      
      // take larger steps through low densities.
      // something like using the density function as a SDF.
      float stepMult = 1.0 + (1.-clamp(rawDens+col.a,0.,1.));
      // step along ray
      pos += rayd * step * stepMult;
        march_pos += step * stepMult;

      //pos += rayd * step;
   }

#ifdef SHOW_BOUNDS
    if ( sum.a < 0.1 )
        sum = vec4(0.,0.,.5,0.1);
#endif
   
    return clamp( sum, 0.0, 1.0 );
}

// iq's sphere intersection, but here fixed for a sphere at (0,0,0)
vec2 iSphere(in vec3 ro, in vec3 rd, in float rad)
{
   //sphere at origin has equation |xyz| = r
   //sp |xyz|^2 = r^2.
   //Since |xyz| = ro + t*rd (where t is the parameter to move along the ray),
   //we have ro^2 + 2*ro*rd*t + t^2 - r2. This is a quadratic equation, so:
   //vec3 oc = ro - sph.xyz; //distance ray origin - sphere center
   
   float b = dot(ro, rd);                             //=dot(oc, rd);
   float c = dot(ro, ro) - rad * rad;           //=dot(oc, oc) - sph.w * sph.w; //sph.w is radius
   float h = b*b - c; // delta
   if(h < 0.0) 
      return vec2(-1.0);
    //h = sqrt(h);
    h *= 0.5;           // just some rough approximation to prevent sqrt.
    return vec2(-b-h, -b+h);
}

vec3 computePixelRay( in vec2 p, out vec3 cameraPos )
{
    // camera orbits around explosion
   
    float camRadius = CAM_DIST;
   // use mouse x coord
    float a = time * CAM_ROTATION_SPEED;
    float b = CAM_TILT * sin(a * .014);
    float phi = b * 3.14;
    float camRadiusProjectedDown = camRadius * cos(phi);
    float theta = -(a-resolution.x)/80.;
    float xoff = camRadiusProjectedDown * cos(theta);
    float zoff = camRadiusProjectedDown * sin(theta);
    float yoff = camRadius * sin(phi);
    cameraPos = vec3(xoff,yoff,zoff);
     
    // camera target
    vec3 target = vec3(0.);
     
    // camera frame
    vec3 fo = normalize(target-cameraPos);
    vec3 ri = normalize(vec3(fo.z, 0., -fo.x ));
    vec3 up = normalize(cross(fo,ri));
     
    // multiplier to emulate a fov control
    float fov = .5;
   
    // ray direction
    vec3 rayDir = normalize(fo + fov*p.x*ri + fov*p.y*up);
   
    return rayDir;
}

void setup()
{
    // first expl ball always centered looking up
    balls[0] = Ball(
        vec3(0.),
        vec3(0.,.7,0.),       // not normalized so that expl ball 0 rolls somewhat slower
        0.0
    );

    float pseed = variation_seed;
    float tseed = delay_seed;
    float maxdelay = 0.0;
    for ( int k = 1; k < mult_explosions; ++k )
    {
        float pseed = variation_seed + 3. * float(k-1);
        float tseed = delay_seed + 3. * float(k-1);
        vec2 phi = hash2(pseed) * vec2(2.*pi, pi*ball_spread);
        vec2 tilted = vec2( sin(phi.y), cos(phi.y) );
        vec3 rotated = vec3( tilted.x * cos(phi.x), tilted.y, tilted.x * sin(phi.x) );
        balls[k].offset = 0.7 * rotated; //hash3(pseed) - 0.5;
        balls[k].dir = normalize( balls[k].offset );
        balls[k].delay = delay_range * hash(tseed);
        pseed += 3.;
        tseed += 3.;
        maxdelay = max(maxdelay, balls[k].delay);
    }
    
    if ( maxdelay > 0.0 )
    {
        // Now stretch the ball explosion delays to the maximum allowed range.
        // So that the last ball starts with a delay of exactly delay_range and thus we do not waste any final time with just empty space.
         for ( int k = 0; k < mult_explosions; ++k )
            balls[k].delay *= delay_range / maxdelay;
    }
}

void main()
{
    float t = getTime();

    // some global initialization.
    setup();

    // get aspect corrected normalized pixel coordinate
    vec2 q = gl_FragCoord.xy / resolution.xy;
    vec2 p = -1.0 + 2.0*q;
    p.x *= resolution.x / resolution.y;
    
    vec3 rayDir, cameraPos;
    rayDir = computePixelRay( p, cameraPos );
   
    vec4 col = vec4(0.);
    float d = 4000.0;
   
    // does pixel ray intersect with exp bounding sphere?
    vec2 boundingSphereInter = iSphere( cameraPos, rayDir, expRadius );
    if( boundingSphereInter.x > 0. )
    {
        // yes, cast ray
        col = raymarch( cameraPos, rayDir, boundingSphereInter, t, d );
    }
    gl_FragColor.xyz = col.xyz*col.xyz*(1.0+contrast*(1.0-col.xyz));
    gl_FragColor.w = 1.0;
}
