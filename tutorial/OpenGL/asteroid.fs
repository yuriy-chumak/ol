// http://glslsandbox.com/e#29707.2

//---------------------------------------------------------
// Shader:   Meteorite.glsl     by foxes         2015-12-26
//           http://glslsandbox.com/e#29707.2 adapted by I.G.P.
//           ray cast rendering of procedural noised meteorite
// Tags:     meteroit, 3d, procedural, noise, shadow, raycast 
// Note:     rotate meteorite with mouse
// see also: https://www.shadertoy.com/view/4s33RX
//---------------------------------------------------------
#ifdef GL_ES
  precision highp float;
#endif

uniform float time;
uniform vec2 mouse;
uniform vec2 resolution;

//---------------------------------------------------------
#define TWO_PI 6.283185307179586476925286766559
#define ROTATIONS 60.0
#define ROTATION_ANGLE (TWO_PI / 60.0)

#define pradius 2.0
#define mradius 0.1
#define iterations 20.0
#define shadowit 10.0
#define line 0.4

//---------------------------------------------------------
vec4 NC0=vec4(0.0,157.0,113.0,270.0);
vec4 NC1=vec4(1.0,158.0,114.0,271.0);

vec4 hash4( vec4 n )
{
    return fract(sin(n)*753.5453123);
}

float noise3( vec3 x )
{
    vec3 p = floor(x);
    vec3 f = fract(x);
    f = f*f*(3.0-2.0*f);
    float n = p.x + dot(p.yz,vec2(157.0,113.0));
    vec4 s1=mix(hash4(vec4(n)+NC0),hash4(vec4(n)+NC1),vec4(f.x));
    return mix(mix(s1.x,s1.y,f.y),mix(s1.z,s1.w,f.y),f.z);
}

float heightMapif(vec3 rad,float d)
{
    float iline=1.0/(1.0-line);
    float a=noise3(rad*1.6)*0.885;
    float na=a;
    if (a>line) a=pow((a-line)*iline,1.8)*(1.0-line)+line;
    if (abs(d-a)<0.2) 
    {
        na+=noise3(rad*8.0)*0.1;
        a=na;
        if (a>line) a=pow((a-line)*iline,1.8)*(1.0-line)+line;
    }
    if (abs(d-a)<0.02)
    {
        na+=noise3(rad*32.0)*0.01;
        a=na;
        if (a>line) a=pow((a-line)*iline,1.8)*(1.0-line)+line;
    }
    if (abs(d-a)<0.01) a+=noise3(rad*128.0)*0.005;
    return a;
}

float heightMap(vec3 rad)
{
    float iline=1.0/(1.0-line);
    float a=noise3(rad*1.6)*0.885+noise3(rad*8.0)*0.1
      +noise3(rad*32.0)*0.01+noise3(rad*64.0)*0.005;
    if (a>line) a=pow((a-line)*iline,1.8)*(1.0-line)+line;
    return a;
}

vec3 distObj(vec3 pos,vec3 ray,float radius,float minr)
{
    float b = dot(ray,pos);
    float c = dot(pos,pos) - b*b;
    
    float sta=radius-minr;
    float invm=1.0/sta;
    float rq=radius*radius;
    vec3 dist=ray*10000.0;
    if(c < rq)
    {
        vec3 r1 = (ray*(abs(b)-sqrt(rq-c))-pos);
   float maxs=abs(dot(r1,ray));
        if (c<minr*minr) 
   {
            vec3 r2 = (ray*(abs(b)-sqrt(minr*minr-c))-pos);
            maxs=maxs-abs(dot(r2,ray));
        }
        //else maxs*=0.5;
        
        maxs*=0.5;
        float len;
        float h;

        for (float m=0.0; (m<iterations); m+=1.0)
   {
            len=length(r1);
            vec3 d=r1/len;
            h=sta*heightMapif(d,(len-minr)*invm)+minr;
            //h=sta*heightMap(d)+minr;
            float ahl=abs(h-len);
            if (ahl<0.0001) break;
            maxs=abs(maxs);
            if (len<h) maxs=-maxs;
            r1+=ray*maxs*ahl;
            maxs*=0.99;
        }
        if (len<h+0.1) dist=r1+pos;
    }
    return dist;
}

void main()
{
    vec2 p = (resolution.xy - 2.0*gl_FragCoord.xy) / resolution.y;

    float mx = (mouse.x>0.0) ? mouse.x *TWO_PI  : 0.0;
    float my = (mouse.y>0.0) ? mouse.y *5.0-2.5 : 0.0;
    vec2 rotate = vec2(mx+time*ROTATION_ANGLE,my);

    vec2 s=sin(rotate);
    vec2 c=cos(rotate);
    mat3 mr=mat3(vec3(c.x,0.0,s.x),vec3(0.0,1.0,0.0),vec3(-s.x,0.0,c.x));
    mr=mr*mat3(vec3(1.0,0.0,0.0),vec3(0.0,c.y,s.y),vec3(0.0,-s.y,c.y));    
    
    vec3 ray = normalize(vec3(p,2.0));
    vec3 ray1 = normalize(vec3(p+vec2(0.0,0.01),2.0));
    vec3 ray2 = normalize(vec3(p+vec2(0.01,0.0),2.0));
    vec3 pos = vec3(0.0,0.0,3.0);
    
    vec3 light=vec3(-300.0,0.0,-300.0);

    vec3 n1=distObj(pos*mr,ray1*mr,pradius,mradius);
    vec3 n2=distObj(pos*mr,ray2*mr,pradius,mradius);
    vec3 rt=distObj(pos*mr,ray*mr,pradius,mradius);
    
    vec3 lightn=normalize(light*mr-rt);
    vec3 sd=distObj((pos-light)*mr,-lightn,pradius,mradius);
    
    float shadow=1.0-clamp(pow(length(sd+light*mr-rt),2.0)*200.0,0.0,1.0);
    vec3 n=normalize(cross(n1-rt,n2-rt));
    
    rt=rt-pos*mr;
    float fd=(length(rt)-mradius)/(pradius-mradius);
    float d=dot(n,lightn)*shadow;
    gl_FragColor.a = 1.0;
   if (fd<line) 
         gl_FragColor.xyz = d*mix(vec3(0.21,0.19,0.0),vec3(1.0,0.99,1.0),noise3(rt*128.0)*0.9+noise3(rt*8.0)*0.1);
    else gl_FragColor.xyz = d*mix(mix(vec3(1.0,1.0,0.9),vec3(0.8,0.79,0.7),noise3(rt*128.0)*0.9+noise3(rt*8.0)*0.1),vec3(1.0),pow(fd+0.5,10.0));  
    gl_FragColor = min( vec4(1.0), gl_FragColor );
}
