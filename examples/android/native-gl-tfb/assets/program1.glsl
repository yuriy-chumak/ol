#version 120 // OpenGL 2.1

uniform float time;
uniform vec2 dimensions;

// --------[ Original ShaderToy begins here ]---------- //
vec2 rand(vec2 p)
{
    float n = sin(dot(p, vec2(1, 10)));
    return fract(vec2(262144, 32768)*n);     
}


float sdRoundBox(vec2 p, vec2 b, float r)
{
  vec2 q = abs(p) - b;
  return length(max(q,.0)) + min(max(q.x,q.y),.0) - r;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    vec3 col;
    vec2 uv = (fragCoord-dimensions.xy*.5)/dimensions.y;
    float g = smoothstep(.0, .25, uv.y+.0015)+.25;
	uv.y = 1. - abs(uv.y);
    uv *= 10.;
    vec2 fl = floor(uv);
    vec2 fr = (fract(uv)-.5)*2.;
    float a = sdRoundBox(fr, vec2(.7, .7), .1);
    float s = smoothstep(0.1, 0.01, a);
    //vec2 ran = rand(fl);
	vec2 ran = rand(fl);
    float t = pow(abs(sin(ran.x*6.2831 + time*(0.25)*(.25 + ran.y))), 15.);
    //col = vec3(ran.x * ran.y, ran.y, .0)*g*s*t;
col = vec3(ran.y*ran.x/5., ran.y*1.5, 3.)*(g)*(s)*t;
    fragColor = vec4(col,1.);
}
// --------[ Original ShaderToy ends here ]---------- //

void main(void)
{
    mainImage(gl_FragColor, gl_FragCoord.xy);
}