#version 120 // OpenGL 2.1

uniform float time;
uniform vec2 dimensions;

#ifdef GL_ES
precision mediump float;
#endif

//#extension GL_OES_standard_derivatives : enable

const float PI = acos(-1.0);
const float TAU = PI * 2.0;
const float phi = sqrt(5.0) * 0.5 + 0.5;

const float goldenAngle = TAU / phi / phi;

#define time time*4.
vec2 rotateAroundPoint(float x){
	return vec2(sin(x), cos(x));
}

vec3 hsv2rgb(vec3 c)
{
    const vec4 K = vec4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
	
    vec3 p = abs(fract(c.xxx + K.xyz) * 6.0 - K.www);
    return c.z * mix(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}

vec3 calculateGoldenShape(vec2 p){
	const int steps =64;
	const float rSteps = 1.0 / float(steps);
	
	vec3 result = vec3(0.0);
	
	for (int i = steps; i > 0; --i)
	{
		float n = float(i);
		
		float inc = n * rSteps;
		vec2 offset = rotateAroundPoint(fract(time*0.055)*6.28+n * goldenAngle*cos(time*0.0025)) * inc * 0.45;
		
		vec3 dist = vec3(distance(p, offset));
		     dist = exp2(-dist * 64.0) * hsv2rgb(vec3(fract(time*0.2)+inc*0.75, 1.0, 1.0));
		
		result = max(result, dist);
	}
	
	return result;
}

void main( void ) {

	vec2 position = (gl_FragCoord.xy / dimensions.xy - 0.5) * vec2(dimensions.x/ dimensions.y, 1.0);

	vec3 color = vec3(0.0);
	     color += sin(calculateGoldenShape(position) * 3.0);
	     color = pow(color, vec3(2.2));
	     color /= sin(color + 1.0);
	     color = pow(color, vec3(1.0 / 2.2));

	
	gl_FragColor = vec4(color, 1.0 );

}