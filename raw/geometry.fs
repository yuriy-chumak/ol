#version 120 // OpenGL 2.1
// http://glslsandbox.com/e#19291.0
uniform float time;
	
const int MAXITER = 80;
vec3 field(vec3 p) {
	p *= 0.1;
	float f = 0.1;
	for (int i = 0; i < 5; i++) {
		p = p.yzx * mat3(0.8, 0.6, 0.0,-0.6, 0.8, 0.0, 0.0, 0.0, 1.0);
		p += vec3(0.123, 0.456, 0.789) * float(i);
		p = abs(fract(p) - 0.5);
		p *= 2.0;
		f *= 2.0;
	}
	p *= p;
	return sqrt(p + p.yzx) / f - 0.002;
}

void main(void) {
	vec2 viewport = vec2(1280, 720);
	vec2 position = gl_FragCoord.xy / viewport.xy;

	vec3 dir = normalize(vec3((gl_FragCoord.xy - viewport * 0.5) / viewport.x, 1.0));
	vec3 pos = vec3(0.5, 0.0, time);
	vec3 color = vec3(0.0);
	for (int i = 0; i < MAXITER; i++) {
		vec3 f2 = field(pos);
		float f = min(min(f2.x, f2.y), f2.z);
		
		pos += dir * f;
		color += float(MAXITER - i) / (f2 + 0.001);
	}
	vec3 color3 = vec3(0.17 / (color * (0.09 / float (MAXITER*MAXITER))));
	color3 *= color3;
	gl_FragColor = 1.0 - vec4(color3.zyx, 0.0);
}
