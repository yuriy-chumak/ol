#version 120 // OpenGL 2.1

uniform float time;
uniform vec2 dimensions;

#ifdef GL_ES
precision mediump float;
#endif

#define tileSize 0.08
#define charScale 1.2

vec2 norm(vec2 pos) {
	return fract(pos * tileSize) * 2.0 - vec2(1.0, 1.0);
}
float rnd(float t, vec2 i) {
	float mag = length(i);
	return floor(1.0 + 0.5 * sin(t + mag) + 0.5 * sin(2. * t + 3. * mag));
}
float antialias(float diff) {
	return 1.0 - clamp(diff * 10.0, 0.0, 68.);
}
float ellipse(vec2 pos, vec2 c, vec2 r) {
	vec2 d = pos - c;
	return antialias((d.x * d.x * r.x * r.x) + (d.y * d.y * r.y * r.y) - (r.x * r.x * r.y * r.y));
}
float rect(vec2 pos, vec2 c, vec2 r) {
	vec2 d = (pos - c);
	return (abs(d.x) < r.x) && (abs(d.y) < r.y) ? 1.0 : 0.0;
}
float character0(vec2 posraw) {
	vec2 pos = posraw * charScale;
	return clamp(ellipse(pos, vec2(0, 0), vec2(1.0, 0.8)) - ellipse(pos, vec2(0.0, 0.0), vec2(0.64, 0.5)), 0.0, 1.0);
}
float character1(vec2 posraw) {
	vec2 pos = posraw * charScale;
	return clamp(rect(pos, vec2(-0.25, 0.8), vec2(0.25, 0.2)) + rect(pos, vec2(0.0, 0.0), vec2(0.2, 1.0)), 0.0, 1.0);
}
void main( void ) {
	vec2 scaledNormalCoords = norm(gl_FragCoord.xy) * charScale;
	vec2 charIndex = floor(gl_FragCoord.xy * tileSize);
	float loadIndex = rnd(time, charIndex);
	if (loadIndex < 2.0) {
		gl_FragColor = vec4(0.0, character0(scaledNormalCoords), 0.0, 1.0);
	}
	if (loadIndex < 1.0) {
		gl_FragColor = vec4(0.0, character1(scaledNormalCoords), 0.0, 1.0);
	}
	else {
		gl_FragColor = vec4(0.0, character0(scaledNormalCoords), 0.0, 1.0);
	}
}