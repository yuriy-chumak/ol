#version 120 // OpenGL 2.1

uniform float time;
uniform vec2 dimensions;

#ifdef GL_ES
precision mediump float;
#endif

// shadertoy globals
float iTime = 0.0;
vec3  iResolution = vec3(0.0);

// --------[ Original ShaderToy begins here ]---------- //
#define M_PI 3.1415926535897932384626433832795
#define M_PI05 (M_PI * 0.5)

vec2 rotate(vec2 v, float c, float s){
	return vec2(v.x*c - v.y*s, v.x*s + v.y*c);
}

vec2 rotate(vec2 v, float r){
	return rotate(v, cos(r), sin(r));
}

float boxLength(vec2 pos) {
	vec2 q = abs(pos);
	return max(q.x, q.y);
}

float capsuleLength(vec2 pos, vec2 dir) {
	vec2 ba = -dir;
	vec2 pa = pos + ba;
	ba *= 2.0;
	return length(pa - ba * clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0));
} 

float triangleLength(vec2 p) {
    p.y += 0.32;
	return max(abs(p.x * 1.8) + p.y, 1.0 - p.y * 1.8) * 0.75;
}

vec2 fracOrigin(vec2 v){
	return (fract(v) - 0.5) * 2.0;
}

float Bu(vec2 pos){
 	float a = capsuleLength(pos + vec2(0.0, -0.5), vec2(1.0, 0.0));   
 	float b = capsuleLength(pos + vec2(-0.3, 0.3), vec2(1.0, 1.0) * 0.707);  
    float c = length(pos + vec2(-1.3, -1.3));
    float d = length(pos + vec2(-1.8, -1.3));
    return min(min(min(a, b), c), d);
}

float Ri(vec2 pos){
 	float a = capsuleLength(pos + vec2(-0.5, 0.0), vec2(0.0, 1.0));   
 	float b = capsuleLength(pos + vec2(0.0, 0.0), vec2(0.1, -0.8) * 0.4);    
    return min(a, b);
}

float To(vec2 pos){ 	
    float a = capsuleLength(pos + vec2(0.0, -0.7), vec2(0.5, 0.0));   
 	float b = capsuleLength(pos + vec2(-0.3, -0.3), vec2(0.3, 1.3));  
    float c = capsuleLength(pos + vec2(0.3, -0.5), vec2(0, 0.5)); 
    return min(min(a, b), c);
}

float Ba(vec2 pos){
 	float a = capsuleLength(pos + vec2(0.8, 0.0), vec2(0.3, 1.0));   
 	float b = capsuleLength(pos + vec2(-0.8, 0.0), vec2(-0.3, 1.0));     
    float c = length(pos + vec2(-1.3, -1.3));
    float d = length(pos + vec2(-1.8, -1.3));
    return min(min(min(a, b), c), d);
}

float Burisaba(vec2 pos, float power){
    float ret = 0.0
     + power / Bu(pos)
     + power / Ri(pos + vec2(-3.0, 0.0))
     + power / To(pos + vec2(-6.0, 0.0))
     + power / Ba(pos + vec2(-9.0, 0.0))
        ;
    
    return ret;
}

float smoothstepLine(float lower, float upper, float value, float width){
    width *= 0.5;
    return smoothstep(lower - width, lower, value) * (1.0 - smoothstep(upper, upper + width, value));
}

float smoothLine(float value, float target, float width){
    return width / abs(value - target);
}

vec2 smoothLine2(float value, float target, float width){
    return vec2(step(0.0, value - target), width / abs(value - target));
}

float circleTriangle(vec2 pos){
    float circle = length(pos * 0.5);
    float triangle = triangleLength(pos * 0.3);    
    return smoothLine(circle, 1.0, 0.025) + smoothLine(triangle, 1.0, 0.025);
}

vec2 circleTriangle2(vec2 pos){
    float circle2 = length(pos * 0.35);
    vec2 ret = smoothLine2(circle2, 1.0, 0.025);
    ret.y += circleTriangle(pos);
    return ret;
}

float atan2(in float y, in float x)
{
    return x == 0.0 ? sign(y) * M_PI05 : atan(y, x);
}

vec2 polar(vec2 uv) {
	float r = length(uv);
	float s = atan2(uv.y, uv.x) / M_PI;
	return vec2(r, s);
}

float BurisabaCircle(vec2 pos){
    vec2 pp = polar(rotate(pos, -iTime) * 0.75);
    return Burisaba(mod(rotate(pp * vec2(2.0, 32.0), M_PI05), vec2(16.0, 4.0)) - 1.5, 0.05) * smoothstepLine(6.0, 7.5, pp.x, 1.5);
}

float BurisabaCircle2(vec2 pos, float scale, float x, float y, float x2, float y2, float lower, float upper, float r){
    vec2 pp = polar(rotate(pos, r) * scale);
    return Burisaba(mod(rotate(pp * vec2(x, y), M_PI05), vec2(x2, y2)) - 1.5, 0.03) * smoothstepLine(lower, upper, pp.x, 0.2);
}

void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 uv = (fragCoord.xy - iResolution.xy * 0.5) / iResolution.yy * 20.0;     
      
    uv *= clamp(iTime * 0.25, 0.0, 1.0);
    
    vec3 col = vec3(0.0, 0.0, 0.0);
        
    uv = rotate(uv, iTime * 0.3);
    
    vec2 c2 = circleTriangle2(uv * 1.4 + vec2(0.0, 8.0));
    vec2 c3 = circleTriangle2(uv * 1.4 + rotate(vec2(0.0, 8.0), M_PI * 2.0 * 0.3333));
    vec2 c4 = circleTriangle2(uv * 1.4 + rotate(vec2(0.0, 8.0), M_PI * 2.0 * 0.6666));
    
    float mask = c2.x * c3.x * c4.x;
    
    float len = length(uv);
    
    col.g = BurisabaCircle(uv)
  		
        + (BurisabaCircle2(uv, 0.995, 8.0, 64.0, 12.0, 4.0, 7.5, 8.0, 5.0 + iTime * 0.2)
        + smoothLine(len, 8.0, 0.02)
        + smoothLine(len, 7.5, 0.02)
        
        + BurisabaCircle2(uv, 1.1, 8.0, 64.0, 12.0, 4.0, 7.5, 7.9, 5.0 + iTime * 0.7)
        + BurisabaCircle2(uv, 1.2, 8.0, 64.0, 12.0, 4.0, 7.5, 7.9, 15.0 + iTime * 0.1564)
        
        + BurisabaCircle2(uv, 1.45, 8.0, 64.0, 12.0, 4.0, 7.5, 7.9, 15.0 + iTime * 0.2418654)
        + smoothLine(len, 5.0, 0.02)
        + smoothLine(len, 5.5, 0.02)
        
        + BurisabaCircle2(uv, 2.15, 8.0, 64.0, 12.0, 4.0, 7.5, 7.9, 35.0 + iTime * 0.34685)
        + BurisabaCircle2(uv, 2.25, 8.0, 64.0, 12.0, 4.0, 7.5, 7.9, 135.0 + iTime * 0.114)
        + BurisabaCircle2(uv, 1.8, 8.0, 64.0, 12.0, 4.0, 7.5, 7.9, 532.0 + iTime * 0.54158)
        + 0.005 / abs(boxLength(rotate(uv, M_PI05 * 0.0 - iTime * 0.5)) - 4.5)
        + 0.005 / abs(boxLength(rotate(uv, M_PI05 * 0.25 - iTime * 0.5)) - 4.5)
        + 0.005 / abs(boxLength(rotate(uv, M_PI05 * 0.5 - iTime * 0.5)) - 4.5)
        + 0.005 / abs(boxLength(rotate(uv, M_PI05 * 0.75 - iTime * 0.5)) - 4.5)
        + 0.1 / abs(boxLength(uv * vec2(8.0, 0.5) - vec2(0.0, 2.9)) - 1.0)
        + 0.1 / abs(boxLength(rotate(uv, M_PI * 2.0 * 0.3333) * vec2(8.0, 0.5) - vec2(0.0, 2.9)) - 1.0)
        + 0.1 / abs(boxLength(rotate(uv, M_PI * 2.0 * 0.6666) * vec2(8.0, 0.5) - vec2(0.0, 2.9)) - 1.0)
           
          ) * mask
      
        + circleTriangle(uv) 
        + c2.y
    	+ c3.y
     	+ c4.y
        ;
   
    //fragColor = texture(iChannel0, fragCoord.xy / iResolution.xy) + vec4(col, 1.0);
        
    fragColor = vec4(col, 1.0);
}

// --------[ Original ShaderToy ends here ]---------- //

void main(void)
{
    iTime = time;
    iResolution = vec3(dimensions, 0.0);

    mainImage(gl_FragColor, gl_FragCoord.xy);
}