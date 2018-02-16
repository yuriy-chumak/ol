// http://glslsandbox.com/e#30651.0
// Perlin noise implementation by Omar El Sayyed.more information, watch this awesome presentation by Ken Perlin: http://noisemachine.com/talk1/
//
// For generating the scrambled sequence of permutations, I used the method described 
// by Jeff Preshing here: preshing.com/20121224/how-to-generate-a-sequence-of-unique-random-integers/
//
// For generating the gradient vectors from a 8 bit index, I scrambled the index using the same method
// to give the effect of randomness. Then I rotated a unit vector around the z-axis using the index 
// lower 4 bits as angle and around y-axis using the upper 4 bits. That's it.
//
// Note that this implementation is far from optimal.
//
// Join us on our quest for learning shaders: https://www.facebook.com/groups/748114015233903/
// and please like our page :D
// https://www.facebook.com/nomonesoftware

#ifdef GL_ES
precision mediump float;
#endif

uniform float time;
uniform vec2 resolution;
uniform sampler2D backbuffer;

#define PI 3.14159265

//////////////////
// Perlin noise
//////////////////

//#define LINEAR_INTERPOLATION

float permutation(float index) {
   return mod(index * index, 257.0);
}

vec3 gradient(float index) {
   
   index = mod(index * index, 251.0);
   
   float angleAroundZ = mod(index, 16.0) * (2.0 * PI / 16.0);
   float angleAroundY = floor(index / 16.0) * (2.0 * PI / 16.0);
   
   vec3 gradient = vec3(cos(angleAroundZ), sin(angleAroundZ), 0.0);
   vec3 rotatedGradient;
   rotatedGradient.x = gradient.x * cos(angleAroundY);
   rotatedGradient.y = gradient.y;
   rotatedGradient.z = gradient.x * sin(angleAroundY);

   return rotatedGradient;
}
 
float hermit3D(vec3 position) {
   vec3 square = position * position;
   vec3 cube = square * position;
   return (3.0*square.x - 2.0*cube.x) * (3.0*square.y - 2.0*cube.y) * (3.0*square.z - 2.0*cube.z);
}

float perlinNoise3D(int gridWidth, int gridHeight, int gridDepth, vec3 position) {

   // Takes input position in the interval [0, 1] in all axes, outputs noise in the range [0, 1].
   vec3 gridDimensions = vec3(gridWidth, gridHeight, gridDepth);
   position *= gridDimensions;
      
   // Get corners,
   vec3 lowerBoundPosition = floor(position);

   // Calculate gradient values!
   float gradientValues[8];
   for (float z=0.0; z<2.0; z++) {
      for (float y=0.0; y<2.0; y++) {
         for (float x=0.0; x<2.0; x++) {
            vec3 currentPointPosition = lowerBoundPosition + vec3(x, y, z);
            
            vec3 displacementVector = (currentPointPosition - position);
            vec3 gradientVector = gradient(mod(currentPointPosition.x + permutation(mod(currentPointPosition.y + permutation(currentPointPosition.z), 256.0)), 256.0));
            
            gradientValues[int((z*4.0) + (y*2.0) + x)] = (0.0 + dot(gradientVector, displacementVector)) * 2.0;
         }
      }
   }
   
   #ifdef LINEAR_INTERPOLATION
   
   // Interpolate linearly (for simplicity),
   vec3 interpolationRatio = position - lowerBoundPosition;
   float nearPlaneNoise = mix(
      mix(gradientValues[0], gradientValues[1], interpolationRatio.x),
      mix(gradientValues[2], gradientValues[3], interpolationRatio.x),
      interpolationRatio.y);
   float farPlaneNoise = mix(
      mix(gradientValues[4], gradientValues[5], interpolationRatio.x),
      mix(gradientValues[6], gradientValues[7], interpolationRatio.x),
      interpolationRatio.y);
   
   float finalNoise = mix(nearPlaneNoise, farPlaneNoise, interpolationRatio.z);
   
   #else
   
   // Interpolate using Hermit,
   vec3 interpolationRatio = position - lowerBoundPosition;
   float finalNoise = 0.0;
   finalNoise += gradientValues[7] * hermit3D(interpolationRatio);
   finalNoise += gradientValues[6] * hermit3D(vec3(1.0 - interpolationRatio.x,       interpolationRatio.y,       interpolationRatio.z));
   finalNoise += gradientValues[5] * hermit3D(vec3(      interpolationRatio.x, 1.0 - interpolationRatio.y,       interpolationRatio.z));
   finalNoise += gradientValues[4] * hermit3D(vec3(1.0 - interpolationRatio.x, 1.0 - interpolationRatio.y,       interpolationRatio.z));

   finalNoise += gradientValues[3] * hermit3D(vec3(      interpolationRatio.x,       interpolationRatio.y, 1.0 - interpolationRatio.z));
   finalNoise += gradientValues[2] * hermit3D(vec3(1.0 - interpolationRatio.x,       interpolationRatio.y, 1.0 - interpolationRatio.z));
   finalNoise += gradientValues[1] * hermit3D(vec3(      interpolationRatio.x, 1.0 - interpolationRatio.y, 1.0 - interpolationRatio.z));
   finalNoise += gradientValues[0] * hermit3D(vec3(1.0 - interpolationRatio.x, 1.0 - interpolationRatio.y, 1.0 - interpolationRatio.z));
   
   #endif
      
   return finalNoise;
}

//////////////////
// Scene
//////////////////

const vec3 eyePosition = vec3(0.0, 0.0, -2.0);

vec3 sphereCenter = vec3(0.0, 0.0, 0.5);
const float sphereRadius = 0.25;

float intersectSphere(vec3 rayVector, vec3 pointOnRay, vec3 sphereCenter, float sphereRadius) {
  
  vec3 sphereCenterToRayPoint = pointOnRay - sphereCenter;
  
  float a = dot(rayVector, rayVector);
  float b = 2.0*dot(rayVector, sphereCenterToRayPoint);
  float c = dot(sphereCenterToRayPoint, sphereCenterToRayPoint) - sphereRadius*sphereRadius;
  
  float descriminant = b*b - 4.0*a*c;
  if (descriminant < 0.0) return -1.0;  
  
  float t = (-b - sqrt(descriminant)) / (2.0*a);
  
  return t;
}

vec4 light(vec3 pointPosition, vec3 pointNormal, vec3 lightPostiion, vec4 ambientColor, vec4 diffuseColor, vec4 specularColor, float shininess) {  
  
  vec3 L = normalize(lightPostiion - pointPosition);   
  vec3 E = normalize(eyePosition - pointPosition); 
  vec3 R = normalize(-reflect(L, pointNormal));  
 
  // Calculate ambient term,
  vec4 ambient = ambientColor;

  // Calculate diffuse term,
  vec4 diffuse = diffuseColor * max(dot(pointNormal,L), 0.0);
  diffuse = clamp(diffuse, 0.0, 1.0);     

  // Calculate specular term,
  vec4 specular = specularColor * pow(max(dot(R,E),0.0),0.3*shininess);
  specular = clamp(specular, 0.0, 1.0); 

  return ambient + diffuse + specular;
}

vec4 getSumNeighbors() {
  vec2 pixelDisplacement = 1.0 / resolution;
  vec2 backBufferPosition = gl_FragCoord.xy / resolution;
  return 
    texture2D(backbuffer, vec2(backBufferPosition.x + pixelDisplacement.x, backBufferPosition.y)) + 
    texture2D(backbuffer, vec2(backBufferPosition.x, backBufferPosition.y + pixelDisplacement.y)) + 
    texture2D(backbuffer, vec2(backBufferPosition.x - pixelDisplacement.x, backBufferPosition.y)) + 
    texture2D(backbuffer, vec2(backBufferPosition.x, backBufferPosition.y - pixelDisplacement.y)); 
}

void main() {
  
  // Normalize coordinates,
  vec3 pointPosition = vec3((gl_FragCoord.xy / resolution)*2.0 - 1.0, 0.0);
  pointPosition.y *= resolution.y / resolution.x;
   pointPosition*=0.5;
    
  // Ray cast,
  vec3 rayVector = pointPosition - eyePosition;
  float t = intersectSphere(rayVector, eyePosition, sphereCenter, sphereRadius);

  // Light,
  vec3 lightPosition = vec3(0.25, 0.25, 0.0);
  vec4 lightColor;
  vec4 color;

  float timeInterval = mod(time, 20.0);

  if (t == -1.0) {
    if (timeInterval > 15.0) {
      // Effect #4,
      float noise = 0.0;
      vec3 noisePoint = vec3(pointPosition.xy, pointPosition.z-(time*0.06));
      noise +=         abs(perlinNoise3D(4, 4, 4, noisePoint));
      noise += 0.500 * abs(perlinNoise3D(8, 8, 8, noisePoint));
      noise += 0.250 * abs(perlinNoise3D(16, 16, 16, noisePoint));
      noise += 0.125 * abs(perlinNoise3D(32, 32, 32, noisePoint));
      noise += 0.0625 * abs(perlinNoise3D(64, 64, 64, noisePoint));
       
      float radius = length(pointPosition) - 1.0*sphereRadius;
      radius /= sphereRadius * 0.4;
       
      float phase = clamp(radius + 1.0*noise, 0.0, 0.5*PI);
      radius = sin(phase);
      vec4 color = mix(vec4(0.8, 0.55, 0.2, 1.0), vec4(1.0, 0.1, 0.0, 1.0), radius);
      color.rgb *= 1.0 - radius;
      gl_FragColor = color;
    } else {
      //gl_FragColor = vec4(0.0);
      // A bit of blur,
      gl_FragColor = getSumNeighbors() * 0.235;
    }
    return ;
  }
  
  vec3 intersectionPoint = eyePosition + (t * rayVector);
  vec3 normal = normalize(intersectionPoint - sphereCenter);
  
  if (timeInterval < 5.0) {
    // Effect #1,
    color = vec4(0.5 + 0.5 * perlinNoise3D(10, 10, 10, vec3(intersectionPoint.xy, intersectionPoint.z+(time*0.1))));
    lightColor = light(
      intersectionPoint, normal, 
      lightPosition, 
      vec4(0.06, 0.06, 0.3, 1.0), 
      vec4(0.5, 0.5, 1.2, 1.0), 
      vec4(0.8, 0.8, 0.8, 1.0),
      40.0);        
     
  } else if (timeInterval < 10.0) {
    // Effect #2,
    float noise = 0.0;
    noise +=         abs(0.5 + 0.5*perlinNoise3D(10, 10, 10, vec3(intersectionPoint.xy, intersectionPoint.z+(time*0.05))));
    noise += 0.500 * abs(0.5 + 0.5*perlinNoise3D(20, 20, 20, vec3(intersectionPoint.xy, intersectionPoint.z+(time*0.05))));
    noise += 0.250 * abs(0.5 + 0.5*perlinNoise3D(40, 40, 40, vec3(intersectionPoint.xy, intersectionPoint.z+(time*0.05))));
    noise += 0.125 * abs(0.5 + 0.5*perlinNoise3D(80, 80, 80, vec3(intersectionPoint.xy, intersectionPoint.z+(time*0.05))));
   
    noise = (1.0 + sin(intersectionPoint.x*45.0 + noise*5.0)) * 0.5;
    color = mix(vec4(0.4, 0.25, 0.1, 1.0), vec4(0.8, 0.7, 0.6, 1.0), noise);

    lightColor = light(
      intersectionPoint, normal, 
      lightPosition, 
      vec4(0.3, 0.3, 0.3, 1.0), 
      vec4(1.2, 1.2, 1.2, 1.0), 
      vec4(0.8, 0.8, 0.8, 1.0),
      40.0);
     
  } else if (timeInterval < 15.0) {
    // Effect #3,
    float noise = 0.0;
    noise +=         abs(perlinNoise3D(4, 4, 4, vec3(intersectionPoint.xy, intersectionPoint.z-(time*0.1))));
    noise += 0.500 * abs(perlinNoise3D(8, 8, 8, vec3(intersectionPoint.xy, intersectionPoint.z-(time*0.1))));
    noise += 0.250 * abs(perlinNoise3D(16, 16, 16, vec3(intersectionPoint.xy, intersectionPoint.z-(time*0.1))));
    noise += 0.125 * abs(perlinNoise3D(32, 32, 32, vec3(intersectionPoint.xy, intersectionPoint.z-(time*0.1))));
    noise += 0.0625 * abs(perlinNoise3D(64, 64, 64, vec3(intersectionPoint.xy, intersectionPoint.z-(time*0.1))));

    color = mix(vec4(0.8, 0.55, 0.2, 1.0), vec4(0.5, 0.1, 0.0, 1.0), noise);

    lightColor = light(
      intersectionPoint, normal, 
      lightPosition, 
      vec4(0.3, 0.3, 0.3, 1.0), 
      vec4(1.2, 1.2, 1.2, 1.0), 
      vec4(0.8, 0.8, 0.8, 1.0),
      40.0);
  } else {
    // A bit of blur,
    color = getSumNeighbors() * 0.235;
    lightColor = vec4(1.0);
  }
     
  gl_FragColor = lightColor * color;
}