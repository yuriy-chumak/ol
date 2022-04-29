#version 120 // OpenGL 2.1
#define gl_WorldMatrix gl_TextureMatrix[7]

varying vec4 vertexPosition;
varying vec4 vertexNormal;

varying vec4 fragPosLightSpace;

void main() {

	// Подготовительные вектора
	vertexPosition = gl_ModelViewMatrix * gl_WorldMatrix * gl_Vertex; // vertex position in the modelview space (not just in world space)
	vertexNormal   = gl_ModelViewMatrix * gl_WorldMatrix * vec4(gl_Normal, 0.0);

	gl_Position = gl_ProjectionMatrix * vertexPosition;
	gl_FrontColor = gl_Color;
	gl_BackColor = vec4(0.0,0.0,0.0, 1);
	gl_TexCoord[0] = gl_MultiTexCoord0;

	fragPosLightSpace = gl_TextureMatrix[2] * gl_WorldMatrix * gl_Vertex; // position from the sun view point
}
