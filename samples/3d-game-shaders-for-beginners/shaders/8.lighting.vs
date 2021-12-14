#version 120 // OpenGL 2.1
#define gl_WorldMatrix gl_TextureMatrix[7]

varying vec4 vertexPosition;
varying vec4 vertexNormal;

void main() {

	// Подготовительные вектора
	vertexPosition = gl_ModelViewMatrix * gl_WorldMatrix * gl_Vertex; // vertex position in the modelview space (not just in world space)
	vertexNormal   = gl_ModelViewMatrix * gl_WorldMatrix * vec4(gl_Normal, 0.0);

	gl_Position = gl_ProjectionMatrix * vertexPosition;
	gl_FrontColor = gl_Color;
	gl_BackColor = vec4(0.0,0.0,0.0, 1);
}