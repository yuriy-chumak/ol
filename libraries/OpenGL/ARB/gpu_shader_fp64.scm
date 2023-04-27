; ===========================================================================
; GL_ARB_gpu_shader_fp64
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/GL_ARB_gpu_shader_fp64.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB gpu_shader_fp64)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
(export ARB_gpu_shader_fp64

; ---------------------------------------------------------------------------
; New Procedures and Functions

   ;; glUniform1d ; void (int location, double x);
   ;; glUniform2d ; void (int location, double x, double y);
   ;; glUniform3d ; void (int location, double x, double y, double z);
   ;; glUniform4d ; void (int location, double x, double y, double z, double w);
   ;; glUniform1dv ; void (int location, sizei count, const double *value);
   ;; glUniform2dv ; void (int location, sizei count, const double *value);
   ;; glUniform3dv ; void (int location, sizei count, const double *value);
   ;; glUniform4dv ; void (int location, sizei count, const double *value);

   ;; glUniformMatrix2dv ; void (int location, sizei count, boolean transpose, const double *value);
   ;; glUniformMatrix3dv ; void (int location, sizei count, boolean transpose, const double *value);
   glUniformMatrix4dv ; void (int location, sizei count, boolean transpose, const double *value);
   ;; glUniformMatrix2x3dv ; void (int location, sizei count, boolean transpose, const double *value);
   ;; glUniformMatrix2x4dv ; void (int location, sizei count, boolean transpose, const double *value);
   ;; glUniformMatrix3x2dv ; void (int location, sizei count, boolean transpose, const double *value);
   ;; glUniformMatrix3x4dv ; void (int location, sizei count, boolean transpose, const double *value);
   ;; glUniformMatrix4x2dv ; void (int location, sizei count, boolean transpose, const double *value);
   ;; glUniformMatrix4x3dv ; void (int location, sizei count, boolean transpose, const double *value);

   ;; glGetUniformdv ; void (uint program, int location, double *params);

   ;; ; All of the following ProgramUniform* functions are supported
   ;; ;  if and only if EXT_direct_state_access is supported:

   ;; glProgramUniform1dEXT ; void (uint program, int location, double x);
   ;; glProgramUniform2dEXT ; void (uint program, int location, double x, double y);
   ;; glProgramUniform3dEXT ; void (uint program, int location, double x, double y, double z);
   ;; glProgramUniform4dEXT ; void (uint program, int location, double x, double y, double z, double w);
   ;; glProgramUniform1dvEXT ; void (uint program, int location, sizei count, const double *value);
   ;; glProgramUniform2dvEXT ; void (uint program, int location, sizei count, const double *value);
   ;; glProgramUniform3dvEXT ; void (uint program, int location, sizei count, const double *value);
   ;; glProgramUniform4dvEXT ; void (uint program, int location, sizei count, const double *value);

   ;; glProgramUniformMatrix2dvEXT ; void (uint program, int location, sizei count, boolean transpose, const double *value);
   ;; glProgramUniformMatrix3dvEXT ; void (uint program, int location, sizei count, boolean transpose, const double *value);
   ;; glProgramUniformMatrix4dvEXT ; void (uint program, int location, sizei count, boolean transpose, const double *value);
   ;; glProgramUniformMatrix2x3dvEXT ; void (uint program, int location, sizei count, boolean transpose, const double *value);
   ;; glProgramUniformMatrix2x4dvEXT ; void (uint program, int location, sizei count, boolean transpose, const double *value);
   ;; glProgramUniformMatrix3x2dvEXT ; void (uint program, int location, sizei count, boolean transpose, const double *value);
   ;; glProgramUniformMatrix3x4dvEXT ; void (uint program, int location, sizei count, boolean transpose, const double *value);
   ;; glProgramUniformMatrix4x2dvEXT ; void (uint program, int location, sizei count, boolean transpose, const double *value);
   ;; glProgramUniformMatrix4x3dvEXT ; void (uint program, int location, sizei count, boolean transpose, const double *value);

; ---------------------------------------------------------------------------
; New Tokens
   GL_DOUBLE         ; double
   GL_DOUBLE_VEC2    ; dvec2
   GL_DOUBLE_VEC3    ; dvec3
   GL_DOUBLE_VEC4    ; dvec4
   GL_DOUBLE_MAT2    ; dmat2
   GL_DOUBLE_MAT3    ; dmat3
   GL_DOUBLE_MAT4    ; dmat4
   GL_DOUBLE_MAT2x3  ; dmat2x3
   GL_DOUBLE_MAT2x4  ; dmat2x4
   GL_DOUBLE_MAT3x2  ; dmat3x2
   GL_DOUBLE_MAT3x4  ; dmat3x4
   GL_DOUBLE_MAT4x2  ; dmat4x2
   GL_DOUBLE_MAT4x3  ; dmat4x3

)

(import (only (OpenGL version-1-1) GL_DOUBLE))
; ---------------------------------------------------------------------------
(begin
   (define ARB_gpu_shader_fp64 (gl:QueryExtension "GL_ARB_gpu_shader_fp64"))
   (setq GL gl:GetProcAddress)

   (define GL_DOUBLE        GL_DOUBLE)
   (define GL_DOUBLE_VEC2      #x8FFC)
   (define GL_DOUBLE_VEC3      #x8FFD)
   (define GL_DOUBLE_VEC4      #x8FFE)
   (define GL_DOUBLE_MAT2      #x8F46)
   (define GL_DOUBLE_MAT3      #x8F47)
   (define GL_DOUBLE_MAT4      #x8F48)
   (define GL_DOUBLE_MAT2x3    #x8F49)
   (define GL_DOUBLE_MAT2x4    #x8F4A)
   (define GL_DOUBLE_MAT3x2    #x8F4B)
   (define GL_DOUBLE_MAT3x4    #x8F4C)
   (define GL_DOUBLE_MAT4x2    #x8F4D)
   (define GL_DOUBLE_MAT4x3    #x8F4E)

   ;; void Uniform1d(int location, double x);
   ;; void Uniform2d(int location, double x, double y);
   ;; void Uniform3d(int location, double x, double y, double z);
   ;; void Uniform4d(int location, double x, double y, double z, double w);
   ;; void Uniform1dv(int location, sizei count, const double *value);
   ;; void Uniform2dv(int location, sizei count, const double *value);
   ;; void Uniform3dv(int location, sizei count, const double *value);
   ;; void Uniform4dv(int location, sizei count, const double *value);

   ;; void UniformMatrix2dv(int location, sizei count, boolean transpose, const double *value);
   ;; void UniformMatrix3dv(int location, sizei count, boolean transpose, const double *value);
   (define glUniformMatrix4dv (GL GLvoid "glUniformMatrix4dv" GLint GLsizei GLboolean GLdouble*))
   ;; void UniformMatrix2x3dv(int location, sizei count, boolean transpose, const double *value);
   ;; void UniformMatrix2x4dv(int location, sizei count, boolean transpose, const double *value);
   ;; void UniformMatrix3x2dv(int location, sizei count, boolean transpose, const double *value);
   ;; void UniformMatrix3x4dv(int location, sizei count, boolean transpose, const double *value);
   ;; void UniformMatrix4x2dv(int location, sizei count, boolean transpose, const double *value);
   ;; void UniformMatrix4x3dv(int location, sizei count, boolean transpose, const double *value);

   ;; void GetUniformdv(uint program, int location, double *params);

   ; All of the following ProgramUniform* functions are supported
   ;  if and only if EXT_direct_state_access is supported:

   ;; void ProgramUniform1dEXT(uint program, int location, double x);
   ;; void ProgramUniform2dEXT(uint program, int location, double x, double y);
   ;; void ProgramUniform3dEXT(uint program, int location, double x, double y, double z);
   ;; void ProgramUniform4dEXT(uint program, int location, double x, double y, double z, double w);
   ;; void ProgramUniform1dvEXT(uint program, int location, sizei count, const double *value);
   ;; void ProgramUniform2dvEXT(uint program, int location, sizei count, const double *value);
   ;; void ProgramUniform3dvEXT(uint program, int location, sizei count, const double *value);
   ;; void ProgramUniform4dvEXT(uint program, int location, sizei count, const double *value);

   ;; void ProgramUniformMatrix2dvEXT(uint program, int location, sizei count, boolean transpose, const double *value);
   ;; void ProgramUniformMatrix3dvEXT(uint program, int location, sizei count, boolean transpose, const double *value);
   ;; void ProgramUniformMatrix4dvEXT(uint program, int location, sizei count, boolean transpose, const double *value);
   ;; void ProgramUniformMatrix2x3dvEXT(uint program, int location, sizei count, boolean transpose, const double *value);
   ;; void ProgramUniformMatrix2x4dvEXT(uint program, int location, sizei count, boolean transpose, const double *value);
   ;; void ProgramUniformMatrix3x2dvEXT(uint program, int location, sizei count, boolean transpose, const double *value);
   ;; void ProgramUniformMatrix3x4dvEXT(uint program, int location, sizei count, boolean transpose, const double *value);
   ;; void ProgramUniformMatrix4x2dvEXT(uint program, int location, sizei count, boolean transpose, const double *value);
   ;; void ProgramUniformMatrix4x3dvEXT(uint program, int location, sizei count, boolean transpose, const double *value);
))
