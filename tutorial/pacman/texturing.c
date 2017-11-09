#include <GL/gl.h>
#include <GL/glu.h>
#include <png.h>

#include <string.h>

#define TEXTURE_LOAD_ERROR 0

/** loadTexture
 *   loads a png file into an opengl texture object, using cstdio , libpng, and opengl.
 * 
 *   \param filename : the png file to be loaded
 *   \param width : width of png, to be updated as a side effect of this function
 *   \param height : height of png, to be updated as a side effect of this function
 * 
 *   \return GLuint : an opengl texture id.  Will be 0 if there is a major error,
 *                           should be validated by the client of this function.
 * 
 */
GLuint loadTexture(const char* filename, int *width, int *height)
{
  //header for testing if it is a png
  png_byte header[8];
  
  //open file as binary
  FILE *fp = fopen(filename, "rb");
  if (!fp) {
    return TEXTURE_LOAD_ERROR;
  }
  
  //read the header
  fread(header, 1, 8, fp);
  
  //test if png
  int is_png = !png_sig_cmp(header, 0, 8);
  if (!is_png) {
    fclose(fp);
    return TEXTURE_LOAD_ERROR;
  }
  
  //create png struct
  png_structp png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL,
      NULL, NULL);
  if (!png_ptr) {
    fclose(fp);
    return (TEXTURE_LOAD_ERROR);
  }
  
  //create png info struct
  png_infop info_ptr = png_create_info_struct(png_ptr);
  if (!info_ptr) {
    png_destroy_read_struct(&png_ptr, (png_infopp) NULL, (png_infopp) NULL);
    fclose(fp);
    return (TEXTURE_LOAD_ERROR);
  }

  //create png info struct
  png_infop end_info = png_create_info_struct(png_ptr);
  if (!end_info) {
    png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp) NULL);
    fclose(fp);
    return (TEXTURE_LOAD_ERROR);
  }

  //png error stuff, not sure libpng man suggests this.
  if (setjmp(png_jmpbuf(png_ptr))) {
    png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);
    fclose(fp);
    return (TEXTURE_LOAD_ERROR);
  }

  //init png reading
  png_init_io(png_ptr, fp);
  
  //let libpng know you already read the first 8 bytes
  png_set_sig_bytes(png_ptr, 8);

  // read all the info up to the image data
  png_read_info(png_ptr, info_ptr);

  //variables to pass to get info
  int bit_depth, color_type;
  png_uint_32 twidth, theight;

  // get info about png
  png_get_IHDR(png_ptr, info_ptr, &twidth, &theight, &bit_depth, &color_type,
      NULL, NULL, NULL);

  //update width and height based on png info
  if (width) *width = twidth;
  if (height) *height = theight;

  // Update the png info struct.
  png_read_update_info(png_ptr, info_ptr);

  // Row size in bytes.
  int rowbytes = png_get_rowbytes(png_ptr, info_ptr);

  // Allocate the image_data as a big block, to be given to opengl
  png_byte *image_data = (png_byte*) malloc(rowbytes * theight * sizeof(png_byte));
  if (!image_data) {
    //clean up memory and close stuff
    png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);
    fclose(fp);
    return TEXTURE_LOAD_ERROR;
  }

  //row_pointers is for pointing to image_data for reading the png with libpng
  png_bytep *row_pointers = (png_bytep*) malloc(theight * sizeof(png_bytep));
  if (!row_pointers) {
    //clean up memory and close stuff
    png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);
    free(image_data);
    fclose(fp);
    return TEXTURE_LOAD_ERROR;
  }
  // set the individual row_pointers to point at the correct offsets of image_data
  for (int i = 0; i < theight; ++i)
    row_pointers[theight - 1 - i] = image_data + i * rowbytes;

  //read the png into image_data through row_pointers
  png_read_image(png_ptr, row_pointers);

  //Now generate the OpenGL texture object
  GLuint texture;
  glGenTextures(1, &texture);
  glBindTexture(GL_TEXTURE_2D, texture);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, twidth, theight, 0,
      GL_RGBA, GL_UNSIGNED_BYTE, (GLvoid*) image_data);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

/*  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);*/


  //clean up memory and close stuff
  png_destroy_read_struct(&png_ptr, &info_ptr, &end_info);
  free(image_data);
  free(row_pointers);
  fclose(fp);

  return texture;
}
