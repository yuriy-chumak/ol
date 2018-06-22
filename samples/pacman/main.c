/*
 * This sample demonstrates embedding the Otus Lisp into C projects.
 *
 * We draw the simple movable pacman (using C for rendering and pacman move),
 * and Otus Lisp as blinky "brain" that blinky next moves using A* path finding algorithm.
 */

#include <GL/glut.h>
#include <../extensions/embed/embed.h>

#include <stdio.h>
#include <sys/time.h>
#include <assert.h>

// texturing
GLuint loadTexture(const char* filename, int *width, int *height);

GLuint background, point, pacman;
GLuint blinky;

// pacman position
unsigned mainx = 1, mainy = 1;

// timers
struct timeval timestamp = {0, 0}; // for fps
struct timeval blinkytimestamp = {0, 0}; // for fps
unsigned frames = 0;
// unsigned ticks = 0; // todo: up every 100 ms


// olvm:
ol_t ol;

// just code simplification, some kind of magic to not manually write 'new_string' and 'make_integer':
// we automatically will call new_string and make_integer depends on argument type
// but in general case you should do this manually. or not.
#define _Q(x) \
	__builtin_choose_expr( __builtin_types_compatible_p (typeof(x), char[]), \
		new_string(&ol, (char*)(uintptr_t)x), \
	__builtin_choose_expr( __builtin_types_compatible_p (typeof(x), char*), \
		new_string(&ol, (char*)(uintptr_t)x), \
	__builtin_choose_expr( __builtin_types_compatible_p (typeof(x), int), \
		make_integer((int)(uintptr_t)x), \
	__builtin_choose_expr( __builtin_types_compatible_p (typeof(x), unsigned), \
		make_integer((int)(uintptr_t)x), \
	__builtin_choose_expr( __builtin_types_compatible_p (typeof(x), long), \
		make_integer((int)(uintptr_t)x), \
	/*else*/ \
		IFALSE)))))

#define eval(...) embed_eval(&ol, MAP_LIST(_Q, __VA_ARGS__), 0)


//Drawing funciton
void drawBackground()
{
	glBindTexture(GL_TEXTURE_2D, background);
	glBegin(GL_QUADS);
	glTexCoord2d(1, 1); glVertex2f( 0,  0);
	glTexCoord2d(1, 0); glVertex2f( 0, 36);
	glTexCoord2d(0, 0); glVertex2f(28, 36);
	glTexCoord2d(0, 1); glVertex2f(28,  0);
	glEnd();
}
void drawPoint(int x, int y)
{
	y += 3;
	glBegin(GL_QUADS);
	glTexCoord2d(1, 1); glVertex2f(x, y);
	glTexCoord2d(1, 0); glVertex2f(x, y+1);
	glTexCoord2d(0, 0); glVertex2f(x+1, y+1);
	glTexCoord2d(0, 1); glVertex2f(x+1, y);
	glEnd();
}

void draw(void)
{
	frames++;

	// Background color
	glClearColor(0,1,0,1);
	glClear(GL_COLOR_BUFFER_BIT);

	// Draw order
	glEnable(GL_TEXTURE_2D);
	drawBackground();

	glBindTexture(GL_TEXTURE_2D, point);
	word points = eval("points");                                assert(is_reference(points));
	for (int y = 0; y < 31; y++) {
		uintptr_t line = ref(points, y);
		for (int x = 0; x < 28; x++) {
			if (ol2int(ref(line, x)) == 1)
				drawPoint(x, y);
		}
	}

	glBindTexture(GL_TEXTURE_2D, pacman);
	drawPoint(mainx, mainy);

	glBindTexture(GL_TEXTURE_2D, blinky);
	{
		word ps = eval("blinky");                                assert(is_pair(ps));
		int x = ol2int(car(ps));
		int y = ol2int(cdr(ps)) + 3;
		float emoji = rand() % 4 / 4.0;
		glBegin(GL_QUADS);
		glTexCoord2d(1, emoji + 0.25); glVertex2f(x, y);
		glTexCoord2d(1, emoji);        glVertex2f(x, y+1);
		glTexCoord2d(0, emoji);        glVertex2f(x+1, y+1);
		glTexCoord2d(0, emoji + 0.25); glVertex2f(x+1, y);
		glEnd();
	}

	// show fps
	struct timeval now;
	gettimeofday(&now, NULL);

	if (now.tv_sec != timestamp.tv_sec) {
		printf("fps: %d\n", frames);
		timestamp = now;
		frames = 0;
	}

	// Done
	glutSwapBuffers();
}

void init(void)
{
	glLoadIdentity();
	glScalef(2, -2, 1);
	glTranslatef(-0.5, -0.5, 0);
	glScalef(1/28.0, 1/36.0, 1);

	glEnable(GL_TEXTURE_2D);
	background = loadTexture("resources/background.png", 0, 0);
	point =      loadTexture("resources/point.png", 0, 0);
	pacman =     loadTexture("resources/pacman.png", 0, 0);
	blinky =     loadTexture("resources/blinky.png", 0, 0);

	// initial vm communication
	eval("(import (main))");
	eval("eat-the-point", mainx, mainy);
}

void idle() {
	// think here
	struct timeval now;
	gettimeofday(&now, NULL);

	int sec = now.tv_sec - blinkytimestamp.tv_sec;
	int usec = now.tv_usec - blinkytimestamp.tv_usec;
	if (usec + sec * 1000000 > 1000000/3) { // 3 times per second
		blinkytimestamp = now;
		eval("blinky-move", mainx, mainy);                    //check_error();
	}

	glutPostRedisplay();   // Post a re-paint request to activate display()
}

void keys(int key, int x, int y) {
	int dx = 0, dy = 0;
	switch (key) {
	case GLUT_KEY_UP:
		dy = -1;
		break;
	case GLUT_KEY_DOWN:
		dy = +1;
		break;
	case GLUT_KEY_LEFT:
		dx = -1;
		break;
	case GLUT_KEY_RIGHT:
		dx = +1;
		break;
	default:
		return;
	}

	word p = eval("get-level", mainx+dx, mainy+dy);      assert(is_small(p));
	if (ol2small(p) == 1) {
		mainx += dx;
		mainy += dy;
		eval("eat-the-point", mainx, mainy);
	}
	glutPostRedisplay();
}


//Main program
int main(int argc, char **argv)
{
	embed_new(&ol); // ol creation
	eval("print", "do some logs: ", 42);

#if 1 // eclipse
	chdir("samples/pacman");
#endif

	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_RGB|GLUT_DOUBLE);
	glutInitWindowPosition((glutGet(GLUT_SCREEN_WIDTH) - 224*2) / 2,
	                       (glutGet(GLUT_SCREEN_HEIGHT) - 288*2) / 2);
	glutInitWindowSize(224*2, 288*2);
	glutCreateWindow("Ol'ed Pacman");

	init();

	glutDisplayFunc(draw);
	glutIdleFunc(idle);
	glutSpecialFunc(keys);

	// init timers
	gettimeofday(&timestamp, NULL);
	gettimeofday(&blinkytimestamp, NULL);

	// let's go
	glutMainLoop();

	//embed_delete(olvm);
	return 0;
}
