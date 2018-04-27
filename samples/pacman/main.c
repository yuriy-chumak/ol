#include <GL/glut.h>
#include <../../extensions/talkback/talkback.h>

#include <stdio.h>
#include <sys/time.h>
#include <assert.h>

GLuint loadTexture(const char* filename, int *width, int *height);

GLuint background, point, pacman;
GLuint blinky;

unsigned mainx = 1, mainy = 1;

struct timeval timestamp = {0, 0};
unsigned frames = 0;


// talkback:
void* brain;
// just simplification
#define eval(format, ...) OL_tb_eval(brain, format, ##__VA_ARGS__)
#define send(format, ...) OL_tb_send(brain, format, ##__VA_ARGS__)
void check_error()
{
	void* error = OL_tb_error(brain);
	if (error && is_pair(error)) { // assume that pair is head of proper list
		printf("got an error: ");
		while (error != INULL) {
			uintptr_t part = car(error);
			if (is_string(part))
				printf("%.*s ", string_length(part), string_value(part));
			error = cdr(error);
		}
		fflush(stdout);
		exit(1);
	}
}



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
	if (timestamp.tv_sec == 0)
		gettimeofday(&timestamp, NULL);
	frames++;

	// Background color
	glClearColor(0,1,0,1);
	glClear(GL_COLOR_BUFFER_BIT);

	// Draw order
	glEnable(GL_TEXTURE_2D);
	drawBackground();

	glBindTexture(GL_TEXTURE_2D, point);
	void* points = eval("points");                                check_error();
	for (int y = 0; y < 31; y++) {
		// points is an object vector, so should start from 1
		uintptr_t line = ref(points, y+1);
		for (int x = 0; x < 28; x++) {
			if (ol2small(ref(line, x+1)) == 1)
				drawPoint(x, y);
		}
	}

	glBindTexture(GL_TEXTURE_2D, pacman);
	drawPoint(mainx, mainy);

	glBindTexture(GL_TEXTURE_2D, blinky);
	{
		void* ps = eval("blinky");                                check_error();
		int x = ol2small(car(ps));
		int y = ol2small(cdr(ps)) + 3;
		float emoji = rand() % 4 / 4.0;
		glBegin(GL_QUADS);
		glTexCoord2d(1, emoji + 0.25); glVertex2f(x, y);
		glTexCoord2d(1, emoji);        glVertex2f(x, y+1);
		glTexCoord2d(0, emoji);        glVertex2f(x+1, y+1);
		glTexCoord2d(0, emoji + 0.25); glVertex2f(x+1, y);
		glEnd();
	}

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
	point = loadTexture("resources/point.png", 0, 0);
	pacman = loadTexture("resources/pacman.png", 0, 0);
	blinky = loadTexture("resources/blinky.png", 0, 0);

	send(",load \"%s\"\n", "main.lisp");
	send("(eat-the-point %d %d)", mainx, mainy);
}

void idle() {
	// think here
	eval("(blinky-move %d %d)", mainx, mainy);                    check_error();

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

	void* p = eval("(get-level %d %d)", mainx+dx, mainy+dy);      check_error();
	if (is_small(p)) {
		if (ol2small(p) == 1) {
			mainx += dx;
			mainy += dy;
			eval("(eat-the-point %d %d)", mainx, mainy);          check_error();
		}
	}
	glutPostRedisplay();
}


//Main program
int main(int argc, char **argv)
{
	brain = OL_tb_start();

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
	glutMainLoop();

	OL_tb_stop(brain);
	return 0;
}
