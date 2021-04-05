/*
 * This sample demonstrates embedding the Otus Lisp into C projects.
 *
 * We draw the simple movable pacman (using C for rendering and pacman move),
 * and Otus Lisp as blinky "brain" that blinky next moves using A* path finding algorithm.
 */

#include <GL/glut.h>

#include <ol/ol.h>
#include <stdio.h>

#include <sys/time.h>
#include <assert.h>

// texturing
GLuint loadTexture(const char* filename, int *width, int *height);

GLuint background, point, pacman, gameover, winner;
GLuint blinky;

// pacman position
unsigned mainx = 1, mainy = 1;
unsigned finita = 0, youwin = 0;

// timers
struct timeval timestamp = {0, 0}; // for fps
struct timeval blinkytimestamp = {0, 0}; // for fps
unsigned frames = 0;
// unsigned ticks = 0; // todo: up every 100 ms
unsigned rotation = 0;

// our lisp logic:
void ol_new_ol();
void ol_delete_ol();

uintptr_t ol_points();
void ol_get_blinky(int* x, int* y);
size_t ol_get_heap_memory();
size_t ol_get_used_memory();
void ol_eat_the_point(int x, int y);
void ol_blinky_move(int x, int y);
int ol_get_level(int x, int y);


// Drawing functions
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
void drawPacman(int x, int y)
{
	static double coords[] = { 1, 1, 1, 0, 0, 0, 0, 1 };
	y += 3;
	int r = rotation * 2; int m = sizeof(coords) / sizeof(coords[0]);
	glBegin(GL_QUADS);
	glTexCoord2dv(&coords[r]); glVertex2f(x, y);   r = (r + 2) % m;
	glTexCoord2dv(&coords[r]); glVertex2f(x, y+1);   r = (r + 2) % m;
	glTexCoord2dv(&coords[r]); glVertex2f(x+1, y+1);   r = (r + 2) % m;
	glTexCoord2dv(&coords[r]); glVertex2f(x+1, y);   r = (r + 2) % m;
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
	uintptr_t points = ol_points();
	assert(is_reference(points));
	int points_left = 0;
	for (int y = 0; y < 31; y++) {
		uintptr_t line = ref(points, y+1);
		for (int x = 0; x < 28; x++) {
			if (ol2int(ref(line, x+1)) == 1) {
				points_left++;
				drawPoint(x, y);
			}
		}
	}
	if (!points_left)
		youwin = 1;


	glBindTexture(GL_TEXTURE_2D, pacman);
	drawPacman(mainx, mainy);

	glBindTexture(GL_TEXTURE_2D, blinky);
	{
		int x, y;
		ol_get_blinky(&x, &y);
		float emoji = rand() % 4 / 4.0;

		y += 3;
		glBegin(GL_QUADS);
		glTexCoord2d(0, emoji + 0.25); glVertex2f(x, y);
		glTexCoord2d(0, emoji);        glVertex2f(x, y+1);
		glTexCoord2d(1, emoji);        glVertex2f(x+1, y+1);
		glTexCoord2d(1, emoji + 0.25); glVertex2f(x+1, y);
		glEnd();
	}

	// show fps
	struct timeval now;
	gettimeofday(&now, NULL);

	if (now.tv_sec != timestamp.tv_sec) {
		size_t heap = ol_get_heap_memory();
		size_t used = ol_get_used_memory();
		printf("fps: %d, used %ld bytes of %ld allocated heap (%ld%%)\n",
			frames,
			used * sizeof(uintptr_t),
			heap * sizeof(uintptr_t),
			(used * 100) / heap);
		timestamp = now;
		frames = 0;
	}

	if (youwin) {
		glBindTexture(GL_TEXTURE_2D, winner);
	}
	else
	if (finita) {
		glBindTexture(GL_TEXTURE_2D, gameover);
	}
	if (youwin || finita) {
		glBegin(GL_QUADS);
		glTexCoord2d(0, 1); glVertex2f( 6,  9);
		glTexCoord2d(0, 0); glVertex2f( 6, 27);
		glTexCoord2d(1, 0); glVertex2f(22, 27);
		glTexCoord2d(1, 1); glVertex2f(22,  9);
		glEnd();
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
	gameover =   loadTexture("resources/gameover.png", 0, 0);
	winner =     loadTexture("resources/winner.png", 0, 0);

	// initial vm communication
	ol_eat_the_point(mainx, mainy);
}

void idle() {
	// think here
	struct timeval now;
	gettimeofday(&now, NULL);

	int sec = now.tv_sec - blinkytimestamp.tv_sec;
	int usec = now.tv_usec - blinkytimestamp.tv_usec;
	if (usec + sec * 1000000 > 1000000/3) { // 3 times per second
		blinkytimestamp = now;
		if (!finita && !youwin)
			ol_blinky_move(mainx, mainy);                    //check_error();
	}

	int x, y;
	ol_get_blinky(&x, &y);
	if (x == mainx && y == mainy) {
		finita = 1;
	}

	glutPostRedisplay();   // Post a re-paint request to activate display()
}

void keys(int key, int x, int y) {
	int dx = 0, dy = 0;
	switch (key) {
	case GLUT_KEY_UP:
		dy = -1; rotation = 3;
		break;
	case GLUT_KEY_DOWN:
		dy = +1; rotation = 1;
		break;
	case GLUT_KEY_LEFT:
		dx = -1; rotation = 2;
		break;
	case GLUT_KEY_RIGHT:
		dx = +1; rotation = 0;
		break;
	default:
		return;
	}

	if (!finita && !youwin) {
		int p = ol_get_level(mainx+dx, mainy+dy);
		if (p == 1) {
			mainx += dx;
			mainy += dy;
			ol_eat_the_point(mainx, mainy);
		}
	}
	glutPostRedisplay();
}

//Main program
int main(int argc, char **argv)
{
	ol_new_ol();

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

	ol_delete_ol();
	return 0;
}
