#include <stdint.h>

__attribute__((visibility("default")))
int vrapi_Initialize(void * initParms)
{
	return -1;
}

__attribute__((visibility("default")))
int vrapi_GetSystemPropertyInt( void * java, int propType )
{
	return 0;
}

void * vrapi_CreateTextureSwapChain3( int type, int64_t format, int width, int height, int levels, int bufferCount )
{
	return 0;
}

__attribute__((visibility("default")))
int vrapi_GetTextureSwapChainLength( void * chain )
{
	return 0;
}

__attribute__((visibility("default")))
unsigned int vrapi_GetTextureSwapChainHandle( void * chain, int index )
{
	return 0;
}

__attribute__((visibility("default")))
void * vrapi_EnterVrMode( void * parms )
{
	return 0;
}

__attribute__((visibility("default")))
void vrapi_LeaveVrMode( void * ovr )
{
	(void) 0;
}

__attribute__((visibility("default")))
double vrapi_GetPredictedDisplayTime( void * ovr, long long frameIndex )
{
	return 0;
}

__attribute__((visibility("default")))
void vrapi_Shutdown()
{
	(void) 0;
}

__attribute__((visibility("default")))
int vrapi_GetPredictedTracking2( void * ovr, double absTimeInSeconds )
{
	return 0;
}

__attribute__((visibility("default")))
int vrapi_SubmitFrame2( void * ovr, void * frameDescription )
{
	return -1;
}
