using System;
using System.Runtime.InteropServices;
using System.Text;

using iaaa.framework.math;

//	Базовая система координат OpenGL: http://www.intuit.ru/department/se/prcsharp/21/
// Правая. x-направо, y-вверх, z-к себе
// В исходном состоянии OpenGL камера находится в начале мировых координат, смотрит в
// отрицательную сторону оси z, направляющий вектор камеры (нормаль) совпадает с осью
// y (камера стоит на плоскости x0z).
//	GL_MODELVIEW: Модельные преобразования (модельно-видовая матрица) - применяются к размещению объектов на сцене.
//	GL_PROJECTION: Видовые преобразования (проекционная матрица) - применяются к размещению и ориентации точки обзора (настройка камеры).
//	GL_TEXTURE: Текстурные преобразования (текстурная матрица) - применяются для управления текстурами заполнения объектов. (?)

// todo: завести глобальную переменную "Use_Materials" для рендеринга сцены с материалами и без оных (например, что бы прозрачность забацать)
//	url: http://www.uraldev.ru/articles/id/36
//	url: http://www.uraldev.ru/articles/id/40

// todo: разделить "по умному" OpenGL.Invoke и OpenGL.Enqueue
// function inlining: http://blogs.msdn.com/b/vancem/archive/2008/08/19/to-inline-or-not-to-inline-that-is-the-question.aspx
// Про маршалинг: http://www.mindon.net/articles/gchandle.html
namespace iaaa.framework.opengl
{
	// Эти объявления не экспортируются!!
	#region OPENGL_1_0
	using GLenum		= System.UInt32;	// unsigned int
	using GLboolean		= System.Byte;		// unsigned char
	using GLbitfield	= System.UInt32;	// unsigned int
	using GLbyte		= System.SByte;		// signed char
	using GLshort		= System.Int16;		// short
	using GLint			= System.Int32;		// int
	using GLsizei		= System.Int32;		// int
	using GLubyte		= System.Byte;		// unsigned char
	using GLushort		= System.UInt16;	// unsigned short
	using GLuint		= System.UInt32;	// unsigned int
	using GLfloat		= System.Single;	// float
	using GLclampf		= System.Single;	// float
	using GLdouble		= System.Double;	// double
	using GLclampd		= System.Double;	// double
//	using GLvoid		= System.Object;	// void /ну не позволяет C# поюзать System.Void! :(/
	#endregion

	#region GL_VERSION_1_5
	using GLintptr		= System.IntPtr;	// ptrdiff_t
	using GLsizeiptr	= System.IntPtr;	// ptrdiff_t
	#endregion

	#region GL_VERSION_2_0
	using GLchar		= System.Byte;		// char
	#endregion

	// GL_ARB_shader_objects
	using GLcharARB		= System.Byte;		// char
	using GLstringARB	= System.String;	// заменитель GLcharARB*, пришлось! (todo: проверить)
	using GLhandleARB	= System.UInt32;	// unsigned int

	// GL_ARB_vertex_buffer_object
	//	typedef ptrdiff_t GLintptrARB;
	//	typedef ptrdiff_t GLsizeiptrARB;
	// GL_ARB_half_float_pixel
	//	typedef unsigned short GLhalfARB;
	// GL_NV_half_float
	//	typedef unsigned short GLhalfNV;
	// ??? GLEXT_64_TYPES_DEFINED ???
	// GL_EXT_timer_query
	//	typedef int64_t GLint64EXT;
	//	typedef uint64_t GLuint64EXT;
	// GL_ARB_sync
	//	typedef int64_t GLint64;
	//	typedef uint64_t GLuint64;
	//	typedef struct __GLsync *GLsync;
	// GL_ARB_cl_event
	//	struct _cl_context;
	//	struct _cl_event;
	// GL_ARB_debug_output
	//	typedef void (APIENTRY *GLDEBUGPROCARB)(GLenum source,GLenum type,GLuint id,GLenum severity,GLsizei length,const GLchar *message,void *userParam);
	// GL_AMD_debug_output
	//	typedef void (APIENTRY *GLDEBUGPROCAMD)(GLuint id,GLenum category,GLenum severity,GLsizei length,const GLchar *message,void *userParam);
	// GL_NV_vdpau_interop
	//	typedef GLintptr GLvdpauSurfaceNV;

	[AttributeUsage(AttributeTargets.Delegate | AttributeTargets.Field | AttributeTargets.Class)]
	class OpenGLExtension : Attribute
	{
		public string Extension;
		public string EntryPoint = null;

		public OpenGLExtension(string extension)
		{
			this.Extension = extension;
		}

		public bool Equals(String extension)
		{
			return Extension.Equals(extension);
		}
	}

	/// <summary>
	/// Настройки
	/// </summary>
	public partial class OpenGL
	{
		public static class Config
		{
			public class VersionInfo
			{
				public uint Major = 0;
				public uint Minor = 0;

				public VersionInfo(Double mm) : this(mm.ToString(System.Globalization.CultureInfo.InvariantCulture)) { }
				public VersionInfo(String mm)
				{
					String[] ss = mm.Split('.');
					Major = UInt32.Parse(ss[0]);
					if (ss.Length > 1)
						Minor = UInt32.Parse(ss[1]);
				}
				public static implicit operator VersionInfo(Double dd)
				{
					return new VersionInfo(dd);
				}
				public static implicit operator VersionInfo(String ss)
				{
					return new VersionInfo(ss);
				}


				public static bool operator >=(VersionInfo a, VersionInfo b)
				{
					return (a.Major > b.Major) || ((a.Major == b.Major) && (a.Minor >= b.Minor));
				}
				public static bool operator <=(VersionInfo a, VersionInfo b)
				{
					return (a.Major < b.Major) || ((a.Major == b.Major) && (a.Minor <= b.Minor));
				}
				public static bool operator ==(VersionInfo a, VersionInfo b)
				{
					if (System.Object.ReferenceEquals(a, b))
						return true;
					if ((object)a == null || (object)b == null)
						return false;
					return (a.Major == b.Major) && (a.Minor == b.Minor);
				}
				public static bool operator !=(VersionInfo a, VersionInfo b)
				{
					if (System.Object.ReferenceEquals(a, b))
						return false;
					if ((object)a == null || (object)b == null)
						return false;
					return (a.Major != b.Major) || (a.Minor != b.Minor);
				}
			}

			public static VersionInfo Version = null;
			public static String[] Extensions = null;
		}
	}

	/// <summary>
	/// Legacy OpenGL Code
	/// </summary>
	public partial class OpenGL	// LEGACY! MUST BE MOVED TO ATTRIBUTED CLASSES!
	{
		#region Defines from gl.h
		// Pixel types
		public const byte
			PFD_TYPE_RGBA = 0,
			PFD_TYPE_COLORINDEX = 1;

		// Layer types
		public const sbyte
			PFD_MAIN_PLANE = 0,
			PFD_OVERLAY_PLANE = 1,
			PFD_UNDERLAY_PLANE = -1;

		// PIXELFORMATDESCRIPTOR flags
		public const System.UInt32
			PFD_DOUBLEBUFFER = 0x00000001,
			PFD_STEREO = 0x00000002,
			PFD_DRAW_TO_WINDOW = 0x00000004,
			PFD_DRAW_TO_BITMAP = 0x00000008,
			PFD_SUPPORT_GDI = 0x00000010,
			PFD_SUPPORT_OPENGL = 0x00000020,
			PFD_GENERIC_FORMAT = 0x00000040,
			PFD_NEED_PALETTE = 0x00000080,
			PFD_NEED_SYSTEM_PALETTE = 0x00000100,
			PFD_SWAP_EXCHANGE = 0x00000200,
			PFD_SWAP_COPY = 0x00000400,
			PFD_SWAP_LAYER_BUFFERS = 0x00000800,
			PFD_GENERIC_ACCELERATED = 0x00001000,
			PFD_SUPPORT_DIRECTDRAW = 0x00002000;

		/* PIXELFORMATDESCRIPTOR flags for use in ChoosePixelFormat only */
		public const System.UInt32
			PFD_DEPTH_DONTCARE = 0x20000000,
			PFD_DOUBLEBUFFER_DONTCARE = 0x40000000,
			PFD_STEREO_DONTCARE = 0x80000000;

		/* AccumOp */
		public const System.UInt32
			GL_ACCUM = 0x0100,
			GL_LOAD = 0x0101,
			GL_RETURN = 0x0102,
			GL_MULT = 0x0103,
			GL_ADD = 0x0104;

		/* AlphaFunction */
		public const System.UInt32
			GL_NEVER = 0x0200,
			GL_LESS = 0x0201,
			GL_EQUAL = 0x0202,
			GL_LEQUAL = 0x0203,
			GL_GREATER = 0x0204,
			GL_NOTEQUAL = 0x0205,
			GL_GEQUAL = 0x0206,
			GL_ALWAY = 0x0207;

		/* AttribMask */
		public const System.UInt32
			GL_CURRENT_BIT = 0x00000001,
			GL_POINT_BIT = 0x00000002,
			GL_LINE_BIT = 0x00000004,
			GL_POLYGON_BIT = 0x00000008,
			GL_POLYGON_STIPPLE_BIT = 0x00000010,
			GL_PIXEL_MODE_BIT = 0x00000020,
			GL_LIGHTING_BIT = 0x00000040,
			GL_FOG_BIT = 0x00000080,
			GL_DEPTH_BUFFER_BIT = 0x00000100,
			GL_ACCUM_BUFFER_BIT = 0x00000200,
			GL_STENCIL_BUFFER_BIT = 0x00000400,
			GL_VIEWPORT_BIT = 0x00000800,
			GL_TRANSFORM_BIT = 0x00001000,
			GL_ENABLE_BIT = 0x00002000,
			GL_COLOR_BUFFER_BIT = 0x00004000,
			GL_HINT_BIT = 0x00008000,
			GL_EVAL_BIT = 0x00010000,
			GL_LIST_BIT = 0x00020000,
			GL_TEXTURE_BIT = 0x00040000,
			GL_SCISSOR_BIT = 0x00080000,
			GL_ALL_ATTRIB_BITS = 0x000fffff;


		/* BlendingFactorDest */
		public const System.UInt32
			GL_ZERO = 0,
			GL_ONE = 1,
			GL_SRC_COLOR = 0x0300,
			GL_ONE_MINUS_SRC_COLOR = 0x0301,
			GL_SRC_ALPHA = 0x0302,
			GL_ONE_MINUS_SRC_ALPHA = 0x0303,
			GL_DST_ALPHA = 0x0304,
			GL_ONE_MINUS_DST_ALPHA = 0x0305;

		/* BlendingFactorSrc */
		public const System.UInt32
			GL_DST_COLOR = 0x0306,
			GL_ONE_MINUS_DST_COLOR = 0x0307,
			GL_SRC_ALPHA_SATURATE = 0x0308;

		/* Boolean */
		public const System.UInt32
			GL_TRUE = 1,
			GL_FALSE = 0;

		/* ClipPlaneName */
		public const System.UInt32
			GL_CLIP_PLANE0 = 0x3000,
			GL_CLIP_PLANE1 = 0x3001,
			GL_CLIP_PLANE2 = 0x3002,
			GL_CLIP_PLANE3 = 0x3003,
			GL_CLIP_PLANE4 = 0x3004,
			GL_CLIP_PLANE5 = 0x3005;

		/* DataType */
		public const System.UInt32
			GL_BYTE = 0x1400,
			GL_UNSIGNED_BYTE = 0x1401,
			GL_SHORT = 0x1402,
			GL_UNSIGNED_SHORT = 0x1403,
			GL_INT = 0x1404,
			GL_UNSIGNED_INT = 0x1405,
			GL_FLOAT = 0x1406,
			GL_2_BYTES = 0x1407,
			GL_3_BYTES = 0x1408,
			GL_4_BYTES = 0x1409,
			GL_DOUBLE = 0x140A;

		/* DrawBufferMode */
		public const System.UInt32
			GL_NONE = 0,
			GL_FRONT_LEFT = 0x0400,
			GL_FRONT_RIGHT = 0x0401,
			GL_BACK_LEFT = 0x0402,
			GL_BACK_RIGHT = 0x0403,
			GL_FRONT = 0x0404,
			GL_BACK = 0x0405,
			GL_LEFT = 0x0406,
			GL_RIGHT = 0x0407,
			GL_FRONT_AND_BACK = 0x0408,
			GL_AUX0 = 0x0409,
			GL_AUX1 = 0x040A,
			GL_AUX2 = 0x040B,
			GL_AUX3 = 0x040C;

		/* FeedBackMode */
		public const System.UInt32
			GL_2D = 0x0600,
			GL_3D = 0x0601,
			GL_3D_COLOR = 0x0602,
			GL_3D_COLOR_TEXTURE = 0x0603,
			GL_4D_COLOR_TEXTURE = 0x0604;

		/* FeedBackToken */
		public const System.UInt32
			GL_PASS_THROUGH_TOKEN = 0x0700,
			GL_POINT_TOKEN = 0x0701,
			GL_LINE_TOKEN = 0x0702,
			GL_POLYGON_TOKEN = 0x0703,
			GL_BITMAP_TOKEN = 0x0704,
			GL_DRAW_PIXEL_TOKEN = 0x0705,
			GL_COPY_PIXEL_TOKEN = 0x0706,
			GL_LINE_RESET_TOKEN = 0x0707;

		/* FogMode */
		public const System.Int32
			GL_EXP = 0x0800,
			GL_EXP2 = 0x0801;
		/* FrontFaceDirection */
		public const System.UInt32
			GL_CW = 0x0900,
			GL_CCW = 0x0901;

		/* GetMapTarget */
		public const System.UInt32
			GL_COEFF = 0x0A00,
			GL_ORDER = 0x0A01,
			GL_DOMAIN = 0x0A02;

		/* GetTarget */
		public const System.UInt32
			GL_CURRENT_INDEX = 0x0B01,
			GL_CURRENT_NORMAL = 0x0B02,
			GL_CURRENT_TEXTURE_COORDS = 0x0B03,
			GL_CURRENT_RASTER_COLOR = 0x0B04,
			GL_CURRENT_RASTER_INDEX = 0x0B05,
			GL_CURRENT_RASTER_TEXTURE_COORDS = 0x0B06,
			GL_CURRENT_RASTER_POSITION = 0x0B07,
			GL_CURRENT_RASTER_POSITION_VALID = 0x0B08,
			GL_CURRENT_RASTER_DISTANCE = 0x0B09,
			GL_POINT_SMOOTH = 0x0B10,
			GL_POINT_SIZE = 0x0B11,
			GL_POINT_SIZE_RANGE = 0x0B12,
			GL_POINT_SIZE_GRANULARITY = 0x0B13,
			GL_LINE_SMOOTH = 0x0B20,
			GL_LINE_WIDTH = 0x0B21,
			GL_LINE_WIDTH_RANGE = 0x0B22,
			GL_LINE_WIDTH_GRANULARITY = 0x0B23,
			GL_LINE_STIPPLE = 0x0B24,
			GL_LINE_STIPPLE_PATTERN = 0x0B25,
			GL_LINE_STIPPLE_REPEAT = 0x0B26,
			GL_LIST_MODE = 0x0B30,
			GL_MAX_LIST_NESTING = 0x0B31,
			GL_LIST_BASE = 0x0B32,
			GL_LIST_INDEX = 0x0B33,
			GL_POLYGON_MODE = 0x0B40,
			GL_POLYGON_SMOOTH = 0x0B41,
			GL_POLYGON_STIPPLE = 0x0B42,
			GL_EDGE_FLAG = 0x0B43,
			GL_CULL_FACE = 0x0B44,
			GL_CULL_FACE_MODE = 0x0B45,
			GL_FRONT_FACE = 0x0B46,
			GL_LIGHTING = 0x0B50,
			GL_LIGHT_MODEL_LOCAL_VIEWER = 0x0B51,
			GL_LIGHT_MODEL_TWO_SIDE = 0x0B52,
			GL_LIGHT_MODEL_AMBIENT = 0x0B53,
			GL_SHADE_MODEL = 0x0B54,
			GL_COLOR_MATERIAL_FACE = 0x0B55,
			GL_COLOR_MATERIAL_PARAMETER = 0x0B56,
			GL_COLOR_MATERIAL = 0x0B57,
			GL_FOG = 0x0B60,
			GL_FOG_INDEX = 0x0B61,
			GL_FOG_DENSITY = 0x0B62,
			GL_FOG_START = 0x0B63,
			GL_FOG_END = 0x0B64,
			GL_FOG_MODE = 0x0B65,
			GL_FOG_COLOR = 0x0B66,
			GL_DEPTH_RANGE = 0x0B70,
			GL_DEPTH_TEST = 0x0B71,
			GL_DEPTH_WRITEMASK = 0x0B72,
			GL_DEPTH_CLEAR_VALUE = 0x0B73,
			GL_DEPTH_FUNC = 0x0B74,
			GL_ACCUM_CLEAR_VALUE = 0x0B80,
			GL_STENCIL_TEST = 0x0B90,
			GL_STENCIL_CLEAR_VALUE = 0x0B91,
			GL_STENCIL_FUNC = 0x0B92,
			GL_STENCIL_VALUE_MASK = 0x0B93,
			GL_STENCIL_FAIL = 0x0B94,
			GL_STENCIL_PASS_DEPTH_FAIL = 0x0B95,
			GL_STENCIL_PASS_DEPTH_PASS = 0x0B96,
			GL_STENCIL_REF = 0x0B97,
			GL_STENCIL_WRITEMASK = 0x0B98,
			GL_MATRIX_MODE = 0x0BA0,
			GL_NORMALIZE = 0x0BA1,
			GL_VIEWPORT = 0x0BA2,
			GL_MODELVIEW_STACK_DEPTH = 0x0BA3,
			GL_PROJECTION_STACK_DEPTH = 0x0BA4,
			GL_TEXTURE_STACK_DEPTH = 0x0BA5,
			GL_MODELVIEW_MATRIX = 0x0BA6,
			GL_PROJECTION_MATRIX = 0x0BA7,
			GL_TEXTURE_MATRIX = 0x0BA8,
			GL_ATTRIB_STACK_DEPTH = 0x0BB0,
			GL_CLIENT_ATTRIB_STACK_DEPTH = 0x0BB1,
			GL_ALPHA_TEST = 0x0BC0,
			GL_ALPHA_TEST_FUNC = 0x0BC1,
			GL_ALPHA_TEST_REF = 0x0BC2,
			GL_DITHER = 0x0BD0,
			GL_BLEND_DST = 0x0BE0,
			GL_BLEND_SRC = 0x0BE1,
			GL_BLEND = 0x0BE2,
			GL_LOGIC_OP_MODE = 0x0BF0,
			GL_INDEX_LOGIC_OP = 0x0BF1,
			GL_COLOR_LOGIC_OP = 0x0BF2,
			GL_AUX_BUFFERS = 0x0C00,
			GL_DRAW_BUFFER = 0x0C01,
			GL_READ_BUFFER = 0x0C02,
			GL_SCISSOR_BOX = 0x0C10,
			GL_SCISSOR_TEST = 0x0C11,
			GL_INDEX_CLEAR_VALUE = 0x0C20,
			GL_INDEX_WRITEMASK = 0x0C21,
			GL_COLOR_CLEAR_VALUE = 0x0C22,
			GL_COLOR_WRITEMASK = 0x0C23,
			GL_INDEX_MODE = 0x0C30,
			GL_RGBA_MODE = 0x0C31,
			GL_DOUBLEBUFFER = 0x0C32,
			GL_STEREO = 0x0C33,
			GL_RENDER_MODE = 0x0C40,
			GL_PERSPECTIVE_CORRECTION_HINT = 0x0C50,
			GL_POINT_SMOOTH_HINT = 0x0C51,
			GL_LINE_SMOOTH_HINT = 0x0C52,
			GL_POLYGON_SMOOTH_HINT = 0x0C53,
			GL_FOG_HINT = 0x0C54,
			GL_GENERATE_MIPMAP_HINT = 0x8192,
			GL_TEXTURE_GEN_S = 0x0C60,
			GL_TEXTURE_GEN_T = 0x0C61,
			GL_TEXTURE_GEN_R = 0x0C62,
			GL_TEXTURE_GEN_Q = 0x0C63,
			GL_PIXEL_MAP_I_TO_I = 0x0C70,
			GL_PIXEL_MAP_S_TO_S = 0x0C71,
			GL_PIXEL_MAP_I_TO_R = 0x0C72,
			GL_PIXEL_MAP_I_TO_G = 0x0C73,
			GL_PIXEL_MAP_I_TO_B = 0x0C74,
			GL_PIXEL_MAP_I_TO_A = 0x0C75,
			GL_PIXEL_MAP_R_TO_R = 0x0C76,
			GL_PIXEL_MAP_G_TO_G = 0x0C77,
			GL_PIXEL_MAP_B_TO_B = 0x0C78,
			GL_PIXEL_MAP_A_TO_A = 0x0C79,
			GL_PIXEL_MAP_I_TO_I_SIZE = 0x0CB0,
			GL_PIXEL_MAP_S_TO_S_SIZE = 0x0CB1,
			GL_PIXEL_MAP_I_TO_R_SIZE = 0x0CB2,
			GL_PIXEL_MAP_I_TO_G_SIZE = 0x0CB3,
			GL_PIXEL_MAP_I_TO_B_SIZE = 0x0CB4,
			GL_PIXEL_MAP_I_TO_A_SIZE = 0x0CB5,
			GL_PIXEL_MAP_R_TO_R_SIZE = 0x0CB6,
			GL_PIXEL_MAP_G_TO_G_SIZE = 0x0CB7,
			GL_PIXEL_MAP_B_TO_B_SIZE = 0x0CB8,
			GL_PIXEL_MAP_A_TO_A_SIZE = 0x0CB9,
			GL_UNPACK_SWAP_BYTES = 0x0CF0,
			GL_UNPACK_LSB_FIRST = 0x0CF1,
			GL_UNPACK_ROW_LENGTH = 0x0CF2,
			GL_UNPACK_SKIP_ROWS = 0x0CF3,
			GL_UNPACK_SKIP_PIXELS = 0x0CF4,
			GL_UNPACK_ALIGNMENT = 0x0CF5,
			GL_PACK_SWAP_BYTES = 0x0D00,
			GL_PACK_LSB_FIRST = 0x0D01,
			GL_PACK_ROW_LENGTH = 0x0D02,
			GL_PACK_SKIP_ROWS = 0x0D03,
			GL_PACK_SKIP_PIXELS = 0x0D04,
			GL_PACK_ALIGNMENT = 0x0D05,
			GL_MAP_COLOR = 0x0D10,
			GL_MAP_STENCIL = 0x0D11,
			GL_INDEX_SHIFT = 0x0D12,
			GL_INDEX_OFFSET = 0x0D13,
			GL_RED_SCALE = 0x0D14,
			GL_RED_BIAS = 0x0D15,
			GL_ZOOM_X = 0x0D16,
			GL_ZOOM_Y = 0x0D17,
			GL_GREEN_SCALE = 0x0D18,
			GL_GREEN_BIAS = 0x0D19,
			GL_BLUE_SCALE = 0x0D1A,
			GL_BLUE_BIAS = 0x0D1B,
			GL_ALPHA_SCALE = 0x0D1C,
			GL_ALPHA_BIAS = 0x0D1D,
			GL_DEPTH_SCALE = 0x0D1E,
			GL_DEPTH_BIAS = 0x0D1F,
			GL_MAX_EVAL_ORDER = 0x0D30,
			GL_MAX_LIGHTS = 0x0D31,
			GL_MAX_CLIP_PLANES = 0x0D32,
			GL_MAX_TEXTURE_SIZE = 0x0D33,
			GL_MAX_PIXEL_MAP_TABLE = 0x0D34,
			GL_MAX_ATTRIB_STACK_DEPTH = 0x0D35,
			GL_MAX_MODELVIEW_STACK_DEPTH = 0x0D36,
			GL_MAX_NAME_STACK_DEPTH = 0x0D37,
			GL_MAX_PROJECTION_STACK_DEPTH = 0x0D38,
			GL_MAX_TEXTURE_STACK_DEPTH = 0x0D39,
			GL_MAX_VIEWPORT_DIMS = 0x0D3A,
			GL_MAX_CLIENT_ATTRIB_STACK_DEPTH = 0x0D3B,
			GL_SUBPIXEL_BITS = 0x0D50,
			GL_INDEX_BITS = 0x0D51,
			GL_RED_BITS = 0x0D52,
			GL_GREEN_BITS = 0x0D53,
			GL_BLUE_BITS = 0x0D54,
			GL_ALPHA_BITS = 0x0D55,
			GL_DEPTH_BITS = 0x0D56,
			GL_STENCIL_BITS = 0x0D57,
			GL_ACCUM_RED_BITS = 0x0D58,
			GL_ACCUM_GREEN_BITS = 0x0D59,
			GL_ACCUM_BLUE_BITS = 0x0D5A,
			GL_ACCUM_ALPHA_BITS = 0x0D5B,
			GL_NAME_STACK_DEPTH = 0x0D70,
			GL_AUTO_NORMAL = 0x0D80,
			GL_MAP1_COLOR_4 = 0x0D90,
			GL_MAP1_INDEX = 0x0D91,
			GL_MAP1_NORMAL = 0x0D92,
			GL_MAP1_TEXTURE_COORD_1 = 0x0D93,
			GL_MAP1_TEXTURE_COORD_2 = 0x0D94,
			GL_MAP1_TEXTURE_COORD_3 = 0x0D95,
			GL_MAP1_TEXTURE_COORD_4 = 0x0D96,
			GL_MAP1_VERTEX_3 = 0x0D97,
			GL_MAP1_VERTEX_4 = 0x0D98,
			GL_MAP2_COLOR_4 = 0x0DB0,
			GL_MAP2_INDEX = 0x0DB1,
			GL_MAP2_NORMAL = 0x0DB2,
			GL_MAP2_TEXTURE_COORD_1 = 0x0DB3,
			GL_MAP2_TEXTURE_COORD_2 = 0x0DB4,
			GL_MAP2_TEXTURE_COORD_3 = 0x0DB5,
			GL_MAP2_TEXTURE_COORD_4 = 0x0DB6,
			GL_MAP2_VERTEX_3 = 0x0DB7,
			GL_MAP2_VERTEX_4 = 0x0DB8,
			GL_MAP1_GRID_DOMAIN = 0x0DD0,
			GL_MAP1_GRID_SEGMENTS = 0x0DD1,
			GL_MAP2_GRID_DOMAIN = 0x0DD2,
			GL_MAP2_GRID_SEGMENTS = 0x0DD3,
			GL_TEXTURE_1D = 0x0DE0,
			GL_TEXTURE_2D = 0x0DE1,
			GL_FEEDBACK_BUFFER_POINTER = 0x0DF0,
			GL_FEEDBACK_BUFFER_SIZE = 0x0DF1,
			GL_FEEDBACK_BUFFER_TYPE = 0x0DF2,
			GL_SELECTION_BUFFER_POINTER = 0x0DF3,
			GL_SELECTION_BUFFER_SIZE = 0x0DF4;

		/* GetTextureParameter */
		public const System.UInt32
			GL_TEXTURE_WIDTH = 0x1000,
			GL_TEXTURE_HEIGHT = 0x1001,
			GL_TEXTURE_INTERNAL_FORMAT = 0x1003,
			GL_TEXTURE_BORDER_COLOR = 0x1004,
			GL_TEXTURE_BORDER = 0x1005;

		/* LightName */
		public const System.UInt32
			GL_LIGHT0 = 0x4000,
			GL_LIGHT1 = 0x4001,
			GL_LIGHT2 = 0x4002,
			GL_LIGHT3 = 0x4003,
			GL_LIGHT4 = 0x4004,
			GL_LIGHT5 = 0x4005,
			GL_LIGHT6 = 0x4006,
			GL_LIGHT7 = 0x4007;

		/* LightParameter */
		public const System.UInt32
			GL_AMBIENT = 0x1200,
			GL_DIFFUSE = 0x1201,
			GL_SPECULAR = 0x1202,
			GL_POSITION = 0x1203,
			GL_SPOT_DIRECTION = 0x1204,
			GL_SPOT_EXPONENT = 0x1205,
			GL_SPOT_CUTOFF = 0x1206,
			GL_CONSTANT_ATTENUATION = 0x1207,
			GL_LINEAR_ATTENUATION = 0x1208,
			GL_QUADRATIC_ATTENUATION = 0x1209;

		/* ListMode */
		public const System.UInt32
			GL_COMPILE = 0x1300,
			GL_COMPILE_AND_EXECUTE = 0x1301;

		/* LogicOp */
		public const System.UInt32
			GL_CLEAR = 0x1500,
			GL_AND = 0x1501,
			GL_AND_REVERSE = 0x1502,
			GL_COPY = 0x1503,
			GL_AND_INVERTED = 0x1504,
			GL_NOOP = 0x1505,
			GL_XOR = 0x1506,
			GL_OR = 0x1507,
			GL_NOR = 0x1508,
			GL_EQUIV = 0x1509,
			GL_INVERT = 0x150A,
			GL_OR_REVERSE = 0x150B,
			GL_COPY_INVERTED = 0x150C,
			GL_OR_INVERTED = 0x150D,
			GL_NAND = 0x150E,
			GL_SET = 0x150F;

		/* MaterialParameter */
		public const System.UInt32
			GL_EMISSION = 0x1600,
			GL_SHININESS = 0x1601,
			GL_AMBIENT_AND_DIFFUSE = 0x1602,
			GL_COLOR_INDEXES = 0x1603;

		/* MatrixMode */
		public const System.UInt32
			GL_MODELVIEW = 0x1700,
			GL_PROJECTION = 0x1701,
			GL_TEXTURE = 0x1702;

		/* PixelCopyType */
		public const System.UInt32
			GL_COLOR = 0x1800,
			GL_DEPTH = 0x1801,
			GL_STENCIL = 0x1802;

		/* PixelFormat */
		public const System.UInt32
			GL_COLOR_INDEX = 0x1900,
			GL_STENCIL_INDEX = 0x1901,
			GL_DEPTH_COMPONENT = 0x1902,
			GL_RED = 0x1903,
			GL_GREEN = 0x1904,
			GL_BLUE = 0x1905,
			GL_ALPHA = 0x1906,
			GL_RGB = 0x1907,
			GL_RGBA = 0x1908,
			GL_LUMINANCE = 0x1909,
			GL_LUMINANCE_ALPHA = 0x190A;

		/* PixelType */
		public const System.UInt32
			GL_BITMAP = 0x1A00;

		/* PolygonMode */
		public const System.UInt32
			GL_POINT = 0x1B00,
			GL_LINE = 0x1B01,
			GL_FILL = 0x1B02;

		/* RenderingMode */
		public const System.UInt32
			GL_RENDER = 0x1C00,
			GL_FEEDBACK = 0x1C01,
			GL_SELECT = 0x1C02;

		/* ShadingModel */
		public const System.UInt32
			GL_FLAT = 0x1D00,
			GL_SMOOTH = 0x1D01;

		/* StencilOp */
		public const System.UInt32
			GL_KEEP = 0x1E00,
			GL_REPLACE = 0x1E01,
			GL_INCR = 0x1E02,
			GL_DECR = 0x1E03;

		/* StringName */
		public const System.UInt32
			GL_VENDOR = 0x1F00,
			GL_RENDERER = 0x1F01,
			GL_VERSION = 0x1F02,
			GL_EXTENSIONS = 0x1F03;

		/* TextureCoordName */
		public const System.UInt32
			GL_S = 0x2000,
			GL_T = 0x2001,
			GL_R = 0x2002,
			GL_Q = 0x2003;

		public const System.UInt32
			GL_MODULATE = 0x2100,
			GL_DECAL = 0x2101;

		/* TextureEnvParameter */
		public const System.UInt32
			GL_TEXTURE_ENV_MODE = 0x2200,
			GL_TEXTURE_ENV_COLOR = 0x2201;

		/* TextureEnvTarget */
		public const System.UInt32
			GL_TEXTURE_ENV = 0x2300;

		/* TextureGenMode */
		public const System.UInt32
			GL_EYE_LINEAR = 0x2400,
			GL_OBJECT_LINEAR = 0x2401,
			GL_SPHERE_MAP = 0x2402;

		/* TextureGenParameter */
		public const System.UInt32
			GL_TEXTURE_GEN_MODE = 0x2500,
			GL_OBJECT_PLANE = 0x2501,
			GL_EYE_PLANE = 0x2502;

		/* TextureMagFilter */
		public const System.Int32
			GL_NEAREST = 0x2600,
			GL_LINEAR = 0x2601;

		/* TextureMinFilter */
		public const System.UInt32
			GL_NEAREST_MIPMAP_NEAREST = 0x2700,
			GL_LINEAR_MIPMAP_NEAREST = 0x2701,
			GL_NEAREST_MIPMAP_LINEAR = 0x2702,
			GL_LINEAR_MIPMAP_LINEAR = 0x2703;

		/* TextureParameterName */
		public const System.UInt32
			GL_TEXTURE_MAG_FILTER = 0x2800,
			GL_TEXTURE_MIN_FILTER = 0x2801,
			GL_TEXTURE_WRAP_S = 0x2802,
			GL_TEXTURE_WRAP_T = 0x2803;

		/* TextureWrapMode */
		public const System.UInt32
			GL_CLAMP = 0x2900,
			GL_REPEAT = 0x2901;

		/* ClientAttribMask */
		public const System.UInt32
			GL_CLIENT_PIXEL_STORE_BIT = 0x00000001,
			GL_CLIENT_VERTEX_ARRAY_BIT = 0x00000002,
			GL_CLIENT_ALL_ATTRIB_BITS = 0xffffffff;

		/* polygon_offset */
		public const System.Int32
			GL_POLYGON_OFFSET_FACTOR = 0x8038,
			GL_POLYGON_OFFSET_UNITS = 0x2A00,
			GL_POLYGON_OFFSET_POINT = 0x2A01,
			GL_POLYGON_OFFSET_LINE = 0x2A02,
			GL_POLYGON_OFFSET_FILL = 0x8037;

		/* texture */
		public const System.Int32
			GL_ALPHA4 = 0x803B,
			GL_ALPHA8 = 0x803C,
			GL_ALPHA12 = 0x803D,
			GL_ALPHA16 = 0x803E,
			GL_LUMINANCE4 = 0x803F,
			GL_LUMINANCE8 = 0x8040,
			GL_LUMINANCE12 = 0x8041,
			GL_LUMINANCE16 = 0x8042,
			GL_LUMINANCE4_ALPHA4 = 0x8043,
			GL_LUMINANCE6_ALPHA2 = 0x8044,
			GL_LUMINANCE8_ALPHA8 = 0x8045,
			GL_LUMINANCE12_ALPHA4 = 0x8046,
			GL_LUMINANCE12_ALPHA12 = 0x8047,
			GL_LUMINANCE16_ALPHA16 = 0x8048,
			GL_INTENSITY = 0x8049,
			GL_INTENSITY4 = 0x804A,
			GL_INTENSITY8 = 0x804B,
			GL_INTENSITY12 = 0x804C,
			GL_INTENSITY16 = 0x804D,
			GL_R3_G3_B2 = 0x2A10,
			GL_RGB4 = 0x804F,
			GL_RGB5 = 0x8050,
			GL_RGB8 = 0x8051,
			GL_RGB10 = 0x8052,
			GL_RGB12 = 0x8053,
			GL_RGB16 = 0x8054,
			GL_RGBA2 = 0x8055,
			GL_RGBA4 = 0x8056,
			GL_RGB5_A1 = 0x8057,
			GL_RGBA8 = 0x8058,
			GL_RGB10_A2 = 0x8059,
			GL_RGBA12 = 0x805A,
			GL_RGBA16 = 0x805B,
			GL_TEXTURE_RED_SIZE = 0x805C,
			GL_TEXTURE_GREEN_SIZE = 0x805D,
			GL_TEXTURE_BLUE_SIZE = 0x805E,
			GL_TEXTURE_ALPHA_SIZE = 0x805F,
			GL_TEXTURE_LUMINANCE_SIZE = 0x8060,
			GL_TEXTURE_INTENSITY_SIZE = 0x8061,
			GL_PROXY_TEXTURE_1D = 0x8063,
			GL_PROXY_TEXTURE_2D = 0x8064;

		/* texture_object */
		public const System.Int32
			GL_TEXTURE_PRIORITY = 0x8066,
			GL_TEXTURE_RESIDENT = 0x8067,
			GL_TEXTURE_BINDING_1D = 0x8068,
			GL_TEXTURE_BINDING_2D = 0x8069;

		/* vertex_array */
		public const System.Int32
			GL_VERTEX_ARRAY = 0x8074,
			GL_NORMAL_ARRAY = 0x8075,
			GL_COLOR_ARRAY = 0x8076,
			GL_INDEX_ARRAY = 0x8077,
			GL_TEXTURE_COORD_ARRAY = 0x8078,
			GL_EDGE_FLAG_ARRAY = 0x8079,
			GL_VERTEX_ARRAY_SIZE = 0x807A,
			GL_VERTEX_ARRAY_TYPE = 0x807B,
			GL_VERTEX_ARRAY_STRIDE = 0x807C,
			GL_NORMAL_ARRAY_TYPE = 0x807E,
			GL_NORMAL_ARRAY_STRIDE = 0x807F,
			GL_COLOR_ARRAY_SIZE = 0x8081,
			GL_COLOR_ARRAY_TYPE = 0x8082,
			GL_COLOR_ARRAY_STRIDE = 0x8083,
			GL_INDEX_ARRAY_TYPE = 0x8085,
			GL_INDEX_ARRAY_STRIDE = 0x8086,
			GL_TEXTURE_COORD_ARRAY_SIZE = 0x8088,
			GL_TEXTURE_COORD_ARRAY_TYPE = 0x8089,
			GL_TEXTURE_COORD_ARRAY_STRIDE = 0x808A,
			GL_EDGE_FLAG_ARRAY_STRIDE = 0x808C,
			GL_VERTEX_ARRAY_POINTER = 0x808E,
			GL_NORMAL_ARRAY_POINTER = 0x808F,
			GL_COLOR_ARRAY_POINTER = 0x8090,
			GL_INDEX_ARRAY_POINTER = 0x8091,
			GL_TEXTURE_COORD_ARRAY_POINTER = 0x8092,
			GL_EDGE_FLAG_ARRAY_POINTER = 0x8093,
			GL_V2F = 0x2A20,
			GL_V3F = 0x2A21,
			GL_C4UB_V2F = 0x2A22,
			GL_C4UB_V3F = 0x2A23,
			GL_C3F_V3F = 0x2A24,
			GL_N3F_V3F = 0x2A25,
			GL_C4F_N3F_V3F = 0x2A26,
			GL_T2F_V3F = 0x2A27,
			GL_T4F_V4F = 0x2A28,
			GL_T2F_C4UB_V3F = 0x2A29,
			GL_T2F_C3F_V3F = 0x2A2A,
			GL_T2F_N3F_V3F = 0x2A2B,
			GL_T2F_C4F_N3F_V3F = 0x2A2C,
			GL_T4F_C4F_N3F_V4F = 0x2A2D;

		/* Extensions */
		public const byte
			GL_EXT_vertex_array = 1,
			GL_EXT_bgra = 1,
			GL_EXT_paletted_texture = 1,
			GL_WIN_swap_hint = 1,
			GL_WIN_draw_range_elements = 1;

		/* EXT_vertex_array */
		public const System.UInt32
			GL_VERTEX_ARRAY_EXT = 0x8074,
			GL_NORMAL_ARRAY_EXT = 0x8075,
			GL_COLOR_ARRAY_EXT = 0x8076,
			GL_INDEX_ARRAY_EXT = 0x8077,
			GL_TEXTURE_COORD_ARRAY_EXT = 0x8078,
			GL_EDGE_FLAG_ARRAY_EXT = 0x8079,
			GL_VERTEX_ARRAY_SIZE_EXT = 0x807A,
			GL_VERTEX_ARRAY_TYPE_EXT = 0x807B,
			GL_VERTEX_ARRAY_STRIDE_EXT = 0x807C,
			GL_VERTEX_ARRAY_COUNT_EXT = 0x807D,
			GL_NORMAL_ARRAY_TYPE_EXT = 0x807E,
			GL_NORMAL_ARRAY_STRIDE_EXT = 0x807F,
			GL_NORMAL_ARRAY_COUNT_EXT = 0x8080,
			GL_COLOR_ARRAY_SIZE_EXT = 0x8081,
			GL_COLOR_ARRAY_TYPE_EXT = 0x8082,
			GL_COLOR_ARRAY_STRIDE_EXT = 0x8083,
			GL_COLOR_ARRAY_COUNT_EXT = 0x8084,
			GL_INDEX_ARRAY_TYPE_EXT = 0x8085,
			GL_INDEX_ARRAY_STRIDE_EXT = 0x8086,
			GL_INDEX_ARRAY_COUNT_EXT = 0x8087,
			GL_TEXTURE_COORD_ARRAY_SIZE_EXT = 0x8088,
			GL_TEXTURE_COORD_ARRAY_TYPE_EXT = 0x8089,
			GL_TEXTURE_COORD_ARRAY_STRIDE_EXT = 0x808A,
			GL_TEXTURE_COORD_ARRAY_COUNT_EXT = 0x808B,
			GL_EDGE_FLAG_ARRAY_STRIDE_EXT = 0x808C,
			GL_EDGE_FLAG_ARRAY_COUNT_EXT = 0x808D,
			GL_VERTEX_ARRAY_POINTER_EXT = 0x808E,
			GL_NORMAL_ARRAY_POINTER_EXT = 0x808F,
			GL_COLOR_ARRAY_POINTER_EXT = 0x8090,
			GL_INDEX_ARRAY_POINTER_EXT = 0x8091,
			GL_TEXTURE_COORD_ARRAY_POINTER_EXT = 0x8092,
			GL_EDGE_FLAG_ARRAY_POINTER_EXT = 0x8093,
			GL_DOUBLE_EXT = GL_DOUBLE;

		/* EXT_bgra */
		public const System.Int32
			GL_BGR_EXT = 0x80E0,
			GL_BGRA_EXT = 0x80E1;

		/* These must match the GL_COLOR_TABLE_*_SGI enumerants */
		public const System.Int32
			GL_COLOR_TABLE_FORMAT_EXT = 0x80D8,
			GL_COLOR_TABLE_WIDTH_EXT = 0x80D9,
			GL_COLOR_TABLE_RED_SIZE_EXT = 0x80DA,
			GL_COLOR_TABLE_GREEN_SIZE_EXT = 0x80DB,
			GL_COLOR_TABLE_BLUE_SIZE_EXT = 0x80DC,
			GL_COLOR_TABLE_ALPHA_SIZE_EXT = 0x80DD,
			GL_COLOR_TABLE_LUMINANCE_SIZE_EXT = 0x80DE,
			GL_COLOR_TABLE_INTENSITY_SIZE_EXT = 0x80DF,

			GL_COLOR_INDEX1_EXT = 0x80E2,
			GL_COLOR_INDEX2_EXT = 0x80E3,
			GL_COLOR_INDEX4_EXT = 0x80E4,
			GL_COLOR_INDEX8_EXT = 0x80E5,
			GL_COLOR_INDEX12_EXT = 0x80E6,
			GL_COLOR_INDEX16_EXT = 0x80E7,

			/* WIN_draw_range_elements */
			GL_MAX_ELEMENTS_VERTICES_WIN = 0x80E8,
			GL_MAX_ELEMENTS_INDICES_WIN = 0x80E9,

			/* WIN_phong_shading */
			GL_PHONG_WIN = 0x80EA,
			GL_PHONG_HINT_WIN = 0x80EB,

			/* WIN_specular_fog */
			GL_FOG_SPECULAR_TEXTURE_WIN = 0x80EC;
		#endregion

		#region Functions from opengl32.dll
		[DllImport("opengl32.dll", SetLastError = true)]
		public static extern System.IntPtr wglGetCurrentContext();
		[DllImport("opengl32.dll", SetLastError = true)]
		public static extern System.IntPtr wglGetCurrentDC();
		[DllImport("opengl32.dll", SetLastError = true)]
		public static extern System.IntPtr wglCreateContext(IntPtr hdc);
		[DllImport("opengl32.dll", SetLastError = true)]
		public static extern System.Int32 wglMakeCurrent(IntPtr hdc, IntPtr hglrc);
		[DllImport("opengl32.dll", SetLastError = true)]
		public static extern System.Int32 wglDeleteContext(IntPtr hglrc);

		[DllImport("opengl32.dll")]
		public static extern void glAccum(uint op, float value);
		[DllImport("opengl32.dll")]
		public static extern void glAlphaFunc(uint func, float aRef);
		[DllImport("opengl32.dll")]
		unsafe public static extern byte glAreTexturesResident(int n, uint* textures, byte* residences);
		[DllImport("opengl32.dll")]
		public static extern void glArrayElement(int i);
		[DllImport("opengl32.dll")]
		public static extern void glBindTexture(uint target, uint texture);

		[DllImport("opengl32.dll")]
		unsafe public static extern void glBitmap(int width, int height, float xorig, float yorig, float xmove, float ymove, byte* bitmap);
		[DllImport("opengl32.dll")]
		public static extern void glBlendFunc(uint sfactor, uint dfactor);
		[DllImport("opengl32.dll")]
		public static extern void glCallList(uint list);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glCallLists(int n, uint type, void* lists);
		[DllImport("opengl32.dll", EntryPoint = "glClear")]	// TODO: move
		public static extern void Clear(GLbitfield mask);
		[DllImport("opengl32.dll")]	unsafe public static extern void glClear(GLbitfield mask);
		[DllImport("opengl32.dll")]
		public static extern void glClearAccum(float red, float green, float blue, float alpha);
		[DllImport("opengl32.dll", EntryPoint = "glClearColor")]
		public static extern void ClearColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
		[DllImport("opengl32.dll", EntryPoint = "glClearDepth")]
		public static extern void ClearDepth(GLclampd depth);
		[DllImport("opengl32.dll")]
		public static extern void glClearIndex(float c);
		[DllImport("opengl32.dll")]
		public static extern void glClearStencil(int s);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glClipPlane(uint plane, double* equation);
		[DllImport("opengl32.dll")]
		public static extern void glColorMask(byte red, byte green, byte blue, byte alpha);
		[DllImport("opengl32.dll")]
		public static extern void glColorMaterial(uint face, uint mode);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glColorPointer(int size, uint type, int stride, void* pointer);
		[DllImport("opengl32.dll")]
		public static extern void glCopyPixels(int x, int y, int width, int height, uint type);
		[DllImport("opengl32.dll")]
		public static extern void glCopyTexImage1D(uint target, int level, uint internalformat, int x, int y, int width, int border);
		[DllImport("opengl32.dll")]
		public static extern void glCopyTexImage2D(uint target, int level, uint internalformat, int x, int y, int width, int height, int border);
		[DllImport("opengl32.dll")]
		public static extern void glCopyTexSubImage1D(uint target, int level, int xoffset, int x, int y, int width);
		[DllImport("opengl32.dll")]
		public static extern void glCopyTexSubImage2D(uint target, int level, int xoffset, int yoffset, int x, int y, int width, int height);
		[DllImport("opengl32.dll")]
		public static extern void glDeleteLists(uint list, int range);
		/// <summary>
		/// specify the value used for depth buffer comparisons
		/// </summary>
		/// <param name="func"></param>
		[DllImport("opengl32.dll", EntryPoint = "glDepthFunc")]			public static extern void DepthFunc(GLenum func);
		[DllImport("opengl32.dll")]
		public static extern void glDepthMask(byte flag);
		[DllImport("opengl32.dll")]
		public static extern void glDepthRange(double zNear, double zFar);
		[DllImport("opengl32.dll")]
		public static extern void glDisableClientState(uint array);
		[DllImport("opengl32.dll")]
		public static extern void glDrawBuffer(uint mode);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glDrawElements(uint mode, int count, uint type, void* indices);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glDrawPixels(int width, int height, uint format, uint type, void* pixels);
		[DllImport("opengl32.dll")]
		public static extern void glEdgeFlag(byte flag);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glEdgeFlagPointer(int stride, byte* pointer);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glEdgeFlagv(byte* flag);
		[DllImport("opengl32.dll")]
		public static extern void glEnableClientState(uint array);
		[DllImport("opengl32.dll", EntryPoint = "glEnd")]	// TODO: move
		public static extern void End();
		[DllImport("opengl32.dll")]	unsafe public static extern void glEnd();
		[DllImport("opengl32.dll")]
		public static extern void glEndList();
		[DllImport("opengl32.dll")]
		public static extern void glEvalCoord1d(double u);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glEvalCoord1dv(double* u);
		[DllImport("opengl32.dll")]
		public static extern void glEvalCoord1f(float u);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glEvalCoord1fv(float* u);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glEvalCoord2d(double u, double v);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glEvalCoord2dv(double* u);
		[DllImport("opengl32.dll")]
		public static extern void glEvalCoord2f(float u, float v);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glEvalCoord2fv(float* u);
		[DllImport("opengl32.dll")]
		public static extern void glEvalMesh1(uint mode, int i1, int i2);
		[DllImport("opengl32.dll")]
		public static extern void glEvalMesh2(uint mode, int i1, int i2, int j1, int j2);
		[DllImport("opengl32.dll")]
		public static extern void glEvalPoint1(int i);
		[DllImport("opengl32.dll")]
		public static extern void glEvalPoint2(int i, int j);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glFeedbackBuffer(int size, uint type, float* buffer);
		[DllImport("opengl32.dll")]
		public static extern void glFinish();
		[DllImport("opengl32.dll")]
		public static extern void glFlush();
		[DllImport("opengl32.dll")]
		public static extern void glFogf(uint pname, float param);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glFogfv(uint pname, float* someParams);
		[DllImport("opengl32.dll")]
		public static extern void glFogi(uint pname, int param);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glFogiv(uint pname, int* someParams);
		[DllImport("opengl32.dll")]
		public static extern void glFrustum(double left, double right, double bottom, double top, double zNear, double zFar);
		[DllImport("opengl32.dll")]
		public static extern uint glGenLists(int range);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGenTextures(int n, uint* textures);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGenTextures(int n, out uint textures);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetClipPlane(uint plane, double* equation);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetLightfv(uint light, uint pname, float* someParams);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetLightiv(uint light, uint pname, int* someParams);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetMapdv(uint target, uint query, double* v);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetMapfv(uint target, uint query, float* v);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetMapiv(uint target, uint query, int* v);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetMaterialfv(uint face, uint pname, float* someParams);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetMaterialiv(uint face, uint pname, int* someParams);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetPixelMapfv(uint map, float* values);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetPixelMapuiv(uint map, uint* values);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetPixelMapusv(uint map, ushort* values);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetPointerv(uint pname, void** someParams);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetPolygonStipple(byte* mask);
		[DllImport("opengl32.dll")]
		unsafe public static extern IntPtr glGetString(uint name);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetTexEnvfv(uint target, uint pname, float* someParams);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetTexEnviv(uint target, uint pname, int* someParams);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetTexGendv(uint coord, uint pname, double* someParams);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetTexGenfv(uint coord, uint pname, float* someParams);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetTexGeniv(uint coord, uint pname, int* someParams);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetTexImage(uint target, int level, uint format, uint type, void* pixels);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetTexLevelParameterfv(uint target, int level, uint pname, float* someParams);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetTexLevelParameteriv(uint target, int level, uint pname, int* someParams);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetTexParameterfv(uint target, uint pname, float* someParams);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glGetTexParameteriv(uint target, uint pname, int* someParams);
		[DllImport("opengl32.dll")]
		public static extern void glIndexMask(uint mask);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glIndexPointer(uint type, int stride, void* pointer);
		[DllImport("opengl32.dll")]
		public static extern void glIndexd(double c);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glIndexdv(double* c);
		[DllImport("opengl32.dll")]
		public static extern void glIndexf(float c);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glIndexfv(float* c);
		[DllImport("opengl32.dll")]
		public static extern void glIndexi(int c);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glIndexiv(int* c);
		[DllImport("opengl32.dll")]
		public static extern void glIndexs(short c);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glIndexsv(short* c);
		[DllImport("opengl32.dll")]
		public static extern void glIndexub(byte c);
		[DllImport("opengl32.dll")]
		public static extern void glLightModelf(uint pname, float param);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glLightModelfv(uint pname, float* someParams);
		[DllImport("opengl32.dll")]
		public static extern void glLightModeli(uint pname, int param);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glLightModeliv(uint pname, int* someParams);
		[DllImport("opengl32.dll")]
		public static extern void glLightf(uint light, uint pname, float param);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glLightfv(uint light, uint pname, float* someParams);
		[DllImport("opengl32.dll")]
		public static extern void glLighti(uint light, uint pname, int param);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glLightiv(uint light, uint pname, int* someParams);
		[DllImport("opengl32.dll")]
		public static extern void glLineStipple(int factor, ushort pattern);
		[DllImport("opengl32.dll")]
		public static extern void glLineWidth(float width);
		[DllImport("opengl32.dll")]
		public static extern void glListBase(uint aBase);
		[DllImport("opengl32.dll", EntryPoint = "glLoadIdentity")]	// TODO: move
		public static extern void LoadIdentity();
		[DllImport("opengl32")]	unsafe public static extern void glLoadIdentity();
		[DllImport("opengl32.dll")]
		unsafe public static extern void glLoadMatrixd(double* m);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glLoadMatrixf(float* m);
		[DllImport("opengl32.dll")]
		public static extern void glLoadName(uint name);
		[DllImport("opengl32.dll")]
		public static extern void glLogicOp(uint opcode);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glMap1d(uint target, double u1, double u2, int stride, int order, double* points);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glMap1f(uint target, float u1, float u2, int stride, int order, float* points);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glMap2d(uint target, double u1, double u2, int ustride, int uorder, double v1, double v2, int vstride, int vorder, double* points);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glMap2f(uint target, float u1, float u2, int ustride, int uorder, float v1, float v2, int vstride, int vorder, float* points);
		[DllImport("opengl32.dll")]
		public static extern void glMapGrid1d(int un, double u1, double u2);
		[DllImport("opengl32.dll")]
		public static extern void glMapGrid1f(int un, float u1, float u2);
		[DllImport("opengl32.dll")]
		public static extern void glMapGrid2d(int un, double u1, double u2, int vn, double v1, double v2);
		[DllImport("opengl32.dll")]
		public static extern void glMapGrid2f(int un, float u1, float u2, int vn, float v1, float v2);
		[DllImport("opengl32.dll")]
		public static extern void glMaterialf(uint face, uint pname, float param);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glMaterialfv(uint face, uint pname, float* someParams);
		[DllImport("opengl32.dll")]
		public static extern void glMateriali(uint face, uint pname, int param);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glMaterialiv(uint face, uint pname, int* someParams);
		[DllImport("opengl32.dll", EntryPoint = "glMatrixMode")]	// TODO: move
		public static extern void MatrixMode(GLenum mode);
		[DllImport("opengl32.dll")]	unsafe public static extern void glMatrixMode(GLenum mode);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glMultMatrixd(double* m);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glMultMatrixf(float* m);
		[DllImport("opengl32.dll")]
		public static extern void glNewList(uint list, uint mode);
		[DllImport("opengl32.dll")]
		public static extern void glNormal3b(sbyte nx, sbyte ny, sbyte nz);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glNormal3bv(sbyte* v);
		[DllImport("opengl32.dll")]
		public static extern void glNormal3d(double nx, double ny, double nz);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glNormal3dv(double* v);
		[DllImport("opengl32.dll")]
		public static extern void glNormal3f(float nx, float ny, float nz);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glNormal3fv(float* v);
		[DllImport("opengl32.dll")]
		public static extern void glNormal3i(int nx, int ny, int nz);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glNormal3iv(int* v);
		[DllImport("opengl32.dll")]
		public static extern void glNormal3s(short nx, short ny, short nz);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glNormal3sv(short* v);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glNormalPointer(uint type, int stride, void* pointer);
		[DllImport("opengl32.dll")]
		public static extern void glOrtho(double left, double right, double bottom, double top, double zNear, double zFar);
		[DllImport("opengl32.dll")]
		public static extern void glPassThrough(float token);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glPixelMapfv(uint map, int mapsize, float* values);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glPixelMapuiv(uint map, int mapsize, uint* values);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glPixelMapusv(uint map, int mapsize, ushort* values);
		[DllImport("opengl32.dll")]
		public static extern void glPixelStoref(uint pname, float param);
		[DllImport("opengl32.dll")]
		public static extern void glPixelStorei(uint pname, int param);
		[DllImport("opengl32.dll")]
		public static extern void glPixelTransferf(uint pname, float param);
		[DllImport("opengl32.dll")]
		public static extern void glPixelTransferi(uint pname, int param);
		[DllImport("opengl32.dll")]
		public static extern void glPixelZoom(float xfactor, float yfactor);
		[DllImport("opengl32.dll")]
		public static extern void glPointSize(float size);
		[DllImport("opengl32.dll")]
		public static extern void glPolygonMode(uint face, uint mode);
		[DllImport("opengl32.dll")]
		public static extern void glPolygonOffset(float factor, float units);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glPolygonStipple(byte* mask);
		[DllImport("opengl32.dll")]
		public static extern void glPopAttrib();
		[DllImport("opengl32.dll")]
		public static extern void glPopClientAttrib();
		[DllImport("opengl32.dll", EntryPoint = "glPopMatrix")]
		public static extern void PopMatrix();
		[DllImport("opengl32.dll")]
		public static extern void glPopName();
		[DllImport("opengl32.dll")]
		unsafe public static extern void glPrioritizeTextures(int n, uint* textures, float* priorities);
		[DllImport("opengl32.dll")]
		public static extern void glPushAttrib(uint mask);
		[DllImport("opengl32.dll")]
		public static extern void glPushClientAttrib(uint mask);
		[DllImport("opengl32.dll", EntryPoint = "glPushMatrix")]
		public static extern void PushMatrix();
		[DllImport("opengl32.dll")]
		public static extern void glPushName(uint name);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos2d(double x, double y);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glRasterPos2dv(double* v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos2f(float x, float y);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glRasterPos2fv(float* v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos2i(int x, int y);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glRasterPos2iv(int* v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos2s(short x, short y);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glRasterPos2sv(short* v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos3d(double x, double y, double z);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glRasterPos3dv(double* v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos3f(float x, float y, float z);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glRasterPos3fv(float* v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos3i(int x, int y, int z);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glRasterPos3iv(int* v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos3s(short x, short y, short z);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glRasterPos3sv(short* v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos4d(double x, double y, double z, double w);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glRasterPos4dv(double* v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos4f(float x, float y, float z, float w);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glRasterPos4fv(float* v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos4i(int x, int y, int z, int w);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glRasterPos4iv(int* v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos4s(short x, short y, short z, short w);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glRasterPos4sv(short* v);
		[DllImport("opengl32.dll")]
		public static extern void glReadBuffer(uint mode);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glReadPixels(int x, int y, int width, int height, uint format, uint type, void* pixels);
		[DllImport("opengl32.dll")]
		public static extern void glRectd(double x1, double y1, double x2, double y2);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glRectdv(double* v1, double* v2);
		[DllImport("opengl32.dll")]
		public static extern void glRectf(float x1, float y1, float x2, float y2);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glRectfv(float* v1, float* v2);
		[DllImport("opengl32.dll")]
		public static extern void glRecti(int x1, int y1, int x2, int y2);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glRectiv(int* v1, int* v2);
		[DllImport("opengl32.dll")]
		public static extern void glRects(short x1, short y1, short x2, short y2);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glRectsv(short* v1, short* v2);
		[DllImport("opengl32.dll")]
		public static extern int glRenderMode(uint mode);
		[DllImport("opengl32.dll")]
		public static extern void glRotated(double angle, double x, double y, double z);
		[DllImport("opengl32.dll")]
		public static extern void glRotatef(float angle, float x, float y, float z);
		[DllImport("opengl32.dll")]
		public static extern void glScaled(double x, double y, double z);
		[DllImport("opengl32.dll")]
		public static extern void glScalef(float x, float y, float z);
		[DllImport("opengl32.dll")]
		public static extern void glScissor(int x, int y, int width, int height);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glSelectBuffer(int size, uint* buffer);
		[DllImport("opengl32.dll", EntryPoint = "glShadeModel")]
		public static extern void ShadeModel(GLenum mode);
		[DllImport("opengl32.dll")]
		public static extern void glStencilFunc(uint func, int aRef, uint mask);
		[DllImport("opengl32.dll")]
		public static extern void glStencilMask(uint mask);
		[DllImport("opengl32.dll")]
		public static extern void glStencilOp(uint fail, uint zfail, uint zpass);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord1d(double s);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexCoord1dv(double* v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord1f(float s);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexCoord1fv(float* v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord1i(int s);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexCoord1iv(int* v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord1s(short s);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexCoord1sv(short* v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord2d(double s, double t);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexCoord2dv(double* v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord2f(float s, float t);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexCoord2fv(float* v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord2i(int s, int t);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexCoord2iv(int* v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord2s(short s, short t);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexCoord2sv(short* v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord3d(double s, double t, double r);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexCoord3dv(double* v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord3f(float s, float t, float r);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexCoord3fv(float* v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord3i(int s, int t, int r);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexCoord3iv(int* v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord3s(short s, short t, short r);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexCoord3sv(short* v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord4d(double s, double t, double r, double q);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexCoord4dv(double* v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord4f(float s, float t, float r, float q);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexCoord4fv(float* v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord4i(int s, int t, int r, int q);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexCoord4iv(int* v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord4s(short s, short t, short r, short q);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexCoord4sv(short* v);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexCoordPointer(int size, uint type, int stride, void* pointer);
		[DllImport("opengl32.dll")]
		public static extern void glTexEnvf(uint target, uint pname, float param);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexEnvfv(uint target, uint pname, float* someParams);
		[DllImport("opengl32.dll")]
		public static extern void glTexEnvi(uint target, uint pname, int param);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexEnviv(uint target, uint pname, int* someParams);
		[DllImport("opengl32.dll")]
		public static extern void glTexGend(uint coord, uint pname, double param);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexGendv(uint coord, uint pname, double* someParams);
		[DllImport("opengl32.dll")]
		public static extern void glTexGenf(uint coord, uint pname, float param);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexGenfv(uint coord, uint pname, float* someParams);
		[DllImport("opengl32.dll")]
		public static extern void glTexGeni(uint coord, uint pname, int param);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexGeniv(uint coord, uint pname, int* someParams);
		[DllImport("opengl32.dll")]
		public static extern void glTexParameterf(uint target, uint pname, float param);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexParameterfv(uint target, uint pname, float* someParams);
		[DllImport("opengl32.dll")]
		public static extern void glTexParameteri(uint target, uint pname, int param);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexParameteriv(uint target, uint pname, int* someParams);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexSubImage1D(uint target, int level, int xoffset, int width, uint format, uint type, void* pixels);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glTexSubImage2D(uint target, int level, int xoffset, int yoffset, int width, int height, uint format, uint type, void* pixels);

		[DllImport("opengl32.dll")]
		public static extern void glVertex2d(double x, double y);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glVertex2dv(double* v);
		[DllImport("opengl32.dll")]
		public static extern void glVertex2f(float x, float y);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glVertex2fv(float* v);
		[DllImport("opengl32.dll")]
		public static extern void glVertex2i(int x, int y);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glVertex2iv(int* v);
		[DllImport("opengl32.dll")]
		public static extern void glVertex2s(short x, short y);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glVertex2sv(short* v);
		[DllImport("opengl32.dll")]
		public static extern void glVertex3d(double x, double y, double z);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glVertex3dv(double* v);
		[DllImport("opengl32.dll", EntryPoint = "glVertex3f")]	// TODO: move
		public static extern void Vertex(float x, float y, float z);
		[DllImport("opengl32.dll")]	unsafe public static extern void glVertex3f(float x, float y, float z);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glVertex3fv(float* v);
		[DllImport("opengl32.dll", EntryPoint = "glVertex3i")]
		public static extern void Vertex(int x, int y, int z);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glVertex3iv(int* v);
		[DllImport("opengl32.dll", EntryPoint = "glVertex3s")]
		public static extern void Vertex(short x, short y, short z);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glVertex3sv(short* v);
		[DllImport("opengl32.dll", EntryPoint = "glVertex4d")]
		public static extern void Vertex(double x, double y, double z, double w);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glVertex4dv(double* v);
		[DllImport("opengl32.dll")]
		public static extern void glVertex4f(float x, float y, float z, float w);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glVertex4fv(float* v);
		[DllImport("opengl32.dll")]
		public static extern void glVertex4i(int x, int y, int z, int w);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glVertex4iv(int* v);
		[DllImport("opengl32.dll")]
		public static extern void glVertex4s(short x, short y, short z, short w);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glVertex4sv(short* v);
		[DllImport("opengl32.dll")]
		unsafe public static extern void glVertexPointer(int size, uint type, int stride, void* pointer);

		[DllImport("opengl32.dll")]
		unsafe public static extern void glIndexubv(byte* c);
		[DllImport("opengl32.dll")]
		public static extern void glInitNames();
		[DllImport("opengl32.dll")]
		unsafe public static extern void glInterleavedArrays(uint format, int stride, void* pointer);
		[DllImport("opengl32.dll")]
		public static extern byte glIsEnabled(uint cap);
		[DllImport("opengl32.dll")]
		public static extern byte glIsList(uint list);
		[DllImport("opengl32.dll")]
		public static extern byte glIsTexture(uint texture);
		[DllImport("opengl32.dll")]
		public static extern void glGetTexImage(uint target, int level, uint format, uint type, IntPtr pixels);
		[DllImport("opengl32.dll")]
		public static extern void glTexSubImage1D(uint target, int level, int xoffset, int width, uint format, uint type, IntPtr pixels);
		[DllImport("opengl32.dll")]
		public static extern void glTexSubImage2D(uint target, int level, int xoffset, int yoffset, int width, int height, uint format, uint type, IntPtr pixels);
		[DllImport("opengl32.dll")]
		public static extern void glGetTexImage(uint target, int level, uint format, uint type, byte[] pixels);
		[DllImport("opengl32.dll")]
		public static extern void glTexSubImage1D(uint target, int level, int xoffset, int width, uint format, uint type, byte[] pixels);
		[DllImport("opengl32.dll")]
		public static extern void glTexSubImage2D(uint target, int level, int xoffset, int yoffset, int width, int height, uint format, uint type, byte[] pixels);
		[DllImport("opengl32.dll")]
		public static extern void glTexSubImage2D(uint target, int level, int xoffset, int yoffset, int width, int height, uint format, uint type, uint[] pixels);
		[DllImport("opengl32.dll")]
		public static extern void glTexSubImage2D(uint target, int level, int xoffset, int yoffset, int width, int height, uint format, uint type, byte[, ,] pixels);

		[DllImport("opengl32.dll")]
		public static extern void glGetTexImage(uint target, int level, uint format, uint type, uint[] pixels);
		[DllImport("opengl32.dll")]
		public static extern void glTexSubImage1D(uint target, int level, int xoffset, int width, uint format, uint type, uint[] pixels);
		[DllImport("opengl32.dll")]
		public static extern void glBitmap(int width, int height, float xorig, float yorig, float xmove, float ymove, byte[] bitmap);
		[DllImport("opengl32.dll")]
		public static extern void glBitmap(int width, int height, float xorig, float yorig, float xmove, float ymove, IntPtr bitmap);

		[DllImport("opengl32.dll")]
		public static extern void glTexParameteri(uint target, uint pname, uint param);
		[DllImport("opengl32.dll")]
		public static extern void glGetTexLevelParameterfv(uint target, int level, uint pname, float[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetTexLevelParameteriv(uint target, int level, uint pname, int[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetTexParameterfv(uint target, uint pname, float[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetTexParameteriv(uint target, uint pname, int[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetTexLevelParameterfv(uint target, int level, uint pname, out float someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetTexLevelParameteriv(uint target, int level, uint pname, out int someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetTexParameterfv(uint target, uint pname, out float someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetTexParameteriv(uint target, uint pname, out int someParams);

		[DllImport("opengl32.dll")]
		public static extern void glCallLists(int n, uint type, byte[] lists);
		[DllImport("opengl32.dll")]
		public static extern void glCallLists(int n, uint type, sbyte[] lists);
		[DllImport("opengl32.dll")]
		public static extern void glCallLists(int n, uint type, short[] lists);
		[DllImport("opengl32.dll")]
		public static extern void glCallLists(int n, uint type, ushort[] lists);
		[DllImport("opengl32.dll")]
		public static extern void glCallLists(int n, uint type, int[] lists);
		[DllImport("opengl32.dll")]
		public static extern void glCallLists(int n, uint type, uint[] lists);
		[DllImport("opengl32.dll")]
		public static extern void glCallLists(int n, uint type, float[] lists);
		[DllImport("opengl32.dll")]
		public static extern void glCallLists(int n, uint type, char[] lists);
		[DllImport("opengl32.dll")]
		public static extern void glCallLists(int n, uint type, [MarshalAs(UnmanagedType.LPWStr)] string lists);

		[DllImport("opengl32.dll")]
		public static extern void glRasterPos2dv(double[] v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos2fv(float[] v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos2iv(int[] v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos2sv(short[] v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos3dv(double[] v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos3fv(float[] v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos3iv(int[] v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos3sv(short[] v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos4dv(double[] v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos4fv(float[] v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos4iv(int[] v);
		[DllImport("opengl32.dll")]
		public static extern void glRasterPos4sv(short[] v);

		[DllImport("opengl32.dll")]
		public static extern void glReadPixels(int x, int y, int width, int height, uint format, uint type, IntPtr pixels);
		[DllImport("opengl32.dll")]
		public static extern void glReadPixels(int x, int y, int width, int height, uint format, uint type, byte[] pixels);
		[DllImport("opengl32.dll")]
		public static extern void glReadPixels(int x, int y, int width, int height, uint format, uint type, ushort[] pixels);
		[DllImport("opengl32.dll")]
		public static extern void glReadPixels(int x, int y, int width, int height, uint format, uint type, uint[] pixels);
		[DllImport("opengl32.dll")]
		public static extern void glDrawPixels(int width, int height, uint format, uint type, IntPtr pixels);
		[DllImport("opengl32.dll")]
		public static extern void glDrawPixels(int width, int height, uint format, uint type, byte[] pixels);
		[DllImport("opengl32.dll")]
		public static extern void glDrawPixels(int width, int height, uint format, uint type, ushort[] pixels);
		[DllImport("opengl32.dll")]
		public static extern void glDrawPixels(int width, int height, uint format, uint type, uint[] pixels);

		[DllImport("opengl32.dll")]
		public static extern void glGenTextures(int n, uint[] textures);
		[DllImport("opengl32.dll")]
		public static extern void glDeleteTextures(int n, uint[] textures);
		[DllImport("opengl32.dll")]
		public static extern void glNormal3bv(sbyte[] v);
		[DllImport("opengl32.dll")]
		public static extern void glNormal3dv(double[] v);
		[DllImport("opengl32.dll")]
		public static extern void glNormal3fv(float[] v);
		[DllImport("opengl32.dll")]
		public static extern void glNormal3iv(int[] v);
		[DllImport("opengl32.dll")]
		public static extern void glNormal3sv(short[] v);
		[DllImport("opengl32.dll")]
		public static extern void glVertex2dv(double[] v);
		[DllImport("opengl32.dll")]
		public static extern void glVertex2fv(float[] v);
		[DllImport("opengl32.dll")]
		public static extern void glVertex2iv(int[] v);
		[DllImport("opengl32.dll")]
		public static extern void glVertex2sv(short[] v);
		[DllImport("opengl32.dll")]
		public static extern void glVertex3dv(double[] v);
		[DllImport("opengl32.dll")]
		public static extern void glVertex3fv(float[] v);
		[DllImport("opengl32.dll")]
		public static extern void glVertex3iv(int[] v);
		[DllImport("opengl32.dll")]
		public static extern void glVertex3sv(short[] v);
		[DllImport("opengl32.dll")]
		public static extern void glVertex4dv(double[] v);
		[DllImport("opengl32.dll")]
		public static extern void glVertex4fv(float[] v);
		[DllImport("opengl32.dll")]
		public static extern void glVertex4iv(int[] v);
		[DllImport("opengl32.dll")]
		public static extern void glVertex4sv(short[] v);
		[DllImport("opengl32.dll")]
		public static extern void glLightModelfv(uint pname, float[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glLightModeliv(uint pname, int[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glLightfv(uint light, uint pname, float[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glLightiv(uint light, uint pname, int[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glLoadMatrixd(double[] m);
		[DllImport("opengl32.dll")]
		public static extern void glLoadMatrixf(float[] m);
		[DllImport("opengl32.dll")]
		public static extern void glMultMatrixd(double[] m);
		[DllImport("opengl32.dll")]
		public static extern void glMultMatrixf(float[] m);

		[DllImport("opengl32.dll")]
		public static extern void glEdgeFlagv(ref byte flag);
		[DllImport("opengl32.dll")]
		public static extern void glEdgeFlagv(byte[] flag);

		[DllImport("opengl32.dll")]
		public static extern void glMaterialfv(uint face, uint pname, ref float someParams);
		[DllImport("opengl32.dll")]
		public static extern void glMaterialfv(uint face, uint pname, float[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glMaterialfv(uint face, uint pname, IntPtr someParams);
		[DllImport("opengl32.dll")]
		public static extern void glMaterialiv(uint face, uint pname, ref int someParams);
		[DllImport("opengl32.dll")]
		public static extern void glMaterialiv(uint face, uint pname, int[] someParams);

		[DllImport("opengl32.dll")]
		public static extern void glTexCoord1dv(double[] v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord1fv(float[] v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord1iv(int[] v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord1sv(short[] v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord2dv(double[] v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord2fv(float[] v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord2iv(int[] v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord2sv(short[] v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord3dv(double[] v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord3fv(float[] v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord3iv(int[] v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord3sv(short[] v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord4dv(double[] v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord4fv(float[] v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord4iv(int[] v);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoord4sv(short[] v);

		[DllImport("opengl32.dll")]
		public static extern void glSelectBuffer(int size, uint[] buffer);
		[DllImport("opengl32.dll")]
		public static extern void glSelectBuffer(int size, IntPtr buffer);

		[DllImport("opengl32.dll")]
		public static extern void glFeedbackBuffer(int size, uint type, float[] buffer);
		[DllImport("opengl32.dll")]
		public static extern void glFeedbackBuffer(int size, uint type, IntPtr buffer);

		[DllImport("opengl32.dll")]
		public static extern byte glAreTexturesResident(int n, out uint textures, out byte residences);
		[DllImport("opengl32.dll")]
		public static extern byte glAreTexturesResident(int n, uint[] textures, byte[] residences);
		[DllImport("opengl32.dll")]
		public static extern void glPrioritizeTextures(int n, uint[] textures, float[] priorities);

		[DllImport("opengl32.dll")]
		public static extern void glClipPlane(uint plane, double[] equation);
		[DllImport("opengl32.dll")]
		public static extern void glGetClipPlane(uint plane, double[] equation);

		[DllImport("opengl32.dll")]
		public static extern void glEvalCoord2dv(double[] u);
		[DllImport("opengl32.dll")]
		public static extern void glEvalCoord2fv(float[] u);
		[DllImport("opengl32.dll")]
		public static extern void glEvalCoord1dv(double[] u);
		[DllImport("opengl32.dll")]
		public static extern void glEvalCoord1fv(float[] u);

		[DllImport("opengl32.dll")]
		public static extern void glFogfv(uint pname, float[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glFogiv(uint pname, int[] someParams);

		[DllImport("opengl32.dll")]
		public static extern void glGetLightfv(uint light, uint pname, float[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetLightiv(uint light, uint pname, int[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetLightfv(uint light, uint pname, out float someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetLightiv(uint light, uint pname, out int someParams);

		[DllImport("opengl32.dll")]
		public static extern void glGetMaterialfv(uint face, uint pname, out float someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetMaterialiv(uint face, uint pname, out int someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetMaterialfv(uint face, uint pname, float[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetMaterialiv(uint face, uint pname, int[] someParams);

		[DllImport("opengl32.dll")]
		public static extern void glGetPolygonStipple(byte[] mask);
		[DllImport("opengl32.dll")]
		public static extern void glPolygonStipple(byte[] mask);

		[DllImport("opengl32.dll")]
		public static extern void glGetTexEnvfv(uint target, uint pname, out float someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetTexEnviv(uint target, uint pname, out int someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetTexEnvfv(uint target, uint pname, float[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetTexEnviv(uint target, uint pname, int[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glTexEnvfv(uint target, uint pname, float[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glTexEnviv(uint target, uint pname, int[] someParams);

		[DllImport("opengl32.dll")]
		public static extern void glGetTexGendv(uint coord, uint pname, out double someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetTexGenfv(uint coord, uint pname, out float someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetTexGeniv(uint coord, uint pname, out int someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetTexGendv(uint coord, uint pname, double[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetTexGenfv(uint coord, uint pname, float[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glGetTexGeniv(uint coord, uint pname, int[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glTexGendv(uint coord, uint pname, double[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glTexGenfv(uint coord, uint pname, float[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glTexGeniv(uint coord, uint pname, int[] someParams);

		[DllImport("opengl32.dll")]
		public static extern void glIndexdv(double[] c);
		[DllImport("opengl32.dll")]
		public static extern void glIndexfv(float[] c);
		[DllImport("opengl32.dll")]
		public static extern void glIndexiv(int[] c);
		[DllImport("opengl32.dll")]
		public static extern void glIndexsv(short[] c);
		[DllImport("opengl32.dll")]
		public static extern void glIndexubv(byte[] c);
		[DllImport("opengl32.dll")]
		public static extern void glIndexdv(ref double c);
		[DllImport("opengl32.dll")]
		public static extern void glIndexfv(ref float c);
		[DllImport("opengl32.dll")]
		public static extern void glIndexiv(ref int c);
		[DllImport("opengl32.dll")]
		public static extern void glIndexsv(ref short c);
		[DllImport("opengl32.dll")]
		public static extern void glIndexubv(ref byte c);

		[DllImport("opengl32.dll")]
		public static extern void glGetMapdv(uint target, uint query, double[] v);
		[DllImport("opengl32.dll")]
		public static extern void glGetMapfv(uint target, uint query, float[] v);
		[DllImport("opengl32.dll")]
		public static extern void glGetMapiv(uint target, uint query, int[] v);
		[DllImport("opengl32.dll")]
		public static extern void glMap1d(uint target, double u1, double u2, int stride, int order, double[] points);
		[DllImport("opengl32.dll")]
		public static extern void glMap1f(uint target, float u1, float u2, int stride, int order, float[] points);
		[DllImport("opengl32.dll")]
		public static extern void glMap2d(uint target, double u1, double u2, int ustride, int uorder, double v1, double v2, int vstride, int vorder, double[] points);
		[DllImport("opengl32.dll")]
		public static extern void glMap2f(uint target, float u1, float u2, int ustride, int uorder, float v1, float v2, int vstride, int vorder, float[] points);
		[DllImport("opengl32.dll")]
		public static extern void glPixelMapfv(uint map, int mapsize, float[] values);
		[DllImport("opengl32.dll")]
		public static extern void glPixelMapuiv(uint map, int mapsize, uint[] values);
		[DllImport("opengl32.dll")]
		public static extern void glPixelMapusv(uint map, int mapsize, ushort[] values);
		[DllImport("opengl32.dll")]
		public static extern void glGetPixelMapfv(uint map, float[] values);
		[DllImport("opengl32.dll")]
		public static extern void glGetPixelMapuiv(uint map, uint[] values);
		[DllImport("opengl32.dll")]
		public static extern void glGetPixelMapusv(uint map, ushort[] values);

		[DllImport("opengl32.dll")]
		public static extern void glRectdv(double[] v1, double[] v2);
		[DllImport("opengl32.dll")]
		public static extern void glRectfv(float[] v1, float[] v2);
		[DllImport("opengl32.dll")]
		public static extern void glRectiv(int[] v1, int[] v2);
		[DllImport("opengl32.dll")]
		public static extern void glRectsv(short[] v1, short[] v2);

		[DllImport("opengl32.dll")]
		public static extern void glTexParameterfv(uint target, uint pname, float[] someParams);
		[DllImport("opengl32.dll")]
		public static extern void glTexParameteriv(uint target, uint pname, int[] someParams);

		[DllImport("opengl32.dll")]
		public static extern void glColorPointer(int size, uint type, int stride, IntPtr pointer);
		[DllImport("opengl32.dll")]
		public static extern void glColorPointer(int size, uint type, int stride, float[,] pointer);
		[DllImport("opengl32.dll")]
		public static extern void glEdgeFlagPointer(int stride, IntPtr pointer);
		[DllImport("opengl32.dll")]
		public static extern void glIndexPointer(uint type, int stride, IntPtr pointer);
		[DllImport("opengl32.dll")]
		public static extern void glNormalPointer(uint type, int stride, IntPtr pointer);
		[DllImport("opengl32.dll")]
		public static extern void glTexCoordPointer(int size, uint type, int stride, IntPtr pointer);
		[DllImport("opengl32.dll")]
		public static extern void glVertexPointer(int size, uint type, int stride, IntPtr pointer);
		[DllImport("opengl32.dll")]
		public static extern void glVertexPointer(int size, uint type, int stride, float[,] pointer);
		[DllImport("opengl32.dll")]
		public static extern void glInterleavedArrays(uint format, int stride, IntPtr pointer);
		[DllImport("opengl32.dll")]
		public static extern void glDrawElements(uint mode, int count, uint type, IntPtr indices);
		[DllImport("opengl32.dll")]
		public static extern void glDrawElements(uint mode, int count, uint type, byte[,] indices);
		[DllImport("opengl32.dll")]
		public static extern void glGetPointerv(uint pname, out IntPtr someParams);
		#endregion

		#region Defines from glu32.h
		/* QuadricDrawStyle */
		public const int
			GLU_POINT = 100010,
			GLU_LINE = 100011,
			GLU_FILL = 100012,
			GLU_SILHOUETTE = 100013,
			GLU_SMOOTH = 100000,
			GLU_OUTSIDE = 100020,
			GLU_INSIDE = 100021;

		#endregion

		#region Functions from glu32.dll
		[DllImport("glu32.dll")]
		public unsafe static extern void gluOrtho2D(double left, double right, double bottom, double top);
		[DllImport("glu32.dll")]
		public unsafe static extern void gluPerspective(double fovy, double aspect, double zNear, double zFar);
		[DllImport("glu32.dll")]
		public unsafe static extern void gluLookAt(double eyex, double eyey, double eyez, double centerx, double centery, double centerz, double upx, double upy, double upz);
		[DllImport("glu32.dll")]
		public unsafe static extern void gluProject(Double objx, System.Double objy, System.Double objz, System.Double* modelMatrix, System.Double* projMatrix, System.Int32* viewport, System.Double* winx, System.Double* winy, System.Double* winz);
		[DllImport("glu32.dll")]
		public unsafe static extern void gluUnProject(Double winx, System.Double winy, System.Double winz, System.Double* modelMatrix, System.Double* projMatrix, System.Int32* viewport, System.Double* objx, System.Double* objy, System.Double* objz);

		[DllImport("glu32.dll")]
		public unsafe static extern void gluBuild2DMipmaps(uint target, int internalFormat, int width, int height, uint format, uint type, void* data);
		[DllImport("glu32.dll")]
		public unsafe static extern void gluBuild2DMipmaps(uint target, int internalFormat, int width, int height, uint format, uint type, byte[] data);
		[DllImport("glu32.dll")]
		public unsafe static extern void gluBuild2DMipmaps(uint target, int internalFormat, int width, int height, uint format, uint type, IntPtr data);

		[DllImport("glu32")]	public unsafe static extern uint gluNewQuadric();
		[DllImport("glu32")]	public unsafe static extern void gluDeleteQuadric(uint quadric);
		[DllImport("glu32")]	public unsafe static extern void gluSphere(uint quadric, double radius, int slices, int stacks);
		[DllImport("glu32")]	public unsafe static extern void gluQuadricTexture(uint quadric, bool texture);
		[DllImport("glu32")]	public unsafe static extern void gluQuadricNormals(uint quadric, uint normals);
		[DllImport("glu32")]	public unsafe static extern void gluQuadricDrawStyle(uint quadric, int style);
		[DllImport("glu32")]	public unsafe static extern void gluQuadricOrientation(uint quadric, int orientation);

		#endregion

		#region Defines from gdi.h
		/// <summary>
		/// Structure PIXELFORMATDESCRIPTOR.
		/// </summary>
		[StructLayout(LayoutKind.Sequential)]
		public struct PIXELFORMATDESCRIPTOR
		{
			public ushort nSize, nVersion;
			public uint dwFlags;
			public byte
				iPixelType, cColorBits, cRedBits, cRedShift,
				cGreenBits, cGreenShift, cBlueBits, cBlueShift,
				cAlphaBits, cAlphaShift, cAccumBits, cAccumRedBits,
				cAccumGreenBits, cAccumBlueBits, cAccumAlphaBits,
				cDepthBits, cStencilBits, cAuxBuffers, iLayerType, bReserved;
			public uint dwLayerMask, dwVisibleMask, dwDamageMask;

			/// <summary>
			/// Initializes a current instance of the PIXELFORMATDESCRIPTOR
			/// structure with default values.
			/// </summary>
			public void Initialize()
			{
				nSize = (ushort)Marshal.SizeOf(this);
				nVersion = 1;
				dwFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;
				iPixelType = PFD_TYPE_RGBA;
				cColorBits = 24;
				cRedBits = 24; cRedShift = 0;
				cGreenBits = 24; cGreenShift = 0;
				cBlueBits = 24; cBlueShift = 0;
				cAlphaBits = 0; cAlphaShift = 0;
				cAccumBits = cAccumRedBits = cAccumGreenBits = cAccumBlueBits = cAccumAlphaBits = 0;
				cDepthBits = 24;
				cStencilBits = 0;
				cAuxBuffers = 0;
				iLayerType = 0;
				bReserved = 0;
				dwLayerMask = dwVisibleMask = dwDamageMask = 0;
			}
		}
		#endregion

		#region Functions from gdi32.dll
		[DllImport("gdi32.dll", SetLastError = true)]
		unsafe static extern System.Int32 ChoosePixelFormat(IntPtr hdc, ref PIXELFORMATDESCRIPTOR ppfd);
		[DllImport("gdi32.dll", SetLastError = true)]
		unsafe static extern System.Int32 SetPixelFormat(IntPtr hdc, System.Int32 iPixelFormat, ref PIXELFORMATDESCRIPTOR ppfd);
		[DllImport("gdi32.dll", SetLastError = true)]
		unsafe static extern System.Int32 SwapBuffers(IntPtr hdc);

		[DllImport("opengl32.dll")]
		unsafe static extern void wglShareLists(IntPtr hglrc1, IntPtr hglrc2);

		#endregion

		#region Functions from user32.dll
		[DllImport("user32.dll")]
		public static extern IntPtr GetDC(IntPtr hWnd);
		[DllImport("user32.dll")]
		public static extern System.Int32 ReleaseDC(IntPtr hWnd, IntPtr hDC);
		#endregion
	}

	/// <summary>
	/// GL_VERSION_1_0
	/// </summary>
	[GL_VERSION_1_0]
	public partial class OpenGL
	{
		class GL_VERSION_1_0 : OpenGLExtension {
			public GL_VERSION_1_0() : base("GL_VERSION_1_0") { }
		}


		//WINGDIAPI void APIENTRY glAccum (GLenum op, GLfloat value);
		//WINGDIAPI void APIENTRY glAlphaFunc (GLenum func, GLclampf ref);
		//WINGDIAPI GLboolean APIENTRY glAreTexturesResident (GLsizei n, const GLuint *textures, GLboolean *residences);
		//WINGDIAPI void APIENTRY glArrayElement (GLint i);
		#region void glBegin(GLenum mode)
		// BeginMode
		public const GLenum GL_POINTS = 0x0000;
		public const GLenum GL_LINES = 0x0001;
		public const GLenum GL_LINE_LOOP = 0x0002;
		public const GLenum GL_LINE_STRIP = 0x0003;
		public const GLenum GL_TRIANGLES = 0x0004;
		public const GLenum GL_TRIANGLE_STRIP = 0x0005;
		public const GLenum GL_TRIANGLE_FAN = 0x0006;
		public const GLenum GL_QUADS = 0x0007;		// deprecated
		public const GLenum GL_QUAD_STRIP = 0x0008;	// deprecated
		public const GLenum GL_POLYGON = 0x0009;	// deprecated
		[DllImport("opengl32")]	unsafe public static extern void glBegin(GLenum mode);

		public enum BeginMode : uint
		{
			POINTS = GL_POINTS,
			LINES = GL_LINES,
			LINE_LOOP = GL_LINE_LOOP,
			LINE_STRIP = GL_LINE_STRIP,
			TRIANGLES = GL_TRIANGLES,
			TRIANGLE_STRIP = GL_TRIANGLE_STRIP,
			TRIANGLE_FAN = GL_TRIANGLE_FAN,
			QUADS = GL_QUADS,						// deprecated
			QUAD_STRIP = GL_QUAD_STRIP,				// deprecated
			POLYGON = GL_POLYGON					// deprecated
		}
		[DllImport("opengl32", EntryPoint = "glBegin")]
		public static extern void Begin(BeginMode mode);

		#endregion
		//WINGDIAPI void APIENTRY glBindTexture (GLenum target, GLuint texture);
		//WINGDIAPI void APIENTRY glBitmap (GLsizei width, GLsizei height, GLfloat xorig, GLfloat yorig, GLfloat xmove, GLfloat ymove, const GLubyte *bitmap);
		//WINGDIAPI void APIENTRY glBlendFunc (GLenum sfactor, GLenum dfactor);
		//WINGDIAPI void APIENTRY glCallList (GLuint list);
		//WINGDIAPI void APIENTRY glCallLists (GLsizei n, GLenum type, const void *lists);
		//WINGDIAPI void APIENTRY glClear (GLbitfield mask);
		//WINGDIAPI void APIENTRY glClearAccum (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
		//WINGDIAPI void APIENTRY glClearColor (GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
		//WINGDIAPI void APIENTRY glClearDepth (GLclampd depth);
		//WINGDIAPI void APIENTRY glClearIndex (GLfloat c);
		//WINGDIAPI void APIENTRY glClearStencil (GLint s);
		//WINGDIAPI void APIENTRY glClipPlane (GLenum plane, const GLdouble *equation);
		#region void glColor(GL<type> red, GL<type> green, GL<type> blue, [GL<type> alpha])
		[DllImport("opengl32")]	unsafe public static extern void glColor3b(GLbyte red, GLbyte green, GLbyte blue);
		[DllImport("opengl32")]	unsafe public static extern void glColor3s(GLshort red, GLshort green, GLshort blue);
		[DllImport("opengl32")] unsafe public static extern void glColor3i(GLint red, GLint green, GLint blue);
		[DllImport("opengl32")] unsafe public static extern void glColor3f(GLfloat red, GLfloat green, GLfloat blue);
		[DllImport("opengl32")] unsafe public static extern void glColor3d(GLdouble red, GLdouble green, GLdouble blue);
		[DllImport("opengl32")]	unsafe public static extern void glColor3ub(GLbyte red, GLbyte green, GLbyte blue);
		[DllImport("opengl32")] unsafe public static extern void glColor3us(GLushort red, GLushort green, GLushort blue);
		[DllImport("opengl32")] unsafe public static extern void glColor3ui(GLuint red, GLuint green, GLuint blue);
		[DllImport("opengl32")]	unsafe public static extern void glColor4b(GLbyte red, GLbyte green, GLbyte blue, GLbyte alpha);
		[DllImport("opengl32")]	unsafe public static extern void glColor4s(GLshort red, GLshort green, GLshort blue, GLshort alpha);
		[DllImport("opengl32")] unsafe public static extern void glColor4i(GLint red, GLint green, GLint blue, GLint alpha);
		[DllImport("opengl32")] unsafe public static extern void glColor4f(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
		[DllImport("opengl32")] unsafe public static extern void glColor4d(GLdouble red, GLdouble green, GLdouble blue, GLdouble alpha);
		[DllImport("opengl32")]	unsafe public static extern void glColor4ub(GLbyte red, GLbyte green, GLbyte blue, GLbyte alpha);
		[DllImport("opengl32")] unsafe public static extern void glColor4us(GLushort red, GLushort green, GLushort blue, GLushort alpha);
		[DllImport("opengl32")] unsafe public static extern void glColor4ui(GLuint red, GLuint green, GLuint blue, GLuint alpha);
		[DllImport("opengl32")]	unsafe public static extern void glColor3bv(GLbyte[] rgb);
		[DllImport("opengl32")]	unsafe public static extern void glColor3dv(GLdouble[] rgb);
		[DllImport("opengl32")]	unsafe public static extern void glColor3fv(GLfloat[] rgb);
		[DllImport("opengl32")]	unsafe public static extern void glColor3iv(GLint[] rgb);
		[DllImport("opengl32")]	unsafe public static extern void glColor3sv(GLshort[] rgb);
		[DllImport("opengl32")]	unsafe public static extern void glColor3ubv(GLbyte[] rgb);
		[DllImport("opengl32")] unsafe public static extern void glColor3usv(GLushort[] rgb);
		[DllImport("opengl32")] unsafe public static extern void glColor3uiv(GLuint[] rgb);
		[DllImport("opengl32")]	unsafe public static extern void glColor4bv(GLbyte[] rgba);
		[DllImport("opengl32")]	unsafe public static extern void glColor4dv(GLdouble[] rgba);
		[DllImport("opengl32")]	unsafe public static extern void glColor4fv(GLfloat[] rgba);
		[DllImport("opengl32")]	unsafe public static extern void glColor4iv(GLint[] rgba);
		[DllImport("opengl32")]	unsafe public static extern void glColor4sv(GLshort[] rgba);
		[DllImport("opengl32")]	unsafe public static extern void glColor4ubv(GLbyte[] rgba);
		[DllImport("opengl32")] unsafe public static extern void glColor4usv(GLushort[] rgba);
		[DllImport("opengl32")] unsafe public static extern void glColor4uiv(GLuint[] rgba);

		[DllImport("opengl32", EntryPoint = "glColor3b")]	public static extern void Color(sbyte red, sbyte green, sbyte blue);
		[DllImport("opengl32", EntryPoint = "glColor3s")]	public static extern void Color(short red, short green, short blue);
		[DllImport("opengl32", EntryPoint = "glColor3i")]	public static extern void Color(int red, int green, int blue);
		[DllImport("opengl32", EntryPoint = "glColor3f")]	public static extern void Color(float red, float green, float blue);
		[DllImport("opengl32", EntryPoint = "glColor3d")]	public static extern void Color(double red, double green, double blue);
		[DllImport("opengl32", EntryPoint = "glColor3ub")]	public static extern void Color(byte red, byte green, byte blue);
		[DllImport("opengl32", EntryPoint = "glColor3us")]	public static extern void Color(ushort red, ushort green, ushort blue);
		[DllImport("opengl32", EntryPoint = "glColor3ui")]	public static extern void Color(uint red, uint green, uint blue);
		[DllImport("opengl32", EntryPoint = "glColor4b")]	public static extern void Color(sbyte red, sbyte green, sbyte blue, sbyte alpha);
		[DllImport("opengl32", EntryPoint = "glColor4s")]	public static extern void Color(short red, short green, short blue, short alpha);
		[DllImport("opengl32", EntryPoint = "glColor4i")]	public static extern void Color(int red, int green, int blue, int alpha);
		[DllImport("opengl32", EntryPoint = "glColor4f")]	public static extern void Color(float red, float green, float blue, float alpha);
		[DllImport("opengl32", EntryPoint = "glColor4d")]	public static extern void Color(double red, double green, double blue, double alpha);

		#endregion
		//WINGDIAPI void APIENTRY glColorMask (GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);
		//WINGDIAPI void APIENTRY glColorMaterial (GLenum face, GLenum mode);
		//WINGDIAPI void APIENTRY glColorPointer (GLint size, GLenum type, GLsizei stride, const void *pointer);
		//WINGDIAPI void APIENTRY glCopyPixels (GLint x, GLint y, GLsizei width, GLsizei height, GLenum type);
		//WINGDIAPI void APIENTRY glCopyTexImage1D (GLenum target, GLint level, GLenum internalFormat, GLint x, GLint y, GLsizei width, GLint border);
		//WINGDIAPI void APIENTRY glCopyTexImage2D (GLenum target, GLint level, GLenum internalFormat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border);
		//WINGDIAPI void APIENTRY glCopyTexSubImage1D (GLenum target, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width);
		//WINGDIAPI void APIENTRY glCopyTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height);
		#region void glCullFace(GLenum mode)
		[DllImport("opengl32")]	unsafe public static extern void glCullFace(GLenum mode);

		public enum CullFaceMode : uint
		{	// GLenum
			FRONT = GL_FRONT,
			BACK = GL_BACK,
			FRONT_AND_BACK = GL_FRONT_AND_BACK
		}
		/// <summary>
		/// specify whether front- or back-facing facets can be culled
		/// </summary>
		[DllImport("opengl32", EntryPoint = "glCullFace")]
		public static extern void CullFace(CullFaceMode mode);

		#endregion
		//WINGDIAPI void APIENTRY glDeleteLists (GLuint list, GLsizei range);
		//WINGDIAPI void APIENTRY glDeleteTextures (GLsizei n, const GLuint *textures);
		//WINGDIAPI void APIENTRY glDepthFunc (GLenum func);
		//WINGDIAPI void APIENTRY glDepthMask (GLboolean flag);
		//WINGDIAPI void APIENTRY glDepthRange (GLclampd zNear, GLclampd zFar);
		//WINGDIAPI void APIENTRY glDisableClientState (GLenum array);
		//WINGDIAPI void APIENTRY glDrawArrays (GLenum mode, GLint first, GLsizei count);
		//WINGDIAPI void APIENTRY glDrawBuffer (GLenum mode);
		//WINGDIAPI void APIENTRY glDrawElements (GLenum mode, GLsizei count, GLenum type, const void *indices);
		//WINGDIAPI void APIENTRY glDrawPixels (GLsizei width, GLsizei height, GLenum format, GLenum type, const void *pixels);
		//WINGDIAPI void APIENTRY glEdgeFlag (GLboolean flag);
		//WINGDIAPI void APIENTRY glEdgeFlagPointer (GLsizei stride, const void *pointer);
		//WINGDIAPI void APIENTRY glEdgeFlagv (const GLboolean *flag);
		#region void APIENTRY glEnable(GLenum cap), glDisable(GLenum cap)
		// Capability
		/*      GL_FOG */
		/*      GL_LIGHTING */
		/*      GL_TEXTURE_1D */
		/*      GL_TEXTURE_2D */
		/*      GL_LINE_STIPPLE */
		/*      GL_POLYGON_STIPPLE */
		/*      GL_CULL_FACE */
		/*      GL_ALPHA_TEST */
		/*      GL_BLEND */
		/*      GL_INDEX_LOGIC_OP */
		/*      GL_COLOR_LOGIC_OP */
		/*      GL_DITHER */
		/*      GL_STENCIL_TEST */
		/*      GL_DEPTH_TEST */
		/*      GL_CLIP_PLANE0 */
		/*      GL_CLIP_PLANE1 */
		/*      GL_CLIP_PLANE2 */
		/*      GL_CLIP_PLANE3 */
		/*      GL_CLIP_PLANE4 */
		/*      GL_CLIP_PLANE5 */
		/*      GL_LIGHT0 */
		/*      GL_LIGHT1 */
		/*      GL_LIGHT2 */
		/*      GL_LIGHT3 */
		/*      GL_LIGHT4 */
		/*      GL_LIGHT5 */
		/*      GL_LIGHT6 */
		/*      GL_LIGHT7 */
		/*      GL_TEXTURE_GEN_S */
		/*      GL_TEXTURE_GEN_T */
		/*      GL_TEXTURE_GEN_R */
		/*      GL_TEXTURE_GEN_Q */
		/*      GL_MAP1_VERTEX_3 */
		/*      GL_MAP1_VERTEX_4 */
		/*      GL_MAP1_COLOR_4 */
		/*      GL_MAP1_INDEX */
		/*      GL_MAP1_NORMAL */
		/*      GL_MAP1_TEXTURE_COORD_1 */
		/*      GL_MAP1_TEXTURE_COORD_2 */
		/*      GL_MAP1_TEXTURE_COORD_3 */
		/*      GL_MAP1_TEXTURE_COORD_4 */
		/*      GL_MAP2_VERTEX_3 */
		/*      GL_MAP2_VERTEX_4 */
		/*      GL_MAP2_COLOR_4 */
		/*      GL_MAP2_INDEX */
		/*      GL_MAP2_NORMAL */
		/*      GL_MAP2_TEXTURE_COORD_1 */
		/*      GL_MAP2_TEXTURE_COORD_2 */
		/*      GL_MAP2_TEXTURE_COORD_3 */
		/*      GL_MAP2_TEXTURE_COORD_4 */
		/*      GL_POINT_SMOOTH */
		/*      GL_LINE_SMOOTH */
		/*      GL_POLYGON_SMOOTH */
		/*      GL_SCISSOR_TEST */
		/*      GL_COLOR_MATERIAL */
		/*      GL_NORMALIZE */
		/*      GL_AUTO_NORMAL */
		/*      GL_VERTEX_ARRAY */
		/*      GL_NORMAL_ARRAY */
		/*      GL_COLOR_ARRAY */
		/*      GL_INDEX_ARRAY */
		/*      GL_TEXTURE_COORD_ARRAY */
		/*      GL_EDGE_FLAG_ARRAY */
		/*      GL_POLYGON_OFFSET_POINT */
		/*      GL_POLYGON_OFFSET_LINE */
		/*      GL_POLYGON_OFFSET_FILL */
		[DllImport("opengl32")]	unsafe public static extern void glEnable(GLenum cap);
		[DllImport("opengl32")]	unsafe public static extern void glDisable(GLenum cap);
		public enum Capability : uint
		{
			FOG = GL_FOG,
			LIGHTING = GL_LIGHTING,
			TEXTURE_1D = GL_TEXTURE_1D,
			TEXTURE_2D = GL_TEXTURE_2D,
			LINE_STIPPLE = GL_LINE_STIPPLE,
			POLYGON_STIPPLE = GL_POLYGON_STIPPLE,
			CULL_FACE = GL_CULL_FACE,
			ALPHA_TEST = GL_ALPHA_TEST,
			BLEND = GL_BLEND,

			DEPTH_TEST = GL_DEPTH_TEST,
		}

		[DllImport("opengl32.dll", EntryPoint = "glEnable")]
		public static extern void Enable(Capability cap);
		[DllImport("opengl32.dll", EntryPoint = "glEnable")]
		public static extern void Disable(Capability cap);
		#endregion
		//WINGDIAPI void APIENTRY glEnable (GLenum cap);
		//WINGDIAPI void APIENTRY glEnableClientState (GLenum array);
		//WINGDIAPI void APIENTRY glEnd (void);
		//WINGDIAPI void APIENTRY glEndList (void);
		//WINGDIAPI void APIENTRY glEvalCoord1d (GLdouble u);
		//WINGDIAPI void APIENTRY glEvalCoord1dv (const GLdouble *u);
		//WINGDIAPI void APIENTRY glEvalCoord1f (GLfloat u);
		//WINGDIAPI void APIENTRY glEvalCoord1fv (const GLfloat *u);
		//WINGDIAPI void APIENTRY glEvalCoord2d (GLdouble u, GLdouble v);
		//WINGDIAPI void APIENTRY glEvalCoord2dv (const GLdouble *u);
		//WINGDIAPI void APIENTRY glEvalCoord2f (GLfloat u, GLfloat v);
		//WINGDIAPI void APIENTRY glEvalCoord2fv (const GLfloat *u);
		//WINGDIAPI void APIENTRY glEvalMesh1 (GLenum mode, GLint i1, GLint i2);
		//WINGDIAPI void APIENTRY glEvalMesh2 (GLenum mode, GLint i1, GLint i2, GLint j1, GLint j2);
		//WINGDIAPI void APIENTRY glEvalPoint1 (GLint i);
		//WINGDIAPI void APIENTRY glEvalPoint2 (GLint i, GLint j);
		//WINGDIAPI void APIENTRY glFeedbackBuffer (GLsizei size, GLenum type, GLfloat *buffer);
		//WINGDIAPI void APIENTRY glFinish (void);
		//WINGDIAPI void APIENTRY glFlush (void);
		//WINGDIAPI void APIENTRY glFogf (GLenum pname, GLfloat param);
		//WINGDIAPI void APIENTRY glFogfv (GLenum pname, const GLfloat *params);
		//WINGDIAPI void APIENTRY glFogi (GLenum pname, GLint param);
		//WINGDIAPI void APIENTRY glFogiv (GLenum pname, const GLint *params);
		//WINGDIAPI void APIENTRY glFrontFace (GLenum mode);
		#region void glFrontFace(GLenum mode)
		// FrontFaceDirection
		public enum FrontFaceDirection : uint
		{
			CW = GL_CW,
			CCW = GL_CCW
		}
		[DllImport("opengl32")]
		unsafe public static extern void glFrontFace(GLenum mode);

		[DllImport("opengl32", EntryPoint = "glFrontFace")]
		public static extern void FrontFace(FrontFaceDirection mode);

		#endregion
		//WINGDIAPI void APIENTRY glFrustum (GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar);
		//WINGDIAPI GLuint APIENTRY glGenLists (GLsizei range);
		//WINGDIAPI void APIENTRY glGenTextures (GLsizei n, GLuint *textures);
		#region void glGet<type>v(GLenum pname, GL<type> *params)
		// GetTarget
		public const GLenum GL_CURRENT_COLOR = 0x0B00;
		//#define GL_CURRENT_INDEX                  0x0B01
		//#define GL_CURRENT_NORMAL                 0x0B02
		//#define GL_CURRENT_TEXTURE_COORDS         0x0B03
		//#define GL_CURRENT_RASTER_COLOR           0x0B04
		//#define GL_CURRENT_RASTER_INDEX           0x0B05
		//#define GL_CURRENT_RASTER_TEXTURE_COORDS  0x0B06
		//#define GL_CURRENT_RASTER_POSITION        0x0B07
		//#define GL_CURRENT_RASTER_POSITION_VALID  0x0B08
		//#define GL_CURRENT_RASTER_DISTANCE        0x0B09
		//#define GL_POINT_SMOOTH                   0x0B10
		//#define GL_POINT_SIZE                     0x0B11
		//#define GL_POINT_SIZE_RANGE               0x0B12
		//#define GL_POINT_SIZE_GRANULARITY         0x0B13
		//#define GL_LINE_SMOOTH                    0x0B20
		//#define GL_LINE_WIDTH                     0x0B21
		//#define GL_LINE_WIDTH_RANGE               0x0B22
		//#define GL_LINE_WIDTH_GRANULARITY         0x0B23
		//#define GL_LINE_STIPPLE                   0x0B24
		//#define GL_LINE_STIPPLE_PATTERN           0x0B25
		//#define GL_LINE_STIPPLE_REPEAT            0x0B26
		//#define GL_LIST_MODE                      0x0B30
		//#define GL_MAX_LIST_NESTING               0x0B31
		//#define GL_LIST_BASE                      0x0B32
		//#define GL_LIST_INDEX                     0x0B33
		//#define GL_POLYGON_MODE                   0x0B40
		//#define GL_POLYGON_SMOOTH                 0x0B41
		//#define GL_POLYGON_STIPPLE                0x0B42
		//#define GL_EDGE_FLAG                      0x0B43
		//#define GL_CULL_FACE                      0x0B44
		//#define GL_CULL_FACE_MODE                 0x0B45
		//#define GL_FRONT_FACE                     0x0B46
		//#define GL_LIGHTING                       0x0B50
		//#define GL_LIGHT_MODEL_LOCAL_VIEWER       0x0B51
		//#define GL_LIGHT_MODEL_TWO_SIDE           0x0B52
		//#define GL_LIGHT_MODEL_AMBIENT            0x0B53
		//#define GL_SHADE_MODEL                    0x0B54
		//#define GL_COLOR_MATERIAL_FACE            0x0B55
		//#define GL_COLOR_MATERIAL_PARAMETER       0x0B56
		//#define GL_COLOR_MATERIAL                 0x0B57
		//#define GL_FOG                            0x0B60
		//#define GL_FOG_INDEX                      0x0B61
		//#define GL_FOG_DENSITY                    0x0B62
		//#define GL_FOG_START                      0x0B63
		//#define GL_FOG_END                        0x0B64
		//#define GL_FOG_MODE                       0x0B65
		//#define GL_FOG_COLOR                      0x0B66
		//#define GL_DEPTH_RANGE                    0x0B70
		//#define GL_DEPTH_TEST                     0x0B71
		//#define GL_DEPTH_WRITEMASK                0x0B72
		//#define GL_DEPTH_CLEAR_VALUE              0x0B73
		//#define GL_DEPTH_FUNC                     0x0B74
		//#define GL_ACCUM_CLEAR_VALUE              0x0B80
		//#define GL_STENCIL_TEST                   0x0B90
		//#define GL_STENCIL_CLEAR_VALUE            0x0B91
		//#define GL_STENCIL_FUNC                   0x0B92
		//#define GL_STENCIL_VALUE_MASK             0x0B93
		//#define GL_STENCIL_FAIL                   0x0B94
		//#define GL_STENCIL_PASS_DEPTH_FAIL        0x0B95
		//#define GL_STENCIL_PASS_DEPTH_PASS        0x0B96
		//#define GL_STENCIL_REF                    0x0B97
		//#define GL_STENCIL_WRITEMASK              0x0B98
		//#define GL_MATRIX_MODE                    0x0BA0
		//#define GL_NORMALIZE                      0x0BA1
		//#define GL_VIEWPORT                       0x0BA2
		//#define GL_MODELVIEW_STACK_DEPTH          0x0BA3
		//#define GL_PROJECTION_STACK_DEPTH         0x0BA4
		//#define GL_TEXTURE_STACK_DEPTH            0x0BA5
		//#define GL_MODELVIEW_MATRIX               0x0BA6
		//#define GL_PROJECTION_MATRIX              0x0BA7
		//#define GL_TEXTURE_MATRIX                 0x0BA8
		//#define GL_ATTRIB_STACK_DEPTH             0x0BB0
		//#define GL_CLIENT_ATTRIB_STACK_DEPTH      0x0BB1
		//#define GL_ALPHA_TEST                     0x0BC0
		//#define GL_ALPHA_TEST_FUNC                0x0BC1
		//#define GL_ALPHA_TEST_REF                 0x0BC2
		//#define GL_DITHER                         0x0BD0
		//#define GL_BLEND_DST                      0x0BE0
		//#define GL_BLEND_SRC                      0x0BE1
		//#define GL_BLEND                          0x0BE2
		//#define GL_LOGIC_OP_MODE                  0x0BF0
		//#define GL_INDEX_LOGIC_OP                 0x0BF1
		//#define GL_COLOR_LOGIC_OP                 0x0BF2
		//#define GL_AUX_BUFFERS                    0x0C00
		//#define GL_DRAW_BUFFER                    0x0C01
		//#define GL_READ_BUFFER                    0x0C02
		//#define GL_SCISSOR_BOX                    0x0C10
		//#define GL_SCISSOR_TEST                   0x0C11
		//#define GL_INDEX_CLEAR_VALUE              0x0C20
		//#define GL_INDEX_WRITEMASK                0x0C21
		//#define GL_COLOR_CLEAR_VALUE              0x0C22
		//#define GL_COLOR_WRITEMASK                0x0C23
		//#define GL_INDEX_MODE                     0x0C30
		//#define GL_RGBA_MODE                      0x0C31
		//#define GL_DOUBLEBUFFER                   0x0C32
		//#define GL_STEREO                         0x0C33
		//#define GL_RENDER_MODE                    0x0C40
		//#define GL_PERSPECTIVE_CORRECTION_HINT    0x0C50
		//#define GL_POINT_SMOOTH_HINT              0x0C51
		//#define GL_LINE_SMOOTH_HINT               0x0C52
		//#define GL_POLYGON_SMOOTH_HINT            0x0C53
		//#define GL_FOG_HINT                       0x0C54
		//#define GL_TEXTURE_GEN_S                  0x0C60
		//#define GL_TEXTURE_GEN_T                  0x0C61
		//#define GL_TEXTURE_GEN_R                  0x0C62
		//#define GL_TEXTURE_GEN_Q                  0x0C63
		//#define GL_PIXEL_MAP_I_TO_I               0x0C70
		//#define GL_PIXEL_MAP_S_TO_S               0x0C71
		//#define GL_PIXEL_MAP_I_TO_R               0x0C72
		//#define GL_PIXEL_MAP_I_TO_G               0x0C73
		//#define GL_PIXEL_MAP_I_TO_B               0x0C74
		//#define GL_PIXEL_MAP_I_TO_A               0x0C75
		//#define GL_PIXEL_MAP_R_TO_R               0x0C76
		//#define GL_PIXEL_MAP_G_TO_G               0x0C77
		//#define GL_PIXEL_MAP_B_TO_B               0x0C78
		//#define GL_PIXEL_MAP_A_TO_A               0x0C79
		//#define GL_PIXEL_MAP_I_TO_I_SIZE          0x0CB0
		//#define GL_PIXEL_MAP_S_TO_S_SIZE          0x0CB1
		//#define GL_PIXEL_MAP_I_TO_R_SIZE          0x0CB2
		//#define GL_PIXEL_MAP_I_TO_G_SIZE          0x0CB3
		//#define GL_PIXEL_MAP_I_TO_B_SIZE          0x0CB4
		//#define GL_PIXEL_MAP_I_TO_A_SIZE          0x0CB5
		//#define GL_PIXEL_MAP_R_TO_R_SIZE          0x0CB6
		//#define GL_PIXEL_MAP_G_TO_G_SIZE          0x0CB7
		//#define GL_PIXEL_MAP_B_TO_B_SIZE          0x0CB8
		//#define GL_PIXEL_MAP_A_TO_A_SIZE          0x0CB9
		//#define GL_UNPACK_SWAP_BYTES              0x0CF0
		//#define GL_UNPACK_LSB_FIRST               0x0CF1
		//#define GL_UNPACK_ROW_LENGTH              0x0CF2
		//#define GL_UNPACK_SKIP_ROWS               0x0CF3
		//#define GL_UNPACK_SKIP_PIXELS             0x0CF4
		//#define GL_UNPACK_ALIGNMENT               0x0CF5
		//#define GL_PACK_SWAP_BYTES                0x0D00
		//#define GL_PACK_LSB_FIRST                 0x0D01
		//#define GL_PACK_ROW_LENGTH                0x0D02
		//#define GL_PACK_SKIP_ROWS                 0x0D03
		//#define GL_PACK_SKIP_PIXELS               0x0D04
		//#define GL_PACK_ALIGNMENT                 0x0D05
		//#define GL_MAP_COLOR                      0x0D10
		//#define GL_MAP_STENCIL                    0x0D11
		//#define GL_INDEX_SHIFT                    0x0D12
		//#define GL_INDEX_OFFSET                   0x0D13
		//#define GL_RED_SCALE                      0x0D14
		//#define GL_RED_BIAS                       0x0D15
		//#define GL_ZOOM_X                         0x0D16
		//#define GL_ZOOM_Y                         0x0D17
		//#define GL_GREEN_SCALE                    0x0D18
		//#define GL_GREEN_BIAS                     0x0D19
		//#define GL_BLUE_SCALE                     0x0D1A
		//#define GL_BLUE_BIAS                      0x0D1B
		//#define GL_ALPHA_SCALE                    0x0D1C
		//#define GL_ALPHA_BIAS                     0x0D1D
		//#define GL_DEPTH_SCALE                    0x0D1E
		//#define GL_DEPTH_BIAS                     0x0D1F
		//#define GL_MAX_EVAL_ORDER                 0x0D30
		//#define GL_MAX_LIGHTS                     0x0D31
		//#define GL_MAX_CLIP_PLANES                0x0D32
		//#define GL_MAX_TEXTURE_SIZE               0x0D33
		//#define GL_MAX_PIXEL_MAP_TABLE            0x0D34
		//#define GL_MAX_ATTRIB_STACK_DEPTH         0x0D35
		//#define GL_MAX_MODELVIEW_STACK_DEPTH      0x0D36
		//#define GL_MAX_NAME_STACK_DEPTH           0x0D37
		//#define GL_MAX_PROJECTION_STACK_DEPTH     0x0D38
		//#define GL_MAX_TEXTURE_STACK_DEPTH        0x0D39
		//#define GL_MAX_VIEWPORT_DIMS              0x0D3A
		//#define GL_MAX_CLIENT_ATTRIB_STACK_DEPTH  0x0D3B
		//#define GL_SUBPIXEL_BITS                  0x0D50
		//#define GL_INDEX_BITS                     0x0D51
		//#define GL_RED_BITS                       0x0D52
		//#define GL_GREEN_BITS                     0x0D53
		//#define GL_BLUE_BITS                      0x0D54
		//#define GL_ALPHA_BITS                     0x0D55
		//#define GL_DEPTH_BITS                     0x0D56
		//#define GL_STENCIL_BITS                   0x0D57
		//#define GL_ACCUM_RED_BITS                 0x0D58
		//#define GL_ACCUM_GREEN_BITS               0x0D59
		//#define GL_ACCUM_BLUE_BITS                0x0D5A
		//#define GL_ACCUM_ALPHA_BITS               0x0D5B
		//#define GL_NAME_STACK_DEPTH               0x0D70
		//#define GL_AUTO_NORMAL                    0x0D80
		//#define GL_MAP1_COLOR_4                   0x0D90
		//#define GL_MAP1_INDEX                     0x0D91
		//#define GL_MAP1_NORMAL                    0x0D92
		//#define GL_MAP1_TEXTURE_COORD_1           0x0D93
		//#define GL_MAP1_TEXTURE_COORD_2           0x0D94
		//#define GL_MAP1_TEXTURE_COORD_3           0x0D95
		//#define GL_MAP1_TEXTURE_COORD_4           0x0D96
		//#define GL_MAP1_VERTEX_3                  0x0D97
		//#define GL_MAP1_VERTEX_4                  0x0D98
		//#define GL_MAP2_COLOR_4                   0x0DB0
		//#define GL_MAP2_INDEX                     0x0DB1
		//#define GL_MAP2_NORMAL                    0x0DB2
		//#define GL_MAP2_TEXTURE_COORD_1           0x0DB3
		//#define GL_MAP2_TEXTURE_COORD_2           0x0DB4
		//#define GL_MAP2_TEXTURE_COORD_3           0x0DB5
		//#define GL_MAP2_TEXTURE_COORD_4           0x0DB6
		//#define GL_MAP2_VERTEX_3                  0x0DB7
		//#define GL_MAP2_VERTEX_4                  0x0DB8
		//#define GL_MAP1_GRID_DOMAIN               0x0DD0
		//#define GL_MAP1_GRID_SEGMENTS             0x0DD1
		//#define GL_MAP2_GRID_DOMAIN               0x0DD2
		//#define GL_MAP2_GRID_SEGMENTS             0x0DD3
		//#define GL_TEXTURE_1D                     0x0DE0
		//#define GL_TEXTURE_2D                     0x0DE1
		//#define GL_FEEDBACK_BUFFER_POINTER        0x0DF0
		//#define GL_FEEDBACK_BUFFER_SIZE           0x0DF1
		//#define GL_FEEDBACK_BUFFER_TYPE           0x0DF2
		//#define GL_SELECTION_BUFFER_POINTER       0x0DF3
		//#define GL_SELECTION_BUFFER_SIZE          0x0DF4
		///*      GL_TEXTURE_BINDING_1D */
		///*      GL_TEXTURE_BINDING_2D */
		///*      GL_VERTEX_ARRAY */
		///*      GL_NORMAL_ARRAY */
		///*      GL_COLOR_ARRAY */
		///*      GL_INDEX_ARRAY */
		///*      GL_TEXTURE_COORD_ARRAY */
		///*      GL_EDGE_FLAG_ARRAY */
		///*      GL_VERTEX_ARRAY_SIZE */
		///*      GL_VERTEX_ARRAY_TYPE */
		///*      GL_VERTEX_ARRAY_STRIDE */
		///*      GL_NORMAL_ARRAY_TYPE */
		///*      GL_NORMAL_ARRAY_STRIDE */
		///*      GL_COLOR_ARRAY_SIZE */
		///*      GL_COLOR_ARRAY_TYPE */
		///*      GL_COLOR_ARRAY_STRIDE */
		///*      GL_INDEX_ARRAY_TYPE */
		///*      GL_INDEX_ARRAY_STRIDE */
		///*      GL_TEXTURE_COORD_ARRAY_SIZE */
		///*      GL_TEXTURE_COORD_ARRAY_TYPE */
		///*      GL_TEXTURE_COORD_ARRAY_STRIDE */
		///*      GL_EDGE_FLAG_ARRAY_STRIDE */
		///*      GL_POLYGON_OFFSET_FACTOR */
		///*      GL_POLYGON_OFFSET_UNITS */
		[DllImport("opengl32")] unsafe public static extern void glGetBooleanv(GLenum pname, GLboolean* bparams);
		[DllImport("opengl32")] unsafe public static extern void glGetDoublev(GLenum pname, GLdouble* dparams);
		[DllImport("opengl32")]	unsafe public static extern void glGetFloatv(GLenum pname, GLfloat* fparams);
		[DllImport("opengl32")]	unsafe public static extern void glGetIntegerv(GLenum pname, GLint* iparams);
		[DllImport("opengl32")]	unsafe public static extern void glGetIntegerv(GLenum pname, GLuint* uparams);

		public enum GetTarget : uint
		{
			/// <summary>
			/// returns four values: the red, green, blue, and alpha values of the current color. Integer values, if requested, are linearly mapped from the internal floating-point representation such that 1.0 returns the most positive representable integer value, and -1.0 returns the most negative representable integer value. The initial value is (1, 1, 1, 1).
			/// </summary>
			CURRENT_COLOR = GL_CURRENT_COLOR,
			MODELVIEW_MATRIX = GL_MODELVIEW_MATRIX,
		}
		[DllImport("opengl32", EntryPoint = "glGetBooleanv")]
		public static extern void GetBoolean(GetTarget pname, GLboolean[] pparams);
		[DllImport("opengl32", EntryPoint = "glGetBooleanv")]
		public static extern void GetBoolean(GetTarget pname, out GLboolean pparams);

		[DllImport("opengl32", EntryPoint = "glGetDoublev")]
		public static extern void GetDouble(GetTarget pname, GLdouble[] dparams);
		[DllImport("opengl32", EntryPoint = "glGetDoublev")]
		public static extern void GetDouble(GetTarget pname, out GLdouble dparam);

		[DllImport("opengl32", EntryPoint = "glGetFloatv")]
		public static extern void GetFloat(GetTarget pname, GLfloat[] fparams);
		[DllImport("opengl32", EntryPoint = "glGetFloatv")]
		public static extern void GetFloat(GetTarget pname, out GLfloat fparam);

		[DllImport("opengl32", EntryPoint = "glGetIntegerv")]
		public static extern void GetInteger(GetTarget pname, GLint[] iparams);
		[DllImport("opengl32", EntryPoint = "glGetIntegerv")]
		public static extern void GetInteger(GetTarget pname, GLuint[] uparams);
		[DllImport("opengl32", EntryPoint = "glGetIntegerv")]
		public static extern void GetInteger(GetTarget pname, out GLint iparam);
		[DllImport("opengl32", EntryPoint = "glGetIntegerv")]
		public static extern void GetInteger(GetTarget pname, out GLuint uparam);


		#endregion
		//WINGDIAPI void APIENTRY glGetClipPlane (GLenum plane, GLdouble *equation);
		#region GLenum glGetError(void)
		// ErrorCode
		public const GLenum GL_NO_ERROR = 0x0000;
		public const GLenum GL_INVALID_ENUM = 0x0500;
		public const GLenum GL_INVALID_VALUE = 0x0501;
		public const GLenum GL_INVALID_OPERATION = 0x0502;
		public const GLenum GL_STACK_OVERFLOW = 0x0503;		// deprecated
		public const GLenum GL_STACK_UNDERFLOW = 0x0504;	// deprecated
		public const GLenum GL_OUT_OF_MEMORY = 0x0505;
		[DllImport("opengl32")]	unsafe public static extern GLenum glGetError();

		public enum ErrorCode : uint
		{
			NO_ERROR = GL_NO_ERROR,
			INVALID_ENUM = GL_INVALID_ENUM,
			INVALID_VALUE = GL_INVALID_VALUE,
			INVALID_OPERATION = GL_INVALID_OPERATION,
			STACK_OVERFLOW = GL_STACK_OVERFLOW,		// deprecated
			STACK_UNDERFLOW = GL_STACK_UNDERFLOW,	// deprecated
			OUT_OF_MEMORY = GL_OUT_OF_MEMORY,
		}
		[DllImport("opengl32", EntryPoint = "glGetError")]
		public static extern ErrorCode GetError();

		#endregion
		//WINGDIAPI GLenum APIENTRY glGetError (void);
		//WINGDIAPI void APIENTRY glGetLightfv (GLenum light, GLenum pname, GLfloat *params);
		//WINGDIAPI void APIENTRY glGetLightiv (GLenum light, GLenum pname, GLint *params);
		//WINGDIAPI void APIENTRY glGetMapdv (GLenum target, GLenum query, GLdouble *v);
		//WINGDIAPI void APIENTRY glGetMapfv (GLenum target, GLenum query, GLfloat *v);
		//WINGDIAPI void APIENTRY glGetMapiv (GLenum target, GLenum query, GLint *v);
		//WINGDIAPI void APIENTRY glGetMaterialfv (GLenum face, GLenum pname, GLfloat *params);
		//WINGDIAPI void APIENTRY glGetMaterialiv (GLenum face, GLenum pname, GLint *params);
		//WINGDIAPI void APIENTRY glGetPixelMapfv (GLenum map, GLfloat *values);
		//WINGDIAPI void APIENTRY glGetPixelMapuiv (GLenum map, GLuint *values);
		//WINGDIAPI void APIENTRY glGetPixelMapusv (GLenum map, GLushort *values);
		//WINGDIAPI void APIENTRY glGetPointerv (GLenum pname, void* *params);
		//WINGDIAPI void APIENTRY glGetPolygonStipple (GLubyte *mask);
		//WINGDIAPI const GLubyte * APIENTRY glGetString (GLenum name);
		//WINGDIAPI void APIENTRY glGetTexEnvfv (GLenum target, GLenum pname, GLfloat *params);
		//WINGDIAPI void APIENTRY glGetTexEnviv (GLenum target, GLenum pname, GLint *params);
		//WINGDIAPI void APIENTRY glGetTexGendv (GLenum coord, GLenum pname, GLdouble *params);
		//WINGDIAPI void APIENTRY glGetTexGenfv (GLenum coord, GLenum pname, GLfloat *params);
		//WINGDIAPI void APIENTRY glGetTexGeniv (GLenum coord, GLenum pname, GLint *params);
		//WINGDIAPI void APIENTRY glGetTexImage (GLenum target, GLint level, GLenum format, GLenum type, void *pixels);
		//WINGDIAPI void APIENTRY glGetTexLevelParameterfv (GLenum target, GLint level, GLenum pname, GLfloat *params);
		//WINGDIAPI void APIENTRY glGetTexLevelParameteriv (GLenum target, GLint level, GLenum pname, GLint *params);
		//WINGDIAPI void APIENTRY glGetTexParameterfv (GLenum target, GLenum pname, GLfloat *params);
		//WINGDIAPI void APIENTRY glGetTexParameteriv (GLenum target, GLenum pname, GLint *params);
		#region void glHint(GLenum target, GLenum mode)
		// HintTarget
		//GL_PERSPECTIVE_CORRECTION_HINT
		//GL_POINT_SMOOTH_HINT
		//GL_LINE_SMOOTH_HINT
		//GL_POLYGON_SMOOTH_HINT
		//GL_FOG_HINT
		//GL_PHONG_HINT
		// HintMode
		public const GLenum GL_DONT_CARE = 0x1100;
		public const GLenum GL_FASTEST = 0x1101;
		public const GLenum GL_NICEST = 0x1102;
		[DllImport("opengl32")]	unsafe public static extern void glHint(GLenum target, GLenum mode);

		public enum HintTarget : uint
		{
			PerspectiveCorrection = GL_PERSPECTIVE_CORRECTION_HINT,
			PointSmooth = GL_POINT_SMOOTH_HINT,
			LineSmooth = GL_LINE_SMOOTH_HINT,
			Fog = GL_FOG_HINT,
			GenerateMipmap = GL_GENERATE_MIPMAP_HINT
		}
		public enum HintMode : uint
		{
			DONT_CARE = GL_DONT_CARE,
			FASTEST = GL_FASTEST,
			Nicest = GL_NICEST
		}
		[DllImport("opengl32", EntryPoint = "glHint")]
		public static extern void Hint(HintTarget target, HintMode mode);

		#endregion
		//WINGDIAPI void APIENTRY glIndexMask (GLuint mask);
		//WINGDIAPI void APIENTRY glIndexPointer (GLenum type, GLsizei stride, const void *pointer);
		//WINGDIAPI void APIENTRY glIndexd (GLdouble c);
		//WINGDIAPI void APIENTRY glIndexdv (const GLdouble *c);
		//WINGDIAPI void APIENTRY glIndexf (GLfloat c);
		//WINGDIAPI void APIENTRY glIndexfv (const GLfloat *c);
		//WINGDIAPI void APIENTRY glIndexi (GLint c);
		//WINGDIAPI void APIENTRY glIndexiv (const GLint *c);
		//WINGDIAPI void APIENTRY glIndexs (GLshort c);
		//WINGDIAPI void APIENTRY glIndexsv (const GLshort *c);
		//WINGDIAPI void APIENTRY glIndexub (GLubyte c);
		//WINGDIAPI void APIENTRY glIndexubv (const GLubyte *c);
		//WINGDIAPI void APIENTRY glInitNames (void);
		//WINGDIAPI void APIENTRY glInterleavedArrays (GLenum format, GLsizei stride, const void *pointer);
		//WINGDIAPI GLboolean APIENTRY glIsEnabled (GLenum cap);
		//WINGDIAPI GLboolean APIENTRY glIsList (GLuint list);
		//WINGDIAPI GLboolean APIENTRY glIsTexture (GLuint texture);
		//WINGDIAPI void APIENTRY glLightModelf (GLenum pname, GLfloat param);
		//WINGDIAPI void APIENTRY glLightModelfv (GLenum pname, const GLfloat *params);
		//WINGDIAPI void APIENTRY glLightModeli (GLenum pname, GLint param);
		//WINGDIAPI void APIENTRY glLightModeliv (GLenum pname, const GLint *params);
		//WINGDIAPI void APIENTRY glLightf (GLenum light, GLenum pname, GLfloat param);
		//WINGDIAPI void APIENTRY glLightfv (GLenum light, GLenum pname, const GLfloat *params);
		//WINGDIAPI void APIENTRY glLighti (GLenum light, GLenum pname, GLint param);
		//WINGDIAPI void APIENTRY glLightiv (GLenum light, GLenum pname, const GLint *params);
		//WINGDIAPI void APIENTRY glLineStipple (GLint factor, GLushort pattern);
		//WINGDIAPI void APIENTRY glLineWidth (GLfloat width);
		//WINGDIAPI void APIENTRY glListBase (GLuint base);
		//WINGDIAPI void APIENTRY glLoadIdentity (void);
		//WINGDIAPI void APIENTRY glLoadMatrixd (const GLdouble *m);
		//WINGDIAPI void APIENTRY glLoadMatrixf (const GLfloat *m);
		//WINGDIAPI void APIENTRY glLoadName (GLuint name);
		//WINGDIAPI void APIENTRY glLogicOp (GLenum opcode);
		//WINGDIAPI void APIENTRY glMap1d (GLenum target, GLdouble u1, GLdouble u2, GLint stride, GLint order, const GLdouble *points);
		//WINGDIAPI void APIENTRY glMap1f (GLenum target, GLfloat u1, GLfloat u2, GLint stride, GLint order, const GLfloat *points);
		//WINGDIAPI void APIENTRY glMap2d (GLenum target, GLdouble u1, GLdouble u2, GLint ustride, GLint uorder, GLdouble v1, GLdouble v2, GLint vstride, GLint vorder, const GLdouble *points);
		//WINGDIAPI void APIENTRY glMap2f (GLenum target, GLfloat u1, GLfloat u2, GLint ustride, GLint uorder, GLfloat v1, GLfloat v2, GLint vstride, GLint vorder, const GLfloat *points);
		//WINGDIAPI void APIENTRY glMapGrid1d (GLint un, GLdouble u1, GLdouble u2);
		//WINGDIAPI void APIENTRY glMapGrid1f (GLint un, GLfloat u1, GLfloat u2);
		//WINGDIAPI void APIENTRY glMapGrid2d (GLint un, GLdouble u1, GLdouble u2, GLint vn, GLdouble v1, GLdouble v2);
		//WINGDIAPI void APIENTRY glMapGrid2f (GLint un, GLfloat u1, GLfloat u2, GLint vn, GLfloat v1, GLfloat v2);
		//WINGDIAPI void APIENTRY glMaterialf (GLenum face, GLenum pname, GLfloat param);
		//WINGDIAPI void APIENTRY glMaterialfv (GLenum face, GLenum pname, const GLfloat *params);
		//WINGDIAPI void APIENTRY glMateriali (GLenum face, GLenum pname, GLint param);
		//WINGDIAPI void APIENTRY glMaterialiv (GLenum face, GLenum pname, const GLint *params);
		//WINGDIAPI void APIENTRY glMatrixMode (GLenum mode);
		//WINGDIAPI void APIENTRY glMultMatrixd (const GLdouble *m);
		//WINGDIAPI void APIENTRY glMultMatrixf (const GLfloat *m);
		//WINGDIAPI void APIENTRY glNewList (GLuint list, GLenum mode);
		//WINGDIAPI void APIENTRY glNormal3b (GLbyte nx, GLbyte ny, GLbyte nz);
		//WINGDIAPI void APIENTRY glNormal3bv (const GLbyte *v);
		//WINGDIAPI void APIENTRY glNormal3d (GLdouble nx, GLdouble ny, GLdouble nz);
		//WINGDIAPI void APIENTRY glNormal3dv (const GLdouble *v);
		//WINGDIAPI void APIENTRY glNormal3f (GLfloat nx, GLfloat ny, GLfloat nz);
		//WINGDIAPI void APIENTRY glNormal3fv (const GLfloat *v);
		//WINGDIAPI void APIENTRY glNormal3i (GLint nx, GLint ny, GLint nz);
		//WINGDIAPI void APIENTRY glNormal3iv (const GLint *v);
		//WINGDIAPI void APIENTRY glNormal3s (GLshort nx, GLshort ny, GLshort nz);
		//WINGDIAPI void APIENTRY glNormal3sv (const GLshort *v);
		//WINGDIAPI void APIENTRY glNormalPointer (GLenum type, GLsizei stride, const void *pointer);
		//WINGDIAPI void APIENTRY glOrtho (GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar);
		//WINGDIAPI void APIENTRY glPassThrough (GLfloat token);
		//WINGDIAPI void APIENTRY glPixelMapfv (GLenum map, GLsizei mapsize, const GLfloat *values);
		//WINGDIAPI void APIENTRY glPixelMapuiv (GLenum map, GLsizei mapsize, const GLuint *values);
		//WINGDIAPI void APIENTRY glPixelMapusv (GLenum map, GLsizei mapsize, const GLushort *values);
		//WINGDIAPI void APIENTRY glPixelStoref (GLenum pname, GLfloat param);
		//WINGDIAPI void APIENTRY glPixelStorei (GLenum pname, GLint param);
		//WINGDIAPI void APIENTRY glPixelTransferf (GLenum pname, GLfloat param);
		//WINGDIAPI void APIENTRY glPixelTransferi (GLenum pname, GLint param);
		//WINGDIAPI void APIENTRY glPixelZoom (GLfloat xfactor, GLfloat yfactor);
		//WINGDIAPI void APIENTRY glPointSize (GLfloat size);
		//WINGDIAPI void APIENTRY glPolygonMode (GLenum face, GLenum mode);
		//WINGDIAPI void APIENTRY glPolygonOffset (GLfloat factor, GLfloat units);
		//WINGDIAPI void APIENTRY glPolygonStipple (const GLubyte *mask);
		//WINGDIAPI void APIENTRY glPopAttrib (void);
		//WINGDIAPI void APIENTRY glPopClientAttrib (void);
		//WINGDIAPI void APIENTRY glPopMatrix (void);
		//WINGDIAPI void APIENTRY glPopName (void);
		//WINGDIAPI void APIENTRY glPrioritizeTextures (GLsizei n, const GLuint *textures, const GLclampf *priorities);
		//WINGDIAPI void APIENTRY glPushAttrib (GLbitfield mask);
		//WINGDIAPI void APIENTRY glPushClientAttrib (GLbitfield mask);
		//WINGDIAPI void APIENTRY glPushMatrix (void);
		//WINGDIAPI void APIENTRY glPushName (GLuint name);
		//WINGDIAPI void APIENTRY glRasterPos2d (GLdouble x, GLdouble y);
		//WINGDIAPI void APIENTRY glRasterPos2dv (const GLdouble *v);
		//WINGDIAPI void APIENTRY glRasterPos2f (GLfloat x, GLfloat y);
		//WINGDIAPI void APIENTRY glRasterPos2fv (const GLfloat *v);
		//WINGDIAPI void APIENTRY glRasterPos2i (GLint x, GLint y);
		//WINGDIAPI void APIENTRY glRasterPos2iv (const GLint *v);
		//WINGDIAPI void APIENTRY glRasterPos2s (GLshort x, GLshort y);
		//WINGDIAPI void APIENTRY glRasterPos2sv (const GLshort *v);
		//WINGDIAPI void APIENTRY glRasterPos3d (GLdouble x, GLdouble y, GLdouble z);
		//WINGDIAPI void APIENTRY glRasterPos3dv (const GLdouble *v);
		//WINGDIAPI void APIENTRY glRasterPos3f (GLfloat x, GLfloat y, GLfloat z);
		//WINGDIAPI void APIENTRY glRasterPos3fv (const GLfloat *v);
		//WINGDIAPI void APIENTRY glRasterPos3i (GLint x, GLint y, GLint z);
		//WINGDIAPI void APIENTRY glRasterPos3iv (const GLint *v);
		//WINGDIAPI void APIENTRY glRasterPos3s (GLshort x, GLshort y, GLshort z);
		//WINGDIAPI void APIENTRY glRasterPos3sv (const GLshort *v);
		//WINGDIAPI void APIENTRY glRasterPos4d (GLdouble x, GLdouble y, GLdouble z, GLdouble w);
		//WINGDIAPI void APIENTRY glRasterPos4dv (const GLdouble *v);
		//WINGDIAPI void APIENTRY glRasterPos4f (GLfloat x, GLfloat y, GLfloat z, GLfloat w);
		//WINGDIAPI void APIENTRY glRasterPos4fv (const GLfloat *v);
		//WINGDIAPI void APIENTRY glRasterPos4i (GLint x, GLint y, GLint z, GLint w);
		//WINGDIAPI void APIENTRY glRasterPos4iv (const GLint *v);
		//WINGDIAPI void APIENTRY glRasterPos4s (GLshort x, GLshort y, GLshort z, GLshort w);
		//WINGDIAPI void APIENTRY glRasterPos4sv (const GLshort *v);
		//WINGDIAPI void APIENTRY glReadBuffer (GLenum mode);
		//WINGDIAPI void APIENTRY glReadPixels (GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, void *pixels);
		//WINGDIAPI void APIENTRY glRectd (GLdouble x1, GLdouble y1, GLdouble x2, GLdouble y2);
		//WINGDIAPI void APIENTRY glRectdv (const GLdouble *v1, const GLdouble *v2);
		//WINGDIAPI void APIENTRY glRectf (GLfloat x1, GLfloat y1, GLfloat x2, GLfloat y2);
		//WINGDIAPI void APIENTRY glRectfv (const GLfloat *v1, const GLfloat *v2);
		//WINGDIAPI void APIENTRY glRecti (GLint x1, GLint y1, GLint x2, GLint y2);
		//WINGDIAPI void APIENTRY glRectiv (const GLint *v1, const GLint *v2);
		//WINGDIAPI void APIENTRY glRects (GLshort x1, GLshort y1, GLshort x2, GLshort y2);
		//WINGDIAPI void APIENTRY glRectsv (const GLshort *v1, const GLshort *v2);
		//WINGDIAPI GLint APIENTRY glRenderMode (GLenum mode);
		//WINGDIAPI void APIENTRY glRotated (GLdouble angle, GLdouble x, GLdouble y, GLdouble z);
		//WINGDIAPI void APIENTRY glRotatef (GLfloat angle, GLfloat x, GLfloat y, GLfloat z);
		//WINGDIAPI void APIENTRY glScaled (GLdouble x, GLdouble y, GLdouble z);
		//WINGDIAPI void APIENTRY glScalef (GLfloat x, GLfloat y, GLfloat z);
		//WINGDIAPI void APIENTRY glScissor (GLint x, GLint y, GLsizei width, GLsizei height);
		//WINGDIAPI void APIENTRY glSelectBuffer (GLsizei size, GLuint *buffer);
		//WINGDIAPI void APIENTRY glShadeModel (GLenum mode);
		//WINGDIAPI void APIENTRY glStencilFunc (GLenum func, GLint ref, GLuint mask);
		//WINGDIAPI void APIENTRY glStencilMask (GLuint mask);
		//WINGDIAPI void APIENTRY glStencilOp (GLenum fail, GLenum zfail, GLenum zpass);
		//WINGDIAPI void APIENTRY glTexCoord1d (GLdouble s);
		//WINGDIAPI void APIENTRY glTexCoord1dv (const GLdouble *v);
		//WINGDIAPI void APIENTRY glTexCoord1f (GLfloat s);
		//WINGDIAPI void APIENTRY glTexCoord1fv (const GLfloat *v);
		//WINGDIAPI void APIENTRY glTexCoord1i (GLint s);
		//WINGDIAPI void APIENTRY glTexCoord1iv (const GLint *v);
		//WINGDIAPI void APIENTRY glTexCoord1s (GLshort s);
		//WINGDIAPI void APIENTRY glTexCoord1sv (const GLshort *v);
		//WINGDIAPI void APIENTRY glTexCoord2d (GLdouble s, GLdouble t);
		//WINGDIAPI void APIENTRY glTexCoord2dv (const GLdouble *v);
		//WINGDIAPI void APIENTRY glTexCoord2f (GLfloat s, GLfloat t);
		//WINGDIAPI void APIENTRY glTexCoord2fv (const GLfloat *v);
		//WINGDIAPI void APIENTRY glTexCoord2i (GLint s, GLint t);
		//WINGDIAPI void APIENTRY glTexCoord2iv (const GLint *v);
		//WINGDIAPI void APIENTRY glTexCoord2s (GLshort s, GLshort t);
		//WINGDIAPI void APIENTRY glTexCoord2sv (const GLshort *v);
		//WINGDIAPI void APIENTRY glTexCoord3d (GLdouble s, GLdouble t, GLdouble r);
		//WINGDIAPI void APIENTRY glTexCoord3dv (const GLdouble *v);
		//WINGDIAPI void APIENTRY glTexCoord3f (GLfloat s, GLfloat t, GLfloat r);
		//WINGDIAPI void APIENTRY glTexCoord3fv (const GLfloat *v);
		//WINGDIAPI void APIENTRY glTexCoord3i (GLint s, GLint t, GLint r);
		//WINGDIAPI void APIENTRY glTexCoord3iv (const GLint *v);
		//WINGDIAPI void APIENTRY glTexCoord3s (GLshort s, GLshort t, GLshort r);
		//WINGDIAPI void APIENTRY glTexCoord3sv (const GLshort *v);
		//WINGDIAPI void APIENTRY glTexCoord4d (GLdouble s, GLdouble t, GLdouble r, GLdouble q);
		//WINGDIAPI void APIENTRY glTexCoord4dv (const GLdouble *v);
		//WINGDIAPI void APIENTRY glTexCoord4f (GLfloat s, GLfloat t, GLfloat r, GLfloat q);
		//WINGDIAPI void APIENTRY glTexCoord4fv (const GLfloat *v);
		//WINGDIAPI void APIENTRY glTexCoord4i (GLint s, GLint t, GLint r, GLint q);
		//WINGDIAPI void APIENTRY glTexCoord4iv (const GLint *v);
		//WINGDIAPI void APIENTRY glTexCoord4s (GLshort s, GLshort t, GLshort r, GLshort q);
		//WINGDIAPI void APIENTRY glTexCoord4sv (const GLshort *v);
		//WINGDIAPI void APIENTRY glTexCoordPointer (GLint size, GLenum type, GLsizei stride, const void *pointer);
		//WINGDIAPI void APIENTRY glTexEnvf (GLenum target, GLenum pname, GLfloat param);
		//WINGDIAPI void APIENTRY glTexEnvfv (GLenum target, GLenum pname, const GLfloat *params);
		//WINGDIAPI void APIENTRY glTexEnvi (GLenum target, GLenum pname, GLint param);
		//WINGDIAPI void APIENTRY glTexEnviv (GLenum target, GLenum pname, const GLint *params);
		//WINGDIAPI void APIENTRY glTexGend (GLenum coord, GLenum pname, GLdouble param);
		//WINGDIAPI void APIENTRY glTexGendv (GLenum coord, GLenum pname, const GLdouble *params);
		//WINGDIAPI void APIENTRY glTexGenf (GLenum coord, GLenum pname, GLfloat param);
		//WINGDIAPI void APIENTRY glTexGenfv (GLenum coord, GLenum pname, const GLfloat *params);
		//WINGDIAPI void APIENTRY glTexGeni (GLenum coord, GLenum pname, GLint param);
		//WINGDIAPI void APIENTRY glTexGeniv (GLenum coord, GLenum pname, const GLint *params);
		#region void glTexImage...
		// TextureTarget
		//	GL_TEXTURE_1D
		//	GL_TEXTURE_2D
		//	GL_PROXY_TEXTURE_1D
		//	GL_PROXY_TEXTURE_2D
		// texture
		//	GL_ALPHA4
		//	GL_ALPHA8
		//	GL_ALPHA12
		//	GL_ALPHA16
		//	...
		//	GL_PROXY_TEXTURE_1D
		//	GL_PROXY_TEXTURE_2D
		// HintMode
		[DllImport("opengl32")]
		unsafe public static extern void glTexImage1D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLint border, GLenum format, GLenum type, void *pixels);
		[DllImport("opengl32")]
		unsafe public static extern void glTexImage2D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, void* pixels);

		public enum TextureTarget : uint
		{
			Texture1D = GL_TEXTURE_1D,
			Texture2D = GL_TEXTURE_2D,
			// ...
		}
		// ...
		[DllImport("opengl32", EntryPoint = "glHint")]
		public static extern void TexImage1D(TextureTarget target, GLint level, GLint internalformat, GLsizei width, GLint border, GLenum format, GLenum type, byte[] pixels);
		[DllImport("opengl32", EntryPoint = "glHint")]
		public static extern void TexImage2D(TextureTarget target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, byte[] pixels);

		#endregion
		//WINGDIAPI void APIENTRY glTexParameterf (GLenum target, GLenum pname, GLfloat param);
		//WINGDIAPI void APIENTRY glTexParameterfv (GLenum target, GLenum pname, const GLfloat *params);
		//WINGDIAPI void APIENTRY glTexParameteri (GLenum target, GLenum pname, GLint param);
		//WINGDIAPI void APIENTRY glTexParameteriv (GLenum target, GLenum pname, const GLint *params);
		//WINGDIAPI void APIENTRY glTexSubImage1D (GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type, const void *pixels);
		//WINGDIAPI void APIENTRY glTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const void *pixels);
		#region void glTranslate(GL<type> x, GL<type> y, GL<type> z)

		[DllImport("opengl32")]	unsafe public static extern void glTranslated(GLdouble x, GLdouble y, GLdouble z);
		[DllImport("opengl32")]	unsafe public static extern void glTranslatef(GLfloat x, GLfloat y, GLfloat z);

		[DllImport("opengl32.dll", EntryPoint = "glTranslated")]
		public static extern void Translate(double x, double y, double z);
		[DllImport("opengl32.dll", EntryPoint = "glTranslatef")]
		public static extern void Translate(float x, float y, float z);

		#endregion

		//WINGDIAPI void APIENTRY glVertex2d (GLdouble x, GLdouble y);
		//WINGDIAPI void APIENTRY glVertex2dv (const GLdouble *v);
		//WINGDIAPI void APIENTRY glVertex2f (GLfloat x, GLfloat y);
		//WINGDIAPI void APIENTRY glVertex2fv (const GLfloat *v);
		//WINGDIAPI void APIENTRY glVertex2i (GLint x, GLint y);
		//WINGDIAPI void APIENTRY glVertex2iv (const GLint *v);
		//WINGDIAPI void APIENTRY glVertex2s (GLshort x, GLshort y);
		//WINGDIAPI void APIENTRY glVertex2sv (const GLshort *v);
		//WINGDIAPI void APIENTRY glVertex3d (GLdouble x, GLdouble y, GLdouble z);
		//WINGDIAPI void APIENTRY glVertex3dv (const GLdouble *v);
		//WINGDIAPI void APIENTRY glVertex3f (GLfloat x, GLfloat y, GLfloat z);
		//WINGDIAPI void APIENTRY glVertex3fv (const GLfloat *v);
		//WINGDIAPI void APIENTRY glVertex3i (GLint x, GLint y, GLint z);
		//WINGDIAPI void APIENTRY glVertex3iv (const GLint *v);
		//WINGDIAPI void APIENTRY glVertex3s (GLshort x, GLshort y, GLshort z);
		//WINGDIAPI void APIENTRY glVertex3sv (const GLshort *v);
		//WINGDIAPI void APIENTRY glVertex4d (GLdouble x, GLdouble y, GLdouble z, GLdouble w);
		//WINGDIAPI void APIENTRY glVertex4dv (const GLdouble *v);
		//WINGDIAPI void APIENTRY glVertex4f (GLfloat x, GLfloat y, GLfloat z, GLfloat w);
		//WINGDIAPI void APIENTRY glVertex4fv (const GLfloat *v);
		//WINGDIAPI void APIENTRY glVertex4i (GLint x, GLint y, GLint z, GLint w);
		//WINGDIAPI void APIENTRY glVertex4iv (const GLint *v);
		//WINGDIAPI void APIENTRY glVertex4s (GLshort x, GLshort y, GLshort z, GLshort w);
		//WINGDIAPI void APIENTRY glVertex4sv (const GLshort *v);
		//WINGDIAPI void APIENTRY glVertexPointer (GLint size, GLenum type, GLsizei stride, const void *pointer);
		#region void glViewport(GLint x, GLint y, GLsizei width, GLsizei height)
		[DllImport("opengl32")]	unsafe public static extern void glViewport(GLint x, GLint y, GLsizei width, GLsizei height);

		[DllImport("opengl32.dll", EntryPoint = "glViewport")]
		public static extern void Viewport(GLint x, GLint y, GLsizei width, GLsizei height);

		#endregion

		/*
GLAPI void APIENTRY glLineWidth (GLfloat width);
GLAPI void APIENTRY glPointSize (GLfloat size);
GLAPI void APIENTRY glPolygonMode (GLenum face, GLenum mode);
GLAPI void APIENTRY glScissor (GLint x, GLint y, GLsizei width, GLsizei height);
GLAPI void APIENTRY glTexParameterf (GLenum target, GLenum pname, GLfloat param);
GLAPI void APIENTRY glTexParameterfv (GLenum target, GLenum pname, const GLfloat *params);
GLAPI void APIENTRY glTexParameteri (GLenum target, GLenum pname, GLint param);
GLAPI void APIENTRY glTexParameteriv (GLenum target, GLenum pname, const GLint *params);
GLAPI void APIENTRY glTexImage1D (GLenum target, GLint level, GLint internalformat, GLsizei width, GLint border, GLenum format, GLenum type, const void *pixels);
GLAPI void APIENTRY glTexImage2D (GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const void *pixels);
GLAPI void APIENTRY glDrawBuffer (GLenum mode);
GLAPI void APIENTRY glClear (GLbitfield mask);
GLAPI void APIENTRY glClearColor (GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
GLAPI void APIENTRY glClearStencil (GLint s);
GLAPI void APIENTRY glClearDepth (GLclampd depth);
GLAPI void APIENTRY glStencilMask (GLuint mask);
GLAPI void APIENTRY glColorMask (GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);
GLAPI void APIENTRY glDepthMask (GLboolean flag);
GLAPI void APIENTRY glDisable (GLenum cap);
GLAPI void APIENTRY glEnable (GLenum cap);
GLAPI void APIENTRY glFinish (void);
GLAPI void APIENTRY glFlush (void);
GLAPI void APIENTRY glBlendFunc (GLenum sfactor, GLenum dfactor);
GLAPI void APIENTRY glLogicOp (GLenum opcode);
GLAPI void APIENTRY glStencilFunc (GLenum func, GLint ref, GLuint mask);
GLAPI void APIENTRY glStencilOp (GLenum fail, GLenum zfail, GLenum zpass);
GLAPI void APIENTRY glDepthFunc (GLenum func);
GLAPI void APIENTRY glPixelStoref (GLenum pname, GLfloat param);
GLAPI void APIENTRY glPixelStorei (GLenum pname, GLint param);
GLAPI void APIENTRY glReadBuffer (GLenum mode);
GLAPI void APIENTRY glReadPixels (GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, void *pixels);
GLAPI void APIENTRY glGetBooleanv (GLenum pname, GLboolean *params);
GLAPI void APIENTRY glGetDoublev (GLenum pname, GLdouble *params);
GLAPI GLenum APIENTRY glGetError (void);
GLAPI void APIENTRY glGetFloatv (GLenum pname, GLfloat *params);
GLAPI void APIENTRY glGetIntegerv (GLenum pname, GLint *params);
GLAPI const GLubyte * APIENTRY glGetString (GLenum name);
GLAPI void APIENTRY glGetTexImage (GLenum target, GLint level, GLenum format, GLenum type, void *pixels);
GLAPI void APIENTRY glGetTexParameterfv (GLenum target, GLenum pname, GLfloat *params);
GLAPI void APIENTRY glGetTexParameteriv (GLenum target, GLenum pname, GLint *params);
GLAPI void APIENTRY glGetTexLevelParameterfv (GLenum target, GLint level, GLenum pname, GLfloat *params);
GLAPI void APIENTRY glGetTexLevelParameteriv (GLenum target, GLint level, GLenum pname, GLint *params);
GLAPI GLboolean APIENTRY glIsEnabled (GLenum cap);
GLAPI void APIENTRY glDepthRange (GLclampd near, GLclampd far);
GLAPI void APIENTRY glViewport (GLint x, GLint y, GLsizei width, GLsizei height);
*/
	}

	/// <summary>
	/// GL_VERSION_1_1
	/// </summary>
	[GL_VERSION_1_1]
	public partial class OpenGL
	{
		class GL_VERSION_1_1 : OpenGLExtension {
			public GL_VERSION_1_1() : base("GL_VERSION_1_1") { }
		}

		[DllImport("opengl32")]	public static unsafe extern void glDrawArrays(GLenum mode, GLint first, GLsizei count);
		[DllImport("opengl32")]	public static unsafe extern void glDeleteTextures(GLsizei n, GLuint *textures);
		/*
GLAPI void APIENTRY glDrawElements (GLenum mode, GLsizei count, GLenum type, const void *indices);
GLAPI void APIENTRY glGetPointerv (GLenum pname, void* *params);
GLAPI void APIENTRY glPolygonOffset (GLfloat factor, GLfloat units);
GLAPI void APIENTRY glCopyTexImage1D (GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLint border);
GLAPI void APIENTRY glCopyTexImage2D (GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border);
GLAPI void APIENTRY glCopyTexSubImage1D (GLenum target, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width);
GLAPI void APIENTRY glCopyTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height);
GLAPI void APIENTRY glTexSubImage1D (GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type, const void *pixels);
GLAPI void APIENTRY glTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const void *pixels);
GLAPI void APIENTRY glBindTexture (GLenum target, GLuint texture);
GLAPI void APIENTRY glDeleteTextures (GLsizei n, const GLuint *textures);
GLAPI void APIENTRY glGenTextures (GLsizei n, GLuint *textures);
GLAPI GLboolean APIENTRY glIsTexture (GLuint texture);
		*/
	}

	/* Отсюда начинаются функции, которые недоступны прямым импортом из opengl32.dll */

	/// <summary>
	/// GL_VERSION_1_2
	/// </summary>
	[GL_VERSION_1_2]
	public partial class OpenGL
	{
		class GL_VERSION_1_2 : OpenGLExtension {
			public GL_VERSION_1_2() : base("GL_VERSION_1_2") { }
		}

		//#define GL_UNSIGNED_BYTE_3_3_2            0x8032
		//#define GL_UNSIGNED_SHORT_4_4_4_4         0x8033
		//#define GL_UNSIGNED_SHORT_5_5_5_1         0x8034
		//#define GL_UNSIGNED_INT_8_8_8_8           0x8035
		//#define GL_UNSIGNED_INT_10_10_10_2        0x8036
		//#define GL_TEXTURE_BINDING_3D             0x806A
		//#define GL_PACK_SKIP_IMAGES               0x806B
		//#define GL_PACK_IMAGE_HEIGHT              0x806C
		//#define GL_UNPACK_SKIP_IMAGES             0x806D
		//#define GL_UNPACK_IMAGE_HEIGHT            0x806E
		public const GLenum GL_TEXTURE_3D = 0x806F;
		//#define GL_PROXY_TEXTURE_3D               0x8070
		//#define GL_TEXTURE_DEPTH                  0x8071
		public const GLenum GL_TEXTURE_WRAP_R = 0x8072;
		//#define GL_MAX_3D_TEXTURE_SIZE            0x8073
		//#define GL_UNSIGNED_BYTE_2_3_3_REV        0x8362
		//#define GL_UNSIGNED_SHORT_5_6_5           0x8363
		//#define GL_UNSIGNED_SHORT_5_6_5_REV       0x8364
		//#define GL_UNSIGNED_SHORT_4_4_4_4_REV     0x8365
		//#define GL_UNSIGNED_SHORT_1_5_5_5_REV     0x8366
		//#define GL_UNSIGNED_INT_8_8_8_8_REV       0x8367
		//#define GL_UNSIGNED_INT_2_10_10_10_REV    0x8368
		//#define GL_BGR                            0x80E0
		public const GLenum GL_BGRA = 0x80E1;
		//#define GL_MAX_ELEMENTS_VERTICES          0x80E8
		//#define GL_MAX_ELEMENTS_INDICES           0x80E9
		public const GLenum GL_CLAMP_TO_EDGE = 0x812F;
		//#define GL_TEXTURE_MIN_LOD                0x813A
		//#define GL_TEXTURE_MAX_LOD                0x813B
		//#define GL_TEXTURE_BASE_LEVEL             0x813C
		//#define GL_TEXTURE_MAX_LEVEL              0x813D
		//#define GL_SMOOTH_POINT_SIZE_RANGE        0x0B12
		//#define GL_SMOOTH_POINT_SIZE_GRANULARITY  0x0B13
		//#define GL_SMOOTH_LINE_WIDTH_RANGE        0x0B22
		//#define GL_SMOOTH_LINE_WIDTH_GRANULARITY  0x0B23
		//#define GL_ALIASED_LINE_WIDTH_RANGE       0x846E

//GLAPI void APIENTRY glBlendColor (GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
//GLAPI void APIENTRY glBlendEquation (GLenum mode);
//GLAPI void APIENTRY glDrawRangeElements (GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type, const void *indices);
		[GL_VERSION_1_2]	public static PFNGLTEXIMAGE3DPROC glTexImage3D;
//GLAPI void APIENTRY glTexSubImage3D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const void *pixels);
//GLAPI void APIENTRY glCopyTexSubImage3D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height);

//typedef void (APIENTRYP PFNGLBLENDCOLORPROC) (GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
//typedef void (APIENTRYP PFNGLBLENDEQUATIONPROC) (GLenum mode);
//typedef void (APIENTRYP PFNGLDRAWRANGEELEMENTSPROC) (GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type, const void *indices);
		[GL_VERSION_1_2]	unsafe public delegate void PFNGLTEXIMAGE3DPROC(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, void *pixels);
//typedef void (APIENTRYP PFNGLTEXSUBIMAGE3DPROC) (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const void *pixels);
//typedef void (APIENTRYP PFNGLCOPYTEXSUBIMAGE3DPROC) (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height);
	}

	/// <summary>
	/// GL_VERSION_1_2_DEPRECATED
	/// </summary>
	[GL_VERSION_1_2_DEPRECATED]
	public partial class OpenGL
	{
		class GL_VERSION_1_2_DEPRECATED : OpenGLExtension {
			public GL_VERSION_1_2_DEPRECATED() : base("GL_VERSION_1_2_DEPRECATED") { }
		}
		/*
typedef void (APIENTRYP PFNGLCOLORTABLEPROC) (GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type, const void *table);
typedef void (APIENTRYP PFNGLCOLORTABLEPARAMETERFVPROC) (GLenum target, GLenum pname, const GLfloat *params);
typedef void (APIENTRYP PFNGLCOLORTABLEPARAMETERIVPROC) (GLenum target, GLenum pname, const GLint *params);
typedef void (APIENTRYP PFNGLCOPYCOLORTABLEPROC) (GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width);
typedef void (APIENTRYP PFNGLGETCOLORTABLEPROC) (GLenum target, GLenum format, GLenum type, void *table);
typedef void (APIENTRYP PFNGLGETCOLORTABLEPARAMETERFVPROC) (GLenum target, GLenum pname, GLfloat *params);
typedef void (APIENTRYP PFNGLGETCOLORTABLEPARAMETERIVPROC) (GLenum target, GLenum pname, GLint *params);
typedef void (APIENTRYP PFNGLCOLORSUBTABLEPROC) (GLenum target, GLsizei start, GLsizei count, GLenum format, GLenum type, const void *data);
typedef void (APIENTRYP PFNGLCOPYCOLORSUBTABLEPROC) (GLenum target, GLsizei start, GLint x, GLint y, GLsizei width);
typedef void (APIENTRYP PFNGLCONVOLUTIONFILTER1DPROC) (GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type, const void *image);
typedef void (APIENTRYP PFNGLCONVOLUTIONFILTER2DPROC) (GLenum target, GLenum internalformat, GLsizei width, GLsizei height, GLenum format, GLenum type, const void *image);
typedef void (APIENTRYP PFNGLCONVOLUTIONPARAMETERFPROC) (GLenum target, GLenum pname, GLfloat params);
typedef void (APIENTRYP PFNGLCONVOLUTIONPARAMETERFVPROC) (GLenum target, GLenum pname, const GLfloat *params);
typedef void (APIENTRYP PFNGLCONVOLUTIONPARAMETERIPROC) (GLenum target, GLenum pname, GLint params);
typedef void (APIENTRYP PFNGLCONVOLUTIONPARAMETERIVPROC) (GLenum target, GLenum pname, const GLint *params);
typedef void (APIENTRYP PFNGLCOPYCONVOLUTIONFILTER1DPROC) (GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width);
typedef void (APIENTRYP PFNGLCOPYCONVOLUTIONFILTER2DPROC) (GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height);
typedef void (APIENTRYP PFNGLGETCONVOLUTIONFILTERPROC) (GLenum target, GLenum format, GLenum type, void *image);
typedef void (APIENTRYP PFNGLGETCONVOLUTIONPARAMETERFVPROC) (GLenum target, GLenum pname, GLfloat *params);
typedef void (APIENTRYP PFNGLGETCONVOLUTIONPARAMETERIVPROC) (GLenum target, GLenum pname, GLint *params);
typedef void (APIENTRYP PFNGLGETSEPARABLEFILTERPROC) (GLenum target, GLenum format, GLenum type, void *row, void *column, void *span);
typedef void (APIENTRYP PFNGLSEPARABLEFILTER2DPROC) (GLenum target, GLenum internalformat, GLsizei width, GLsizei height, GLenum format, GLenum type, const void *row, const void *column);
typedef void (APIENTRYP PFNGLGETHISTOGRAMPROC) (GLenum target, GLboolean reset, GLenum format, GLenum type, void *values);
typedef void (APIENTRYP PFNGLGETHISTOGRAMPARAMETERFVPROC) (GLenum target, GLenum pname, GLfloat *params);
typedef void (APIENTRYP PFNGLGETHISTOGRAMPARAMETERIVPROC) (GLenum target, GLenum pname, GLint *params);
typedef void (APIENTRYP PFNGLGETMINMAXPROC) (GLenum target, GLboolean reset, GLenum format, GLenum type, void *values);
typedef void (APIENTRYP PFNGLGETMINMAXPARAMETERFVPROC) (GLenum target, GLenum pname, GLfloat *params);
typedef void (APIENTRYP PFNGLGETMINMAXPARAMETERIVPROC) (GLenum target, GLenum pname, GLint *params);
typedef void (APIENTRYP PFNGLHISTOGRAMPROC) (GLenum target, GLsizei width, GLenum internalformat, GLboolean sink);
typedef void (APIENTRYP PFNGLMINMAXPROC) (GLenum target, GLenum internalformat, GLboolean sink);
typedef void (APIENTRYP PFNGLRESETHISTOGRAMPROC) (GLenum target);
typedef void (APIENTRYP PFNGLRESETMINMAXPROC) (GLenum target);

GLAPI void APIENTRY glColorTable (GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type, const void *table);
GLAPI void APIENTRY glColorTableParameterfv (GLenum target, GLenum pname, const GLfloat *params);
GLAPI void APIENTRY glColorTableParameteriv (GLenum target, GLenum pname, const GLint *params);
GLAPI void APIENTRY glCopyColorTable (GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width);
GLAPI void APIENTRY glGetColorTable (GLenum target, GLenum format, GLenum type, void *table);
GLAPI void APIENTRY glGetColorTableParameterfv (GLenum target, GLenum pname, GLfloat *params);
GLAPI void APIENTRY glGetColorTableParameteriv (GLenum target, GLenum pname, GLint *params);
GLAPI void APIENTRY glColorSubTable (GLenum target, GLsizei start, GLsizei count, GLenum format, GLenum type, const void *data);
GLAPI void APIENTRY glCopyColorSubTable (GLenum target, GLsizei start, GLint x, GLint y, GLsizei width);
GLAPI void APIENTRY glConvolutionFilter1D (GLenum target, GLenum internalformat, GLsizei width, GLenum format, GLenum type, const void *image);
GLAPI void APIENTRY glConvolutionFilter2D (GLenum target, GLenum internalformat, GLsizei width, GLsizei height, GLenum format, GLenum type, const void *image);
GLAPI void APIENTRY glConvolutionParameterf (GLenum target, GLenum pname, GLfloat params);
GLAPI void APIENTRY glConvolutionParameterfv (GLenum target, GLenum pname, const GLfloat *params);
GLAPI void APIENTRY glConvolutionParameteri (GLenum target, GLenum pname, GLint params);
GLAPI void APIENTRY glConvolutionParameteriv (GLenum target, GLenum pname, const GLint *params);
GLAPI void APIENTRY glCopyConvolutionFilter1D (GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width);
GLAPI void APIENTRY glCopyConvolutionFilter2D (GLenum target, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height);
GLAPI void APIENTRY glGetConvolutionFilter (GLenum target, GLenum format, GLenum type, void *image);
GLAPI void APIENTRY glGetConvolutionParameterfv (GLenum target, GLenum pname, GLfloat *params);
GLAPI void APIENTRY glGetConvolutionParameteriv (GLenum target, GLenum pname, GLint *params);
GLAPI void APIENTRY glGetSeparableFilter (GLenum target, GLenum format, GLenum type, void *row, void *column, void *span);
GLAPI void APIENTRY glSeparableFilter2D (GLenum target, GLenum internalformat, GLsizei width, GLsizei height, GLenum format, GLenum type, const void *row, const void *column);
GLAPI void APIENTRY glGetHistogram (GLenum target, GLboolean reset, GLenum format, GLenum type, void *values);
GLAPI void APIENTRY glGetHistogramParameterfv (GLenum target, GLenum pname, GLfloat *params);
GLAPI void APIENTRY glGetHistogramParameteriv (GLenum target, GLenum pname, GLint *params);
GLAPI void APIENTRY glGetMinmax (GLenum target, GLboolean reset, GLenum format, GLenum type, void *values);
GLAPI void APIENTRY glGetMinmaxParameterfv (GLenum target, GLenum pname, GLfloat *params);
GLAPI void APIENTRY glGetMinmaxParameteriv (GLenum target, GLenum pname, GLint *params);
GLAPI void APIENTRY glHistogram (GLenum target, GLsizei width, GLenum internalformat, GLboolean sink);
GLAPI void APIENTRY glMinmax (GLenum target, GLenum internalformat, GLboolean sink);
GLAPI void APIENTRY glResetHistogram (GLenum target);
GLAPI void APIENTRY glResetMinmax (GLenum target);
		*/
	}

	/// <summary>
	/// GL_VERSION_1_3
	/// </summary>
	[GL_VERSION_1_3]
	public partial class OpenGL
	{
		class GL_VERSION_1_3 : OpenGLExtension {
			public GL_VERSION_1_3() : base("GL_VERSION_1_3") { }
		}

		public const GLenum GL_TEXTURE0 = 0x84C0;
		public const GLenum GL_TEXTURE1 = 0x84C1;
		public const GLenum GL_TEXTURE2 = 0x84C2;
		public const GLenum GL_TEXTURE3 = 0x84C3;
		public const GLenum GL_TEXTURE4 = 0x84C4;
		public const GLenum GL_TEXTURE5 = 0x84C5;
		public const GLenum GL_TEXTURE6 = 0x84C6;
		public const GLenum GL_TEXTURE7 = 0x84C7;
		public const GLenum GL_TEXTURE8 = 0x84C8;
		public const GLenum GL_TEXTURE9 = 0x84C9;
		//#define GL_TEXTURE10                      0x84CA
		//#define GL_TEXTURE11                      0x84CB
		//#define GL_TEXTURE12                      0x84CC
		//#define GL_TEXTURE13                      0x84CD
		//#define GL_TEXTURE14                      0x84CE
		//#define GL_TEXTURE15                      0x84CF
		//#define GL_TEXTURE16                      0x84D0
		//#define GL_TEXTURE17                      0x84D1
		//#define GL_TEXTURE18                      0x84D2
		//#define GL_TEXTURE19                      0x84D3
		//#define GL_TEXTURE20                      0x84D4
		//#define GL_TEXTURE21                      0x84D5
		//#define GL_TEXTURE22                      0x84D6
		//#define GL_TEXTURE23                      0x84D7
		//#define GL_TEXTURE24                      0x84D8
		//#define GL_TEXTURE25                      0x84D9
		//#define GL_TEXTURE26                      0x84DA
		//#define GL_TEXTURE27                      0x84DB
		//#define GL_TEXTURE28                      0x84DC
		//#define GL_TEXTURE29                      0x84DD
		//#define GL_TEXTURE30                      0x84DE
		//#define GL_TEXTURE31                      0x84DF
		//#define GL_ACTIVE_TEXTURE                 0x84E0
		//#define GL_MULTISAMPLE                    0x809D
		//#define GL_SAMPLE_ALPHA_TO_COVERAGE       0x809E
		//#define GL_SAMPLE_ALPHA_TO_ONE            0x809F
		//#define GL_SAMPLE_COVERAGE                0x80A0
		//#define GL_SAMPLE_BUFFERS                 0x80A8
		//#define GL_SAMPLES                        0x80A9
		//#define GL_SAMPLE_COVERAGE_VALUE          0x80AA
		//#define GL_SAMPLE_COVERAGE_INVERT         0x80AB
		public const GLenum GL_TEXTURE_CUBE_MAP            = 0x8513;
		public const GLenum GL_TEXTURE_BINDING_CUBE_MAP    = 0x8514;
		public const GLenum GL_TEXTURE_CUBE_MAP_POSITIVE_X = 0x8515;
		public const GLenum GL_TEXTURE_CUBE_MAP_NEGATIVE_X = 0x8516;
		public const GLenum GL_TEXTURE_CUBE_MAP_POSITIVE_Y = 0x8517;
		public const GLenum GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = 0x8518;
		public const GLenum GL_TEXTURE_CUBE_MAP_POSITIVE_Z = 0x8519;
		public const GLenum GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = 0x851A;
		//#define GL_PROXY_TEXTURE_CUBE_MAP         0x851B
		//#define GL_MAX_CUBE_MAP_TEXTURE_SIZE      0x851C
		//#define GL_COMPRESSED_RGB                 0x84ED
		//#define GL_COMPRESSED_RGBA                0x84EE
		//#define GL_TEXTURE_COMPRESSION_HINT       0x84EF
		//#define GL_TEXTURE_COMPRESSED_IMAGE_SIZE  0x86A0
		//#define GL_TEXTURE_COMPRESSED             0x86A1
		//#define GL_NUM_COMPRESSED_TEXTURE_FORMATS 0x86A2
		//#define GL_COMPRESSED_TEXTURE_FORMATS     0x86A3
		//#define GL_CLAMP_TO_BORDER                0x812D


		[GL_VERSION_1_3]	public static PFNGLACTIVETEXTUREPROC glActiveTexture;			// (GLenum texture)
//GLAPI void APIENTRY glSampleCoverage (GLclampf value, GLboolean invert);
		[GL_VERSION_1_3]	public static PFNGLCOMPRESSEDTEXIMAGE3DPROC glCompressedTexImage3D;		// (GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLsizei imageSize, const void *data);
		[GL_VERSION_1_3]	public static PFNGLCOMPRESSEDTEXIMAGE2DPROC glCompressedTexImage2D;		// (GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize, const void *data);
		[GL_VERSION_1_3]	public static PFNGLCOMPRESSEDTEXIMAGE1DPROC glCompressedTexImage1D;		// (GLenum target, GLint level, GLenum internalformat, GLsizei width, GLint border, GLsizei imageSize, const void *data);
		[GL_VERSION_1_3]	public static PFNGLCOMPRESSEDTEXSUBIMAGE3DPROC glCompressedTexSubImage3D;// (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLsizei imageSize, const void *data);
		[GL_VERSION_1_3]	public static PFNGLCOMPRESSEDTEXSUBIMAGE2DPROC glCompressedTexSubImage2D;// (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, const void *data);
		[GL_VERSION_1_3]	public static PFNGLCOMPRESSEDTEXSUBIMAGE1DPROC glCompressedTexSubImage1D;// (GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLsizei imageSize, const void *data);
//GLAPI void APIENTRY glGetCompressedTexImage (GLenum target, GLint level, void *img);//*/

		[GL_VERSION_1_3]	unsafe public delegate void PFNGLACTIVETEXTUREPROC(GLenum texture);
//typedef void (APIENTRYP PFNGLSAMPLECOVERAGEPROC) (GLclampf value, GLboolean invert);
		[GL_VERSION_1_3]	unsafe public delegate void PFNGLCOMPRESSEDTEXIMAGE3DPROC(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLsizei imageSize, byte[] data);
		[GL_VERSION_1_3]	unsafe public delegate void PFNGLCOMPRESSEDTEXIMAGE2DPROC(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize, void *data);
		[GL_VERSION_1_3]	unsafe public delegate void PFNGLCOMPRESSEDTEXIMAGE1DPROC(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLint border, GLsizei imageSize, void *data);
		[GL_VERSION_1_3]	unsafe public delegate void PFNGLCOMPRESSEDTEXSUBIMAGE3DPROC(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLsizei imageSize, void *data);
		[GL_VERSION_1_3]	unsafe public delegate void PFNGLCOMPRESSEDTEXSUBIMAGE2DPROC(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, void *data);
		[GL_VERSION_1_3]	unsafe public delegate void PFNGLCOMPRESSEDTEXSUBIMAGE1DPROC(GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLsizei imageSize, void *data);
//typedef void (APIENTRYP PFNGLGETCOMPRESSEDTEXIMAGEPROC) (GLenum target, GLint level, void *img);//*/
	}

	/// <summary>
	/// GL_VERSION_1_3_DEPRECATED
	/// </summary>
	[GL_VERSION_1_3_DEPRECATED]
	public partial class OpenGL	// GL_VERSION_1_3_DEPRECATED
	{
		class GL_VERSION_1_3_DEPRECATED : OpenGLExtension {
			public GL_VERSION_1_3_DEPRECATED() : base("GL_VERSION_1_3_DEPRECATED") { }
		}
		/*
GLAPI void APIENTRY glClientActiveTexture (GLenum texture);
GLAPI void APIENTRY glMultiTexCoord1d (GLenum target, GLdouble s);
GLAPI void APIENTRY glMultiTexCoord1dv (GLenum target, const GLdouble *v);
GLAPI void APIENTRY glMultiTexCoord1f (GLenum target, GLfloat s);
GLAPI void APIENTRY glMultiTexCoord1fv (GLenum target, const GLfloat *v);
GLAPI void APIENTRY glMultiTexCoord1i (GLenum target, GLint s);
GLAPI void APIENTRY glMultiTexCoord1iv (GLenum target, const GLint *v);
GLAPI void APIENTRY glMultiTexCoord1s (GLenum target, GLshort s);
GLAPI void APIENTRY glMultiTexCoord1sv (GLenum target, const GLshort *v);
GLAPI void APIENTRY glMultiTexCoord2d (GLenum target, GLdouble s, GLdouble t);
GLAPI void APIENTRY glMultiTexCoord2dv (GLenum target, const GLdouble *v);
GLAPI void APIENTRY glMultiTexCoord2f (GLenum target, GLfloat s, GLfloat t);
GLAPI void APIENTRY glMultiTexCoord2fv (GLenum target, const GLfloat *v);
GLAPI void APIENTRY glMultiTexCoord2i (GLenum target, GLint s, GLint t);
GLAPI void APIENTRY glMultiTexCoord2iv (GLenum target, const GLint *v);
GLAPI void APIENTRY glMultiTexCoord2s (GLenum target, GLshort s, GLshort t);
GLAPI void APIENTRY glMultiTexCoord2sv (GLenum target, const GLshort *v);
GLAPI void APIENTRY glMultiTexCoord3d (GLenum target, GLdouble s, GLdouble t, GLdouble r);
GLAPI void APIENTRY glMultiTexCoord3dv (GLenum target, const GLdouble *v);
GLAPI void APIENTRY glMultiTexCoord3f (GLenum target, GLfloat s, GLfloat t, GLfloat r);
GLAPI void APIENTRY glMultiTexCoord3fv (GLenum target, const GLfloat *v);
GLAPI void APIENTRY glMultiTexCoord3i (GLenum target, GLint s, GLint t, GLint r);
GLAPI void APIENTRY glMultiTexCoord3iv (GLenum target, const GLint *v);
GLAPI void APIENTRY glMultiTexCoord3s (GLenum target, GLshort s, GLshort t, GLshort r);
GLAPI void APIENTRY glMultiTexCoord3sv (GLenum target, const GLshort *v);
GLAPI void APIENTRY glMultiTexCoord4d (GLenum target, GLdouble s, GLdouble t, GLdouble r, GLdouble q);
GLAPI void APIENTRY glMultiTexCoord4dv (GLenum target, const GLdouble *v);
GLAPI void APIENTRY glMultiTexCoord4f (GLenum target, GLfloat s, GLfloat t, GLfloat r, GLfloat q);
GLAPI void APIENTRY glMultiTexCoord4fv (GLenum target, const GLfloat *v);
GLAPI void APIENTRY glMultiTexCoord4i (GLenum target, GLint s, GLint t, GLint r, GLint q);
GLAPI void APIENTRY glMultiTexCoord4iv (GLenum target, const GLint *v);
GLAPI void APIENTRY glMultiTexCoord4s (GLenum target, GLshort s, GLshort t, GLshort r, GLshort q);
GLAPI void APIENTRY glMultiTexCoord4sv (GLenum target, const GLshort *v);
GLAPI void APIENTRY glLoadTransposeMatrixf (const GLfloat *m);
GLAPI void APIENTRY glLoadTransposeMatrixd (const GLdouble *m);
GLAPI void APIENTRY glMultTransposeMatrixf (const GLfloat *m);
GLAPI void APIENTRY glMultTransposeMatrixd (const GLdouble *m);

typedef void (APIENTRYP PFNGLCLIENTACTIVETEXTUREPROC) (GLenum texture);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1DPROC) (GLenum target, GLdouble s);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1DVPROC) (GLenum target, const GLdouble *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1FPROC) (GLenum target, GLfloat s);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1FVPROC) (GLenum target, const GLfloat *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1IPROC) (GLenum target, GLint s);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1IVPROC) (GLenum target, const GLint *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1SPROC) (GLenum target, GLshort s);
typedef void (APIENTRYP PFNGLMULTITEXCOORD1SVPROC) (GLenum target, const GLshort *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2DPROC) (GLenum target, GLdouble s, GLdouble t);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2DVPROC) (GLenum target, const GLdouble *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2FPROC) (GLenum target, GLfloat s, GLfloat t);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2FVPROC) (GLenum target, const GLfloat *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2IPROC) (GLenum target, GLint s, GLint t);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2IVPROC) (GLenum target, const GLint *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2SPROC) (GLenum target, GLshort s, GLshort t);
typedef void (APIENTRYP PFNGLMULTITEXCOORD2SVPROC) (GLenum target, const GLshort *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3DPROC) (GLenum target, GLdouble s, GLdouble t, GLdouble r);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3DVPROC) (GLenum target, const GLdouble *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3FPROC) (GLenum target, GLfloat s, GLfloat t, GLfloat r);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3FVPROC) (GLenum target, const GLfloat *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3IPROC) (GLenum target, GLint s, GLint t, GLint r);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3IVPROC) (GLenum target, const GLint *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3SPROC) (GLenum target, GLshort s, GLshort t, GLshort r);
typedef void (APIENTRYP PFNGLMULTITEXCOORD3SVPROC) (GLenum target, const GLshort *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4DPROC) (GLenum target, GLdouble s, GLdouble t, GLdouble r, GLdouble q);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4DVPROC) (GLenum target, const GLdouble *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4FPROC) (GLenum target, GLfloat s, GLfloat t, GLfloat r, GLfloat q);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4FVPROC) (GLenum target, const GLfloat *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4IPROC) (GLenum target, GLint s, GLint t, GLint r, GLint q);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4IVPROC) (GLenum target, const GLint *v);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4SPROC) (GLenum target, GLshort s, GLshort t, GLshort r, GLshort q);
typedef void (APIENTRYP PFNGLMULTITEXCOORD4SVPROC) (GLenum target, const GLshort *v);
typedef void (APIENTRYP PFNGLLOADTRANSPOSEMATRIXFPROC) (const GLfloat *m);
typedef void (APIENTRYP PFNGLLOADTRANSPOSEMATRIXDPROC) (const GLdouble *m);
typedef void (APIENTRYP PFNGLMULTTRANSPOSEMATRIXFPROC) (const GLfloat *m);
typedef void (APIENTRYP PFNGLMULTTRANSPOSEMATRIXDPROC) (const GLdouble *m);
		*/
	}

	/// <summary>
	/// GL_VERSION_1_4
	/// </summary>
	[GL_VERSION_1_4]
	public partial class OpenGL
	{
		class GL_VERSION_1_4 : OpenGLExtension {
			public GL_VERSION_1_4() : base("GL_VERSION_1_4") { }
		}
		/*
GLAPI void APIENTRY glBlendFuncSeparate (GLenum sfactorRGB, GLenum dfactorRGB, GLenum sfactorAlpha, GLenum dfactorAlpha);
GLAPI void APIENTRY glMultiDrawArrays (GLenum mode, const GLint *first, const GLsizei *count, GLsizei primcount);
GLAPI void APIENTRY glMultiDrawElements (GLenum mode, const GLsizei *count, GLenum type, const void* *indices, GLsizei primcount);
GLAPI void APIENTRY glPointParameterf (GLenum pname, GLfloat param);
GLAPI void APIENTRY glPointParameterfv (GLenum pname, const GLfloat *params);
GLAPI void APIENTRY glPointParameteri (GLenum pname, GLint param);
GLAPI void APIENTRY glPointParameteriv (GLenum pname, const GLint *params);

typedef void (APIENTRYP PFNGLBLENDFUNCSEPARATEPROC) (GLenum sfactorRGB, GLenum dfactorRGB, GLenum sfactorAlpha, GLenum dfactorAlpha);
typedef void (APIENTRYP PFNGLMULTIDRAWARRAYSPROC) (GLenum mode, const GLint *first, const GLsizei *count, GLsizei primcount);
typedef void (APIENTRYP PFNGLMULTIDRAWELEMENTSPROC) (GLenum mode, const GLsizei *count, GLenum type, const void* *indices, GLsizei primcount);
typedef void (APIENTRYP PFNGLPOINTPARAMETERFPROC) (GLenum pname, GLfloat param);
typedef void (APIENTRYP PFNGLPOINTPARAMETERFVPROC) (GLenum pname, const GLfloat *params);
typedef void (APIENTRYP PFNGLPOINTPARAMETERIPROC) (GLenum pname, GLint param);
typedef void (APIENTRYP PFNGLPOINTPARAMETERIVPROC) (GLenum pname, const GLint *params);
		*/
	}

	/// <summary>
	/// GL_VERSION_1_4_DEPRECATED
	/// </summary>
	[GL_VERSION_1_4_DEPRECATED]
	public partial class OpenGL
	{
		class GL_VERSION_1_4_DEPRECATED : OpenGLExtension {
			public GL_VERSION_1_4_DEPRECATED() : base("GL_VERSION_1_4_DEPRECATED") { }
		}
		/*
GLAPI void APIENTRY glFogCoordf (GLfloat coord);
GLAPI void APIENTRY glFogCoordfv (const GLfloat *coord);
GLAPI void APIENTRY glFogCoordd (GLdouble coord);
GLAPI void APIENTRY glFogCoorddv (const GLdouble *coord);
GLAPI void APIENTRY glFogCoordPointer (GLenum type, GLsizei stride, const void *pointer);
GLAPI void APIENTRY glSecondaryColor3b (GLbyte red, GLbyte green, GLbyte blue);
GLAPI void APIENTRY glSecondaryColor3bv (const GLbyte *v);
GLAPI void APIENTRY glSecondaryColor3d (GLdouble red, GLdouble green, GLdouble blue);
GLAPI void APIENTRY glSecondaryColor3dv (const GLdouble *v);
GLAPI void APIENTRY glSecondaryColor3f (GLfloat red, GLfloat green, GLfloat blue);
GLAPI void APIENTRY glSecondaryColor3fv (const GLfloat *v);
GLAPI void APIENTRY glSecondaryColor3i (GLint red, GLint green, GLint blue);
GLAPI void APIENTRY glSecondaryColor3iv (const GLint *v);
GLAPI void APIENTRY glSecondaryColor3s (GLshort red, GLshort green, GLshort blue);
GLAPI void APIENTRY glSecondaryColor3sv (const GLshort *v);
GLAPI void APIENTRY glSecondaryColor3ub (GLubyte red, GLubyte green, GLubyte blue);
GLAPI void APIENTRY glSecondaryColor3ubv (const GLubyte *v);
GLAPI void APIENTRY glSecondaryColor3ui (GLuint red, GLuint green, GLuint blue);
GLAPI void APIENTRY glSecondaryColor3uiv (const GLuint *v);
GLAPI void APIENTRY glSecondaryColor3us (GLushort red, GLushort green, GLushort blue);
GLAPI void APIENTRY glSecondaryColor3usv (const GLushort *v);
GLAPI void APIENTRY glSecondaryColorPointer (GLint size, GLenum type, GLsizei stride, const void *pointer);
GLAPI void APIENTRY glWindowPos2d (GLdouble x, GLdouble y);
GLAPI void APIENTRY glWindowPos2dv (const GLdouble *v);
GLAPI void APIENTRY glWindowPos2f (GLfloat x, GLfloat y);
GLAPI void APIENTRY glWindowPos2fv (const GLfloat *v);
GLAPI void APIENTRY glWindowPos2i (GLint x, GLint y);
GLAPI void APIENTRY glWindowPos2iv (const GLint *v);
GLAPI void APIENTRY glWindowPos2s (GLshort x, GLshort y);
GLAPI void APIENTRY glWindowPos2sv (const GLshort *v);
GLAPI void APIENTRY glWindowPos3d (GLdouble x, GLdouble y, GLdouble z);
GLAPI void APIENTRY glWindowPos3dv (const GLdouble *v);
GLAPI void APIENTRY glWindowPos3f (GLfloat x, GLfloat y, GLfloat z);
GLAPI void APIENTRY glWindowPos3fv (const GLfloat *v);
GLAPI void APIENTRY glWindowPos3i (GLint x, GLint y, GLint z);
GLAPI void APIENTRY glWindowPos3iv (const GLint *v);
GLAPI void APIENTRY glWindowPos3s (GLshort x, GLshort y, GLshort z);
GLAPI void APIENTRY glWindowPos3sv (const GLshort *v);

typedef void (APIENTRYP PFNGLFOGCOORDFPROC) (GLfloat coord);
typedef void (APIENTRYP PFNGLFOGCOORDFVPROC) (const GLfloat *coord);
typedef void (APIENTRYP PFNGLFOGCOORDDPROC) (GLdouble coord);
typedef void (APIENTRYP PFNGLFOGCOORDDVPROC) (const GLdouble *coord);
typedef void (APIENTRYP PFNGLFOGCOORDPOINTERPROC) (GLenum type, GLsizei stride, const void *pointer);
typedef void (APIENTRYP PFNGLSECONDARYCOLOR3BPROC) (GLbyte red, GLbyte green, GLbyte blue);
typedef void (APIENTRYP PFNGLSECONDARYCOLOR3BVPROC) (const GLbyte *v);
typedef void (APIENTRYP PFNGLSECONDARYCOLOR3DPROC) (GLdouble red, GLdouble green, GLdouble blue);
typedef void (APIENTRYP PFNGLSECONDARYCOLOR3DVPROC) (const GLdouble *v);
typedef void (APIENTRYP PFNGLSECONDARYCOLOR3FPROC) (GLfloat red, GLfloat green, GLfloat blue);
typedef void (APIENTRYP PFNGLSECONDARYCOLOR3FVPROC) (const GLfloat *v);
typedef void (APIENTRYP PFNGLSECONDARYCOLOR3IPROC) (GLint red, GLint green, GLint blue);
typedef void (APIENTRYP PFNGLSECONDARYCOLOR3IVPROC) (const GLint *v);
typedef void (APIENTRYP PFNGLSECONDARYCOLOR3SPROC) (GLshort red, GLshort green, GLshort blue);
typedef void (APIENTRYP PFNGLSECONDARYCOLOR3SVPROC) (const GLshort *v);
typedef void (APIENTRYP PFNGLSECONDARYCOLOR3UBPROC) (GLubyte red, GLubyte green, GLubyte blue);
typedef void (APIENTRYP PFNGLSECONDARYCOLOR3UBVPROC) (const GLubyte *v);
typedef void (APIENTRYP PFNGLSECONDARYCOLOR3UIPROC) (GLuint red, GLuint green, GLuint blue);
typedef void (APIENTRYP PFNGLSECONDARYCOLOR3UIVPROC) (const GLuint *v);
typedef void (APIENTRYP PFNGLSECONDARYCOLOR3USPROC) (GLushort red, GLushort green, GLushort blue);
typedef void (APIENTRYP PFNGLSECONDARYCOLOR3USVPROC) (const GLushort *v);
typedef void (APIENTRYP PFNGLSECONDARYCOLORPOINTERPROC) (GLint size, GLenum type, GLsizei stride, const void *pointer);
typedef void (APIENTRYP PFNGLWINDOWPOS2DPROC) (GLdouble x, GLdouble y);
typedef void (APIENTRYP PFNGLWINDOWPOS2DVPROC) (const GLdouble *v);
typedef void (APIENTRYP PFNGLWINDOWPOS2FPROC) (GLfloat x, GLfloat y);
typedef void (APIENTRYP PFNGLWINDOWPOS2FVPROC) (const GLfloat *v);
typedef void (APIENTRYP PFNGLWINDOWPOS2IPROC) (GLint x, GLint y);
typedef void (APIENTRYP PFNGLWINDOWPOS2IVPROC) (const GLint *v);
typedef void (APIENTRYP PFNGLWINDOWPOS2SPROC) (GLshort x, GLshort y);
typedef void (APIENTRYP PFNGLWINDOWPOS2SVPROC) (const GLshort *v);
typedef void (APIENTRYP PFNGLWINDOWPOS3DPROC) (GLdouble x, GLdouble y, GLdouble z);
typedef void (APIENTRYP PFNGLWINDOWPOS3DVPROC) (const GLdouble *v);
typedef void (APIENTRYP PFNGLWINDOWPOS3FPROC) (GLfloat x, GLfloat y, GLfloat z);
typedef void (APIENTRYP PFNGLWINDOWPOS3FVPROC) (const GLfloat *v);
typedef void (APIENTRYP PFNGLWINDOWPOS3IPROC) (GLint x, GLint y, GLint z);
typedef void (APIENTRYP PFNGLWINDOWPOS3IVPROC) (const GLint *v);
typedef void (APIENTRYP PFNGLWINDOWPOS3SPROC) (GLshort x, GLshort y, GLshort z);
typedef void (APIENTRYP PFNGLWINDOWPOS3SVPROC) (const GLshort *v);
		*/
	}

	/// <summary>
	/// GL_VERSION_1_5
	/// </summary>
	[GL_VERSION_1_5]
	public partial class OpenGL
	{
		class GL_VERSION_1_5 : OpenGLExtension {
			public GL_VERSION_1_5() : base("GL_VERSION_1_5") { }
		}
		/*
GLAPI void APIENTRY glGenQueries (GLsizei n, GLuint *ids);
GLAPI void APIENTRY glDeleteQueries (GLsizei n, const GLuint *ids);
GLAPI GLboolean APIENTRY glIsQuery (GLuint id);
GLAPI void APIENTRY glBeginQuery (GLenum target, GLuint id);
GLAPI void APIENTRY glEndQuery (GLenum target);
GLAPI void APIENTRY glGetQueryiv (GLenum target, GLenum pname, GLint *params);
GLAPI void APIENTRY glGetQueryObjectiv (GLuint id, GLenum pname, GLint *params);
GLAPI void APIENTRY glGetQueryObjectuiv (GLuint id, GLenum pname, GLuint *params);
GLAPI void APIENTRY glBindBuffer (GLenum target, GLuint buffer);
GLAPI void APIENTRY glDeleteBuffers (GLsizei n, const GLuint *buffers);
GLAPI void APIENTRY glGenBuffers (GLsizei n, GLuint *buffers);
GLAPI GLboolean APIENTRY glIsBuffer (GLuint buffer);
GLAPI void APIENTRY glBufferData (GLenum target, GLsizeiptr size, const void *data, GLenum usage);
GLAPI void APIENTRY glBufferSubData (GLenum target, GLintptr offset, GLsizeiptr size, const void *data);
GLAPI void APIENTRY glGetBufferSubData (GLenum target, GLintptr offset, GLsizeiptr size, void *data);
GLAPI void* APIENTRY glMapBuffer (GLenum target, GLenum access);
GLAPI GLboolean APIENTRY glUnmapBuffer (GLenum target);
GLAPI void APIENTRY glGetBufferParameteriv (GLenum target, GLenum pname, GLint *params);
GLAPI void APIENTRY glGetBufferPointerv (GLenum target, GLenum pname, void* *params);

typedef void (APIENTRYP PFNGLGENQUERIESPROC) (GLsizei n, GLuint *ids);
typedef void (APIENTRYP PFNGLDELETEQUERIESPROC) (GLsizei n, const GLuint *ids);
typedef GLboolean (APIENTRYP PFNGLISQUERYPROC) (GLuint id);
typedef void (APIENTRYP PFNGLBEGINQUERYPROC) (GLenum target, GLuint id);
typedef void (APIENTRYP PFNGLENDQUERYPROC) (GLenum target);
typedef void (APIENTRYP PFNGLGETQUERYIVPROC) (GLenum target, GLenum pname, GLint *params);
typedef void (APIENTRYP PFNGLGETQUERYOBJECTIVPROC) (GLuint id, GLenum pname, GLint *params);
typedef void (APIENTRYP PFNGLGETQUERYOBJECTUIVPROC) (GLuint id, GLenum pname, GLuint *params);
typedef void (APIENTRYP PFNGLBINDBUFFERPROC) (GLenum target, GLuint buffer);
typedef void (APIENTRYP PFNGLDELETEBUFFERSPROC) (GLsizei n, const GLuint *buffers);
typedef void (APIENTRYP PFNGLGENBUFFERSPROC) (GLsizei n, GLuint *buffers);
typedef GLboolean (APIENTRYP PFNGLISBUFFERPROC) (GLuint buffer);
typedef void (APIENTRYP PFNGLBUFFERDATAPROC) (GLenum target, GLsizeiptr size, const void *data, GLenum usage);
typedef void (APIENTRYP PFNGLBUFFERSUBDATAPROC) (GLenum target, GLintptr offset, GLsizeiptr size, const void *data);
typedef void (APIENTRYP PFNGLGETBUFFERSUBDATAPROC) (GLenum target, GLintptr offset, GLsizeiptr size, void *data);
typedef void* (APIENTRYP PFNGLMAPBUFFERPROC) (GLenum target, GLenum access);
typedef GLboolean (APIENTRYP PFNGLUNMAPBUFFERPROC) (GLenum target);
typedef void (APIENTRYP PFNGLGETBUFFERPARAMETERIVPROC) (GLenum target, GLenum pname, GLint *params);
typedef void (APIENTRYP PFNGLGETBUFFERPOINTERVPROC) (GLenum target, GLenum pname, void* *params);
		*/
	}
	/// <summary>
	/// GL_VERSION_2_0
	/// </summary>
	[GL_VERSION_2_0]
	public partial class OpenGL
	{
		class GL_VERSION_2_0 : OpenGLExtension {
			public GL_VERSION_2_0() : base("GL_VERSION_2_0") { }
		}

		//GLAPI void APIENTRY glBlendEquationSeparate (GLenum modeRGB, GLenum modeAlpha);
		//GLAPI void APIENTRY glDrawBuffers (GLsizei n, const GLenum *bufs);
		//GLAPI void APIENTRY glStencilOpSeparate (GLenum face, GLenum sfail, GLenum dpfail, GLenum dppass);
		//GLAPI void APIENTRY glStencilFuncSeparate (GLenum face, GLenum func, GLint ref, GLuint mask);
		//GLAPI void APIENTRY glStencilMaskSeparate (GLenum face, GLuint mask);
		[GL_VERSION_2_0]	public static PFNGLATTACHSHADERPROC glAttachShader;// (GLuint program, GLuint shader);
		//GLAPI void APIENTRY glBindAttribLocation (GLuint program, GLuint index, const GLchar *name);
		[GL_VERSION_2_0]	public static PFNGLCOMPILESHADERPROC glCompileShader;// (GLuint shader);
		[GL_VERSION_2_0]	public static PFNGLCREATEPROGRAMPROC glCreateProgram;
		[GL_VERSION_2_0]	public static PFNGLCREATESHADERPROC glCreateShader;
		//GLAPI void APIENTRY glDeleteProgram (GLuint program);
		//GLAPI void APIENTRY glDeleteShader (GLuint shader);
		//GLAPI void APIENTRY glDetachShader (GLuint program, GLuint shader);
		//GLAPI void APIENTRY glDisableVertexAttribArray (GLuint index);
		//GLAPI void APIENTRY glEnableVertexAttribArray (GLuint index);
		//GLAPI void APIENTRY glGetActiveAttrib (GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLint *size, GLenum *type, GLchar *name);
		//GLAPI void APIENTRY glGetActiveUniform (GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLint *size, GLenum *type, GLchar *name);
		//GLAPI void APIENTRY glGetAttachedShaders (GLuint program, GLsizei maxCount, GLsizei *count, GLuint *obj);
		//GLAPI GLint APIENTRY glGetAttribLocation (GLuint program, const GLchar *name);
		[GL_VERSION_2_0]	public static PFNGLGETPROGRAMIVPROC glGetProgramiv;				// (GLuint program, GLenum pname, GLint *params);
		[GL_VERSION_2_0]	public static PFNGLGETPROGRAMINFOLOGPROC glGetProgramInfoLog;	// (GLuint program, GLsizei bufSize, GLsizei *length, GLchar *infoLog);
		[GL_VERSION_2_0]	public static PFNGLGETSHADERIVPROC glGetShaderiv;				// (GLuint shader, GLenum pname, GLint *params);
		[GL_VERSION_2_0]	public static PFNGLGETSHADERINFOLOGPROC glGetShaderInfoLog;		// (GLuint shader, GLsizei bufSize, GLsizei *length, GLchar *infoLog);
		//GLAPI void APIENTRY glGetShaderSource (GLuint shader, GLsizei bufSize, GLsizei *length, GLchar *source);
		[GL_VERSION_2_0]	public static PFNGLGETUNIFORMLOCATIONPROC glGetUniformLocation;	// (GLuint program, const GLchar *name);
		//GLAPI void APIENTRY glGetUniformfv (GLuint program, GLint location, GLfloat *params);
		//GLAPI void APIENTRY glGetUniformiv (GLuint program, GLint location, GLint *params);
		//GLAPI void APIENTRY glGetVertexAttribdv (GLuint index, GLenum pname, GLdouble *params);
		//GLAPI void APIENTRY glGetVertexAttribfv (GLuint index, GLenum pname, GLfloat *params);
		//GLAPI void APIENTRY glGetVertexAttribiv (GLuint index, GLenum pname, GLint *params);
		//GLAPI void APIENTRY glGetVertexAttribPointerv (GLuint index, GLenum pname, void* *pointer);
		//GLAPI GLboolean APIENTRY glIsProgram (GLuint program);
		//GLAPI GLboolean APIENTRY glIsShader (GLuint shader);
		[GL_VERSION_2_0]	public static PFNGLLINKPROGRAMPROC glLinkProgram;	// (GLuint program);
		[GL_VERSION_2_0]	public static PFNGLSHADERSOURCEPROC glShaderSource;	// (GLuint shader, GLsizei count, const GLchar* *string, const GLint *length);
		[GL_VERSION_2_0]	public static PFNGLUSEPROGRAMPROC glUseProgram;		// (GLuint program);
		[GL_VERSION_2_0]	public static PFNGLUNIFORM1FPROC glUniform1f;		// (GLint location, GLfloat v0);
		[GL_VERSION_2_0]	public static PFNGLUNIFORM2FPROC glUniform2f;		// (GLint location, GLfloat v0, GLfloat v1);
		[GL_VERSION_2_0]	public static PFNGLUNIFORM3FPROC glUniform3f;		// (GLint location, GLfloat v0, GLfloat v1, GLfloat v2);
		[GL_VERSION_2_0]	public static PFNGLUNIFORM4FPROC glUniform4f;		// (GLint location, GLfloat v0, GLfloat v1, GLfloat v2, GLfloat v3);
		[GL_VERSION_2_0]	public static PFNGLUNIFORM1IPROC glUniform1i;		// (GLint location, GLint v0);
		[GL_VERSION_2_0]	public static PFNGLUNIFORM2IPROC glUniform2i;		// (GLint location, GLint v0, GLint v1);
		[GL_VERSION_2_0]	public static PFNGLUNIFORM3IPROC glUniform3i;		// (GLint location, GLint v0, GLint v1, GLint v2);
		[GL_VERSION_2_0]	public static PFNGLUNIFORM4IPROC glUniform4i;		// (GLint location, GLint v0, GLint v1, GLint v2, GLint v3);
		[GL_VERSION_2_0]	unsafe public static PFNGLUNIFORM1FVPROC glUniform1fv;		// (GLint location, GLsizei count, const GLfloat *value);
		[GL_VERSION_2_0]	unsafe public static PFNGLUNIFORM2FVPROC glUniform2fv;		// (GLint location, GLsizei count, const GLfloat *value);
		[GL_VERSION_2_0]	unsafe public static PFNGLUNIFORM3FVPROC glUniform3fv;		// (GLint location, GLsizei count, const GLfloat *value);
		[GL_VERSION_2_0]	unsafe public static PFNGLUNIFORM4FVPROC glUniform4fv;		// (GLint location, GLsizei count, const GLfloat *value);
		//[GL_VERSION_2_0]	public static GLAPI void APIENTRY glUniform1iv (GLint location, GLsizei count, const GLint *value);
		//GLAPI void APIENTRY glUniform2iv (GLint location, GLsizei count, const GLint *value);
		//GLAPI void APIENTRY glUniform3iv (GLint location, GLsizei count, const GLint *value);
		//GLAPI void APIENTRY glUniform4iv (GLint location, GLsizei count, const GLint *value);
		//GLAPI void APIENTRY glUniformMatrix2fv (GLint location, GLsizei count, GLboolean transpose, const GLfloat *value);
		//GLAPI void APIENTRY glUniformMatrix3fv (GLint location, GLsizei count, GLboolean transpose, const GLfloat *value);
		//GLAPI void APIENTRY glUniformMatrix4fv (GLint location, GLsizei count, GLboolean transpose, const GLfloat *value);
		//GLAPI void APIENTRY glValidateProgram (GLuint program);
		//GLAPI void APIENTRY glVertexAttrib1d (GLuint index, GLdouble x);
		//GLAPI void APIENTRY glVertexAttrib1dv (GLuint index, const GLdouble *v);
		//GLAPI void APIENTRY glVertexAttrib1f (GLuint index, GLfloat x);
		//GLAPI void APIENTRY glVertexAttrib1fv (GLuint index, const GLfloat *v);
		//GLAPI void APIENTRY glVertexAttrib1s (GLuint index, GLshort x);
		//GLAPI void APIENTRY glVertexAttrib1sv (GLuint index, const GLshort *v);
		//GLAPI void APIENTRY glVertexAttrib2d (GLuint index, GLdouble x, GLdouble y);
		//GLAPI void APIENTRY glVertexAttrib2dv (GLuint index, const GLdouble *v);
		//GLAPI void APIENTRY glVertexAttrib2f (GLuint index, GLfloat x, GLfloat y);
		//GLAPI void APIENTRY glVertexAttrib2fv (GLuint index, const GLfloat *v);
		//GLAPI void APIENTRY glVertexAttrib2s (GLuint index, GLshort x, GLshort y);
		//GLAPI void APIENTRY glVertexAttrib2sv (GLuint index, const GLshort *v);
		//GLAPI void APIENTRY glVertexAttrib3d (GLuint index, GLdouble x, GLdouble y, GLdouble z);
		//GLAPI void APIENTRY glVertexAttrib3dv (GLuint index, const GLdouble *v);
		//GLAPI void APIENTRY glVertexAttrib3f (GLuint index, GLfloat x, GLfloat y, GLfloat z);
		//GLAPI void APIENTRY glVertexAttrib3fv (GLuint index, const GLfloat *v);
		//GLAPI void APIENTRY glVertexAttrib3s (GLuint index, GLshort x, GLshort y, GLshort z);
		//GLAPI void APIENTRY glVertexAttrib3sv (GLuint index, const GLshort *v);
		//GLAPI void APIENTRY glVertexAttrib4Nbv (GLuint index, const GLbyte *v);
		//GLAPI void APIENTRY glVertexAttrib4Niv (GLuint index, const GLint *v);
		//GLAPI void APIENTRY glVertexAttrib4Nsv (GLuint index, const GLshort *v);
		//GLAPI void APIENTRY glVertexAttrib4Nub (GLuint index, GLubyte x, GLubyte y, GLubyte z, GLubyte w);
		//GLAPI void APIENTRY glVertexAttrib4Nubv (GLuint index, const GLubyte *v);
		//GLAPI void APIENTRY glVertexAttrib4Nuiv (GLuint index, const GLuint *v);
		//GLAPI void APIENTRY glVertexAttrib4Nusv (GLuint index, const GLushort *v);
		//GLAPI void APIENTRY glVertexAttrib4bv (GLuint index, const GLbyte *v);
		//GLAPI void APIENTRY glVertexAttrib4d (GLuint index, GLdouble x, GLdouble y, GLdouble z, GLdouble w);
		//GLAPI void APIENTRY glVertexAttrib4dv (GLuint index, const GLdouble *v);
		//GLAPI void APIENTRY glVertexAttrib4f (GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w);
		//GLAPI void APIENTRY glVertexAttrib4fv (GLuint index, const GLfloat *v);
		//GLAPI void APIENTRY glVertexAttrib4iv (GLuint index, const GLint *v);
		//GLAPI void APIENTRY glVertexAttrib4s (GLuint index, GLshort x, GLshort y, GLshort z, GLshort w);
		//GLAPI void APIENTRY glVertexAttrib4sv (GLuint index, const GLshort *v);
		//GLAPI void APIENTRY glVertexAttrib4ubv (GLuint index, const GLubyte *v);
		//GLAPI void APIENTRY glVertexAttrib4uiv (GLuint index, const GLuint *v);
		//GLAPI void APIENTRY glVertexAttrib4usv (GLuint index, const GLushort *v);
		//GLAPI void APIENTRY glVertexAttribPointer (GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const void *pointer);*/

		//[GL_VERSION_2_0]	public delegate void PFNGLBLENDEQUATIONSEPARATEPROC(GLenum modeRGB, GLenum modeAlpha);
		//[GL_VERSION_2_0]	public delegate void PFNGLDRAWBUFFERSPROC(GLsizei n, const GLenum *bufs);
		//[GL_VERSION_2_0]	public delegate void PFNGLSTENCILOPSEPARATEPROC(GLenum face, GLenum sfail, GLenum dpfail, GLenum dppass);
		//[GL_VERSION_2_0]	public delegate void PFNGLSTENCILFUNCSEPARATEPROC(GLenum face, GLenum func, GLint ref, GLuint mask);
		//[GL_VERSION_2_0]	public delegate void PFNGLSTENCILMASKSEPARATEPROC(GLenum face, GLuint mask);
		[GL_VERSION_2_0]	public delegate void PFNGLATTACHSHADERPROC(GLuint program, GLuint shader);
		//[GL_VERSION_2_0]	public delegate void PFNGLBINDATTRIBLOCATIONPROC(GLuint program, GLuint index, const GLchar *name);
		[GL_VERSION_2_0]	public delegate void PFNGLCOMPILESHADERPROC(GLuint shader);
		[GL_VERSION_2_0]	public delegate GLuint PFNGLCREATEPROGRAMPROC();
		[GL_VERSION_2_0]	public delegate GLuint PFNGLCREATESHADERPROC(GLenum type);
		[GL_VERSION_2_0]	public delegate void PFNGLDELETEPROGRAMPROC(GLuint program);
		[GL_VERSION_2_0]	public delegate void PFNGLDELETESHADERPROC(GLuint shader);
		[GL_VERSION_2_0]	public delegate void PFNGLDETACHSHADERPROC(GLuint program, GLuint shader);
		[GL_VERSION_2_0]	public delegate void PFNGLDISABLEVERTEXATTRIBARRAYPROC(GLuint index);
		[GL_VERSION_2_0]	public delegate void PFNGLENABLEVERTEXATTRIBARRAYPROC(GLuint index);
		//[GL_VERSION_2_0]	public delegate void PFNGLGETACTIVEATTRIBPROC(GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLint *size, GLenum *type, GLchar *name);
		//[GL_VERSION_2_0]	public delegate void PFNGLGETACTIVEUNIFORMPROC(GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLint *size, GLenum *type, GLchar *name);
		//[GL_VERSION_2_0]	public delegate void PFNGLGETATTACHEDSHADERSPROC(GLuint program, GLsizei maxCount, GLsizei *count, GLuint *obj);
		//[GL_VERSION_2_0]	public delegate GLint PFNGLGETATTRIBLOCATIONPROC(GLuint program, const GLchar *name);
		[GL_VERSION_2_0]	public delegate void PFNGLGETPROGRAMIVPROC(GLuint program, GLenum pname, out GLint iparam);
		[GL_VERSION_2_0]	public delegate void PFNGLGETPROGRAMINFOLOGPROC(GLuint program, GLsizei bufSize, GLsizei[] length, StringBuilder infoLog);
		[GL_VERSION_2_0]	public delegate void PFNGLGETSHADERIVPROC(GLuint shader, GLenum pname, out GLint iparam);
		[GL_VERSION_2_0]	public delegate void PFNGLGETSHADERINFOLOGPROC(GLuint shader, GLsizei bufSize, GLsizei[] length, StringBuilder infoLog);
		//[GL_VERSION_2_0]	public delegate void PFNGLGETSHADERSOURCEPROC(GLuint shader, GLsizei bufSize, GLsizei *length, GLchar *source);
		[GL_VERSION_2_0]	public delegate GLint PFNGLGETUNIFORMLOCATIONPROC(GLuint program, String name);
		//[GL_VERSION_2_0]	public delegate void PFNGLGETUNIFORMFVPROC(GLuint program, GLint location, GLfloat *params);
		//[GL_VERSION_2_0]	public delegate void PFNGLGETUNIFORMIVPROC(GLuint program, GLint location, GLint *params);
		//[GL_VERSION_2_0]	public delegate void PFNGLGETVERTEXATTRIBDVPROC(GLuint index, GLenum pname, GLdouble *params);
		//[GL_VERSION_2_0]	public delegate void PFNGLGETVERTEXATTRIBFVPROC(GLuint index, GLenum pname, GLfloat *params);
		//[GL_VERSION_2_0]	public delegate void PFNGLGETVERTEXATTRIBIVPROC(GLuint index, GLenum pname, GLint *params);
		//[GL_VERSION_2_0]	public delegate void PFNGLGETVERTEXATTRIBPOINTERVPROC(GLuint index, GLenum pname, void* *pointer);
		[GL_VERSION_2_0]	public delegate GLboolean PFNGLISPROGRAMPROC(GLuint program);
		[GL_VERSION_2_0]	public delegate GLboolean PFNGLISSHADERPROC(GLuint shader);
		[GL_VERSION_2_0]	public delegate void PFNGLLINKPROGRAMPROC(GLuint program);
		[GL_VERSION_2_0]	public delegate void PFNGLSHADERSOURCEPROC(GLuint shader, GLsizei count, String[] source, GLint[] length);
		[GL_VERSION_2_0]	public delegate void PFNGLUSEPROGRAMPROC(GLuint program);
		[GL_VERSION_2_0]	public delegate void PFNGLUNIFORM1FPROC(GLint location, GLfloat v0);
		[GL_VERSION_2_0]	public delegate void PFNGLUNIFORM2FPROC(GLint location, GLfloat v0, GLfloat v1);
		[GL_VERSION_2_0]	public delegate void PFNGLUNIFORM3FPROC(GLint location, GLfloat v0, GLfloat v1, GLfloat v2);
		[GL_VERSION_2_0]	public delegate void PFNGLUNIFORM4FPROC(GLint location, GLfloat v0, GLfloat v1, GLfloat v2, GLfloat v3);
		[GL_VERSION_2_0]	public delegate void PFNGLUNIFORM1IPROC(GLint location, GLint v0);
		[GL_VERSION_2_0]	public delegate void PFNGLUNIFORM2IPROC(GLint location, GLint v0, GLint v1);
		[GL_VERSION_2_0]	public delegate void PFNGLUNIFORM3IPROC(GLint location, GLint v0, GLint v1, GLint v2);
		[GL_VERSION_2_0]	public delegate void PFNGLUNIFORM4IPROC(GLint location, GLint v0, GLint v1, GLint v2, GLint v3);
		[GL_VERSION_2_0]	unsafe public delegate void PFNGLUNIFORM1FVPROC(GLint location, GLsizei count, GLfloat* value);
		[GL_VERSION_2_0]	unsafe public delegate void PFNGLUNIFORM2FVPROC(GLint location, GLsizei count, GLfloat* value);
		[GL_VERSION_2_0]	unsafe public delegate void PFNGLUNIFORM3FVPROC(GLint location, GLsizei count, GLfloat* value);
		[GL_VERSION_2_0]	unsafe public delegate void PFNGLUNIFORM4FVPROC(GLint location, GLsizei count, GLfloat* value);
		[GL_VERSION_2_0]	public delegate void PFNGLUNIFORM1IVPROC(GLint location, GLsizei count, GLint[] value);
		[GL_VERSION_2_0]	public delegate void PFNGLUNIFORM2IVPROC(GLint location, GLsizei count, GLint[] value);
		[GL_VERSION_2_0]	public delegate void PFNGLUNIFORM3IVPROC(GLint location, GLsizei count, GLint[] value);
		[GL_VERSION_2_0]	public delegate void PFNGLUNIFORM4IVPROC(GLint location, GLsizei count, GLint[] value);
		[GL_VERSION_2_0]	public delegate void PFNGLUNIFORMMATRIX2FVPROC(GLint location, GLsizei count, GLboolean transpose, GLfloat[] value);
		[GL_VERSION_2_0]	public delegate void PFNGLUNIFORMMATRIX3FVPROC(GLint location, GLsizei count, GLboolean transpose, GLfloat[] value);
		[GL_VERSION_2_0]	public delegate void PFNGLUNIFORMMATRIX4FVPROC(GLint location, GLsizei count, GLboolean transpose, GLfloat[] value);
		//[GL_VERSION_2_0]	public delegate void PFNGLVALIDATEPROGRAMPROC(GLuint program);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB1DPROC(GLuint index, GLdouble x);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB1DVPROC(GLuint index, const GLdouble *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB1FPROC(GLuint index, GLfloat x);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB1FVPROC(GLuint index, const GLfloat *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB1SPROC(GLuint index, GLshort x);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB1SVPROC(GLuint index, const GLshort *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB2DPROC(GLuint index, GLdouble x, GLdouble y);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB2DVPROC(GLuint index, const GLdouble *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB2FPROC(GLuint index, GLfloat x, GLfloat y);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB2FVPROC(GLuint index, const GLfloat *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB2SPROC(GLuint index, GLshort x, GLshort y);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB2SVPROC(GLuint index, const GLshort *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB3DPROC(GLuint index, GLdouble x, GLdouble y, GLdouble z);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB3DVPROC(GLuint index, const GLdouble *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB3FPROC(GLuint index, GLfloat x, GLfloat y, GLfloat z);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB3FVPROC(GLuint index, const GLfloat *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB3SPROC(GLuint index, GLshort x, GLshort y, GLshort z);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB3SVPROC(GLuint index, const GLshort *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB4NBVPROC(GLuint index, const GLbyte *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB4NIVPROC(GLuint index, const GLint *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB4NSVPROC(GLuint index, const GLshort *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB4NUBPROC(GLuint index, GLubyte x, GLubyte y, GLubyte z, GLubyte w);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB4NUBVPROC(GLuint index, const GLubyte *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB4NUIVPROC(GLuint index, const GLuint *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB4NUSVPROC(GLuint index, const GLushort *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB4BVPROC(GLuint index, const GLbyte *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB4DPROC(GLuint index, GLdouble x, GLdouble y, GLdouble z, GLdouble w);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB4DVPROC(GLuint index, const GLdouble *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB4FPROC(GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB4FVPROC(GLuint index, const GLfloat *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB4IVPROC(GLuint index, const GLint *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB4SPROC(GLuint index, GLshort x, GLshort y, GLshort z, GLshort w);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB4SVPROC(GLuint index, const GLshort *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB4UBVPROC(GLuint index, const GLubyte *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB4UIVPROC(GLuint index, const GLuint *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIB4USVPROC(GLuint index, const GLushort *v);
		//[GL_VERSION_2_0]	public delegate void PFNGLVERTEXATTRIBPOINTERPROC(GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const void *pointer);

	}
	/// <summary>
	/// GL_VERSION_2_1
	/// </summary>
	[GL_VERSION_2_1]
	public partial class OpenGL
	{
		class GL_VERSION_2_1 : OpenGLExtension {
			public GL_VERSION_2_1() : base("GL_VERSION_2_1") { }
		}

		[GL_VERSION_2_1]	unsafe public static PFNGLUNIFORMMATRIX2X3FVPROC glUniformMatrix2x3fv;	// (GLint location, GLsizei count, GLboolean transpose, GLfloat *value)
		[GL_VERSION_2_1]	unsafe public static PFNGLUNIFORMMATRIX3X2FVPROC glUniformMatrix3x2fv;	// (GLint location, GLsizei count, GLboolean transpose, GLfloat *value)
		[GL_VERSION_2_1]	unsafe public static PFNGLUNIFORMMATRIX2X4FVPROC glUniformMatrix2x4fv;	// (GLint location, GLsizei count, GLboolean transpose, GLfloat *value)
		[GL_VERSION_2_1]	unsafe public static PFNGLUNIFORMMATRIX4X2FVPROC glUniformMatrix4x2fv;	// (GLint location, GLsizei count, GLboolean transpose, GLfloat *value)
		[GL_VERSION_2_1]	unsafe public static PFNGLUNIFORMMATRIX3X4FVPROC glUniformMatrix3x4fv;	// (GLint location, GLsizei count, GLboolean transpose, GLfloat *value)
		[GL_VERSION_2_1]	unsafe public static PFNGLUNIFORMMATRIX4X3FVPROC glUniformMatrix4x3fv;	// (GLint location, GLsizei count, GLboolean transpose, GLfloat *value)

		[GL_VERSION_2_1]	unsafe public delegate void PFNGLUNIFORMMATRIX2X3FVPROC(GLint location, GLsizei count, GLboolean transpose, GLfloat *value);
		[GL_VERSION_2_1]	unsafe public delegate void PFNGLUNIFORMMATRIX3X2FVPROC(GLint location, GLsizei count, GLboolean transpose, GLfloat *value);
		[GL_VERSION_2_1]	unsafe public delegate void PFNGLUNIFORMMATRIX2X4FVPROC(GLint location, GLsizei count, GLboolean transpose, GLfloat *value);
		[GL_VERSION_2_1]	unsafe public delegate void PFNGLUNIFORMMATRIX4X2FVPROC(GLint location, GLsizei count, GLboolean transpose, GLfloat *value);
		[GL_VERSION_2_1]	unsafe public delegate void PFNGLUNIFORMMATRIX3X4FVPROC(GLint location, GLsizei count, GLboolean transpose, GLfloat *value);
		[GL_VERSION_2_1]	unsafe public delegate void PFNGLUNIFORMMATRIX4X3FVPROC(GLint location, GLsizei count, GLboolean transpose, GLfloat *value);
	}
	/// <summary>
	/// GL_VERSION_3_0
	/// </summary>
	public partial class OpenGL
	{
		/*
GLAPI void APIENTRY glColorMaski (GLuint index, GLboolean r, GLboolean g, GLboolean b, GLboolean a);
GLAPI void APIENTRY glGetBooleani_v (GLenum target, GLuint index, GLboolean *data);
GLAPI void APIENTRY glGetIntegeri_v (GLenum target, GLuint index, GLint *data);
GLAPI void APIENTRY glEnablei (GLenum target, GLuint index);
GLAPI void APIENTRY glDisablei (GLenum target, GLuint index);
GLAPI GLboolean APIENTRY glIsEnabledi (GLenum target, GLuint index);
GLAPI void APIENTRY glBeginTransformFeedback (GLenum primitiveMode);
GLAPI void APIENTRY glEndTransformFeedback (void);
GLAPI void APIENTRY glBindBufferRange (GLenum target, GLuint index, GLuint buffer, GLintptr offset, GLsizeiptr size);
GLAPI void APIENTRY glBindBufferBase (GLenum target, GLuint index, GLuint buffer);
GLAPI void APIENTRY glTransformFeedbackVaryings (GLuint program, GLsizei count, const GLchar* *varyings, GLenum bufferMode);
GLAPI void APIENTRY glGetTransformFeedbackVarying (GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLsizei *size, GLenum *type, GLchar *name);
GLAPI void APIENTRY glClampColor (GLenum target, GLenum clamp);
GLAPI void APIENTRY glBeginConditionalRender (GLuint id, GLenum mode);
GLAPI void APIENTRY glEndConditionalRender (void);
GLAPI void APIENTRY glVertexAttribIPointer (GLuint index, GLint size, GLenum type, GLsizei stride, const void *pointer);
GLAPI void APIENTRY glGetVertexAttribIiv (GLuint index, GLenum pname, GLint *params);
GLAPI void APIENTRY glGetVertexAttribIuiv (GLuint index, GLenum pname, GLuint *params);
GLAPI void APIENTRY glVertexAttribI1i (GLuint index, GLint x);
GLAPI void APIENTRY glVertexAttribI2i (GLuint index, GLint x, GLint y);
GLAPI void APIENTRY glVertexAttribI3i (GLuint index, GLint x, GLint y, GLint z);
GLAPI void APIENTRY glVertexAttribI4i (GLuint index, GLint x, GLint y, GLint z, GLint w);
GLAPI void APIENTRY glVertexAttribI1ui (GLuint index, GLuint x);
GLAPI void APIENTRY glVertexAttribI2ui (GLuint index, GLuint x, GLuint y);
GLAPI void APIENTRY glVertexAttribI3ui (GLuint index, GLuint x, GLuint y, GLuint z);
GLAPI void APIENTRY glVertexAttribI4ui (GLuint index, GLuint x, GLuint y, GLuint z, GLuint w);
GLAPI void APIENTRY glVertexAttribI1iv (GLuint index, const GLint *v);
GLAPI void APIENTRY glVertexAttribI2iv (GLuint index, const GLint *v);
GLAPI void APIENTRY glVertexAttribI3iv (GLuint index, const GLint *v);
GLAPI void APIENTRY glVertexAttribI4iv (GLuint index, const GLint *v);
GLAPI void APIENTRY glVertexAttribI1uiv (GLuint index, const GLuint *v);
GLAPI void APIENTRY glVertexAttribI2uiv (GLuint index, const GLuint *v);
GLAPI void APIENTRY glVertexAttribI3uiv (GLuint index, const GLuint *v);
GLAPI void APIENTRY glVertexAttribI4uiv (GLuint index, const GLuint *v);
GLAPI void APIENTRY glVertexAttribI4bv (GLuint index, const GLbyte *v);
GLAPI void APIENTRY glVertexAttribI4sv (GLuint index, const GLshort *v);
GLAPI void APIENTRY glVertexAttribI4ubv (GLuint index, const GLubyte *v);
GLAPI void APIENTRY glVertexAttribI4usv (GLuint index, const GLushort *v);
GLAPI void APIENTRY glGetUniformuiv (GLuint program, GLint location, GLuint *params);
GLAPI void APIENTRY glBindFragDataLocation (GLuint program, GLuint color, const GLchar *name);
GLAPI GLint APIENTRY glGetFragDataLocation (GLuint program, const GLchar *name);
GLAPI void APIENTRY glUniform1ui (GLint location, GLuint v0);
GLAPI void APIENTRY glUniform2ui (GLint location, GLuint v0, GLuint v1);
GLAPI void APIENTRY glUniform3ui (GLint location, GLuint v0, GLuint v1, GLuint v2);
GLAPI void APIENTRY glUniform4ui (GLint location, GLuint v0, GLuint v1, GLuint v2, GLuint v3);
GLAPI void APIENTRY glUniform1uiv (GLint location, GLsizei count, const GLuint *value);
GLAPI void APIENTRY glUniform2uiv (GLint location, GLsizei count, const GLuint *value);
GLAPI void APIENTRY glUniform3uiv (GLint location, GLsizei count, const GLuint *value);
GLAPI void APIENTRY glUniform4uiv (GLint location, GLsizei count, const GLuint *value);
GLAPI void APIENTRY glTexParameterIiv (GLenum target, GLenum pname, const GLint *params);
GLAPI void APIENTRY glTexParameterIuiv (GLenum target, GLenum pname, const GLuint *params);
GLAPI void APIENTRY glGetTexParameterIiv (GLenum target, GLenum pname, GLint *params);
GLAPI void APIENTRY glGetTexParameterIuiv (GLenum target, GLenum pname, GLuint *params);
GLAPI void APIENTRY glClearBufferiv (GLenum buffer, GLint drawbuffer, const GLint *value);
GLAPI void APIENTRY glClearBufferuiv (GLenum buffer, GLint drawbuffer, const GLuint *value);
GLAPI void APIENTRY glClearBufferfv (GLenum buffer, GLint drawbuffer, const GLfloat *value);
GLAPI void APIENTRY glClearBufferfi (GLenum buffer, GLint drawbuffer, GLfloat depth, GLint stencil);*/
		/*
		typedef void (APIENTRYP PFNGLCOLORMASKIPROC) (GLuint index, GLboolean r, GLboolean g, GLboolean b, GLboolean a);
		typedef void (APIENTRYP PFNGLGETBOOLEANI_VPROC) (GLenum target, GLuint index, GLboolean *data);
		typedef void (APIENTRYP PFNGLGETINTEGERI_VPROC) (GLenum target, GLuint index, GLint *data);
		typedef void (APIENTRYP PFNGLENABLEIPROC) (GLenum target, GLuint index);
		typedef void (APIENTRYP PFNGLDISABLEIPROC) (GLenum target, GLuint index);
		typedef GLboolean (APIENTRYP PFNGLISENABLEDIPROC) (GLenum target, GLuint index);
		typedef void (APIENTRYP PFNGLBEGINTRANSFORMFEEDBACKPROC) (GLenum primitiveMode);
		typedef void (APIENTRYP PFNGLENDTRANSFORMFEEDBACKPROC) (void);
		typedef void (APIENTRYP PFNGLBINDBUFFERRANGEPROC) (GLenum target, GLuint index, GLuint buffer, GLintptr offset, GLsizeiptr size);
		typedef void (APIENTRYP PFNGLBINDBUFFERBASEPROC) (GLenum target, GLuint index, GLuint buffer);
		typedef void (APIENTRYP PFNGLTRANSFORMFEEDBACKVARYINGSPROC) (GLuint program, GLsizei count, const GLchar* *varyings, GLenum bufferMode);
		typedef void (APIENTRYP PFNGLGETTRANSFORMFEEDBACKVARYINGPROC) (GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLsizei *size, GLenum *type, GLchar *name);
		typedef void (APIENTRYP PFNGLCLAMPCOLORPROC) (GLenum target, GLenum clamp);
		typedef void (APIENTRYP PFNGLBEGINCONDITIONALRENDERPROC) (GLuint id, GLenum mode);
		typedef void (APIENTRYP PFNGLENDCONDITIONALRENDERPROC) (void);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBIPOINTERPROC) (GLuint index, GLint size, GLenum type, GLsizei stride, const void *pointer);
		typedef void (APIENTRYP PFNGLGETVERTEXATTRIBIIVPROC) (GLuint index, GLenum pname, GLint *params);
		typedef void (APIENTRYP PFNGLGETVERTEXATTRIBIUIVPROC) (GLuint index, GLenum pname, GLuint *params);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBI1IPROC) (GLuint index, GLint x);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBI2IPROC) (GLuint index, GLint x, GLint y);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBI3IPROC) (GLuint index, GLint x, GLint y, GLint z);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBI4IPROC) (GLuint index, GLint x, GLint y, GLint z, GLint w);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBI1UIPROC) (GLuint index, GLuint x);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBI2UIPROC) (GLuint index, GLuint x, GLuint y);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBI3UIPROC) (GLuint index, GLuint x, GLuint y, GLuint z);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBI4UIPROC) (GLuint index, GLuint x, GLuint y, GLuint z, GLuint w);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBI1IVPROC) (GLuint index, const GLint *v);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBI2IVPROC) (GLuint index, const GLint *v);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBI3IVPROC) (GLuint index, const GLint *v);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBI4IVPROC) (GLuint index, const GLint *v);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBI1UIVPROC) (GLuint index, const GLuint *v);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBI2UIVPROC) (GLuint index, const GLuint *v);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBI3UIVPROC) (GLuint index, const GLuint *v);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBI4UIVPROC) (GLuint index, const GLuint *v);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBI4BVPROC) (GLuint index, const GLbyte *v);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBI4SVPROC) (GLuint index, const GLshort *v);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBI4UBVPROC) (GLuint index, const GLubyte *v);
		typedef void (APIENTRYP PFNGLVERTEXATTRIBI4USVPROC) (GLuint index, const GLushort *v);
		typedef void (APIENTRYP PFNGLGETUNIFORMUIVPROC) (GLuint program, GLint location, GLuint *params);
		typedef void (APIENTRYP PFNGLBINDFRAGDATALOCATIONPROC) (GLuint program, GLuint color, const GLchar *name);
		typedef GLint (APIENTRYP PFNGLGETFRAGDATALOCATIONPROC) (GLuint program, const GLchar *name);
		typedef void (APIENTRYP PFNGLUNIFORM1UIPROC) (GLint location, GLuint v0);
		typedef void (APIENTRYP PFNGLUNIFORM2UIPROC) (GLint location, GLuint v0, GLuint v1);
		typedef void (APIENTRYP PFNGLUNIFORM3UIPROC) (GLint location, GLuint v0, GLuint v1, GLuint v2);
		typedef void (APIENTRYP PFNGLUNIFORM4UIPROC) (GLint location, GLuint v0, GLuint v1, GLuint v2, GLuint v3);
		typedef void (APIENTRYP PFNGLUNIFORM1UIVPROC) (GLint location, GLsizei count, const GLuint *value);
		typedef void (APIENTRYP PFNGLUNIFORM2UIVPROC) (GLint location, GLsizei count, const GLuint *value);
		typedef void (APIENTRYP PFNGLUNIFORM3UIVPROC) (GLint location, GLsizei count, const GLuint *value);
		typedef void (APIENTRYP PFNGLUNIFORM4UIVPROC) (GLint location, GLsizei count, const GLuint *value);
		typedef void (APIENTRYP PFNGLTEXPARAMETERIIVPROC) (GLenum target, GLenum pname, const GLint *params);
		typedef void (APIENTRYP PFNGLTEXPARAMETERIUIVPROC) (GLenum target, GLenum pname, const GLuint *params);
		typedef void (APIENTRYP PFNGLGETTEXPARAMETERIIVPROC) (GLenum target, GLenum pname, GLint *params);
		typedef void (APIENTRYP PFNGLGETTEXPARAMETERIUIVPROC) (GLenum target, GLenum pname, GLuint *params);
		typedef void (APIENTRYP PFNGLCLEARBUFFERIVPROC) (GLenum buffer, GLint drawbuffer, const GLint *value);
		typedef void (APIENTRYP PFNGLCLEARBUFFERUIVPROC) (GLenum buffer, GLint drawbuffer, const GLuint *value);
		typedef void (APIENTRYP PFNGLCLEARBUFFERFVPROC) (GLenum buffer, GLint drawbuffer, const GLfloat *value);
		typedef void (APIENTRYP PFNGLCLEARBUFFERFIPROC) (GLenum buffer, GLint drawbuffer, GLfloat depth, GLint stencil);*/

		public static String GetString(GLenum name, GLuint index) {
			IntPtr s = glGetStringi(name, index);
			return Marshal.PtrToStringAnsi(s);
		}
		public delegate IntPtr PFNGLGETSTRINGIPROC(GLenum name, GLuint index);
		public static PFNGLGETSTRINGIPROC glGetStringi;
	}

	/// <summary>
	/// GL_VERSION_3_1
	/// </summary>
	public partial class OpenGL
	{
		/*
GLAPI void APIENTRY glDrawArraysInstanced (GLenum mode, GLint first, GLsizei count, GLsizei primcount);
GLAPI void APIENTRY glDrawElementsInstanced (GLenum mode, GLsizei count, GLenum type, const void *indices, GLsizei primcount);
GLAPI void APIENTRY glTexBuffer (GLenum target, GLenum internalformat, GLuint buffer);
GLAPI void APIENTRY glPrimitiveRestartIndex (GLuint index);

typedef void (APIENTRYP PFNGLDRAWARRAYSINSTANCEDPROC) (GLenum mode, GLint first, GLsizei count, GLsizei primcount);
typedef void (APIENTRYP PFNGLDRAWELEMENTSINSTANCEDPROC) (GLenum mode, GLsizei count, GLenum type, const void *indices, GLsizei primcount);
typedef void (APIENTRYP PFNGLTEXBUFFERPROC) (GLenum target, GLenum internalformat, GLuint buffer);
typedef void (APIENTRYP PFNGLPRIMITIVERESTARTINDEXPROC) (GLuint index);
		*/
	}

	/// <summary>
	/// GL_VERSION_3_2
	/// </summary>
	public partial class OpenGL
	{
		/*
GLAPI void APIENTRY glGetInteger64i_v (GLenum target, GLuint index, GLint64 *data);
GLAPI void APIENTRY glGetBufferParameteri64v (GLenum target, GLenum pname, GLint64 *params);
GLAPI void APIENTRY glFramebufferTexture (GLenum target, GLenum attachment, GLuint texture, GLint level);

typedef void (APIENTRYP PFNGLGETINTEGER64I_VPROC) (GLenum target, GLuint index, GLint64 *data);
typedef void (APIENTRYP PFNGLGETBUFFERPARAMETERI64VPROC) (GLenum target, GLenum pname, GLint64 *params);
typedef void (APIENTRYP PFNGLFRAMEBUFFERTEXTUREPROC) (GLenum target, GLenum attachment, GLuint texture, GLint level);
		*/
	}

	/// <summary>
	/// GL_VERSION_3_3
	/// </summary>
	public partial class OpenGL
	{
		/*
GLAPI void APIENTRY glVertexAttribDivisor (GLuint index, GLuint divisor);

typedef void (APIENTRYP PFNGLVERTEXATTRIBDIVISORPROC) (GLuint index, GLuint divisor);
		*/
	}

	/// <summary>
	/// GL_VERSION_4_0
	/// </summary>
	public partial class OpenGL
	{
		/*
GLAPI void APIENTRY glMinSampleShading (GLclampf value);
GLAPI void APIENTRY glBlendEquationi (GLuint buf, GLenum mode);
GLAPI void APIENTRY glBlendEquationSeparatei (GLuint buf, GLenum modeRGB, GLenum modeAlpha);
GLAPI void APIENTRY glBlendFunci (GLuint buf, GLenum src, GLenum dst);
GLAPI void APIENTRY glBlendFuncSeparatei (GLuint buf, GLenum srcRGB, GLenum dstRGB, GLenum srcAlpha, GLenum dstAlpha);

typedef void (APIENTRYP PFNGLMINSAMPLESHADINGPROC) (GLclampf value);
typedef void (APIENTRYP PFNGLBLENDEQUATIONIPROC) (GLuint buf, GLenum mode);
typedef void (APIENTRYP PFNGLBLENDEQUATIONSEPARATEIPROC) (GLuint buf, GLenum modeRGB, GLenum modeAlpha);
typedef void (APIENTRYP PFNGLBLENDFUNCIPROC) (GLuint buf, GLenum src, GLenum dst);
typedef void (APIENTRYP PFNGLBLENDFUNCSEPARATEIPROC) (GLuint buf, GLenum srcRGB, GLenum dstRGB, GLenum srcAlpha, GLenum dstAlpha);
		*/
	}

	/// <summary>
	/// GL_VERSION_4_1
	/// </summary>
	public partial class OpenGL
	{
		/*
		*/
	}

	/// <summary>
	/// GL_VERSION_ALL
	/// </summary>
	public partial class OpenGL
	{
		#region Window

		public static event System.Action Render = null;
		public static event System.Action OnIdle = null;
		public static event System.Action OnInit = null;
		static iaaa.framework.EventManager eventmanager;
		public static void Execute(System.Action action)
		{
			eventmanager.Invoke(() => call.Do(action));
		}

		static Window call = null;	// создано в потоке имент-манагера
		static Window view = null;
		static OpenGL()
		{
			eventmanager = new iaaa.framework.EventManager("OpenGL EM", true);
			eventmanager.Invoke(() => call = new Window());	// контекст OpenGL
			view = new Window(0);		// главное OpenGL окошко для рисования
		}

		#endregion

		public static Window View
		{ get {
			return view;
		} }

		#region Объектные обертки
/*		public static vector ClearColor
		{
			set
			{
				ClearColor((float)value.x, (float)value.y, (float)value.z, (float)1);
			}
		}
		public static double ClearDepth
		{
			set
			{
				ClearDepth(value);
			}
		}*/

		unsafe public static void MultMatrix(Matrix m)
		{
			fixed (double* ptr = &m.m11)
			{
				OpenGL.glMultMatrixd(ptr);
			}
		}

		public static void Translate(vector v) {
			Translate(v.x, v.y, v.z);
		}

		public static void Vertex(vector v)
		{
			glVertex3d(v.x, v.y, v.z);
		}

		public static void Color(vector c)
		{
			glColor3d(c.x, c.y, c.z);
		}

		public static void Scale(double d)
		{
			glScaled(d, d, d);
		}

		public static uint listSphere = 0;
		public static void Sphere(double radius)
		{
			if (listSphere == 0)
			{
				listSphere = glGenLists(1);
				glNewList(listSphere, GL_COMPILE_AND_EXECUTE);

				glDisable(GL_TEXTURE_2D);
				uint quadric = gluNewQuadric();
				gluQuadricTexture(quadric, false);
				gluQuadricDrawStyle(quadric, GLU_LINE);
				gluSphere(quadric, 1.0, 16, 16);
				gluDeleteQuadric(quadric);

				glEndList();
			}
			PushMatrix();
			glScaled(radius, radius, radius);
			glCallList(listSphere);
			PopMatrix();
		}
		#endregion
	}

	/*	public struct color4 : vector
		{

		}*/

	public static class OpenGLHelper
	{
	}

	public partial class OpenGL
	{
		public static String GetString(uint name) { return Marshal.PtrToStringAnsi(glGetString(name)); }
	}

	/// <summary>
	/// GL_ARB_shader_objects
	/// </summary>
	public partial class OpenGL
	{
		class GL_ARB_shader_objects : OpenGLExtension {
			public GL_ARB_shader_objects() : base("GL_ARB_shader_objects"){}
		}

		public const GLenum GL_PROGRAM_OBJECT_ARB			= 0x8B40;
		public const GLenum GL_SHADER_OBJECT_ARB			= 0x8B48;
		public const GLenum GL_OBJECT_TYPE_ARB				= 0x8B4E;
		public const GLenum GL_OBJECT_SUBTYPE_ARB			= 0x8B4F;
		public const GLenum GL_FLOAT_VEC2_ARB				= 0x8B50;
		public const GLenum GL_FLOAT_VEC3_ARB				= 0x8B51;
		public const GLenum GL_FLOAT_VEC4_ARB				= 0x8B52;
		public const GLenum GL_INT_VEC2_ARB					= 0x8B53;
		public const GLenum GL_INT_VEC3_ARB					= 0x8B54;
		public const GLenum GL_INT_VEC4_ARB					= 0x8B55;
		public const GLenum GL_BOOL_ARB						= 0x8B56;
		public const GLenum GL_BOOL_VEC2_ARB				= 0x8B57;
		public const GLenum GL_BOOL_VEC3_ARB				= 0x8B58;
		public const GLenum GL_BOOL_VEC4_ARB				= 0x8B59;
		public const GLenum GL_FLOAT_MAT2_ARB				= 0x8B5A;
		public const GLenum GL_FLOAT_MAT3_ARB				= 0x8B5B;
		public const GLenum GL_FLOAT_MAT4_ARB				= 0x8B5C;
		public const GLenum GL_SAMPLER_1D_ARB				= 0x8B5D;
		public const GLenum GL_SAMPLER_2D_ARB				= 0x8B5E;
		public const GLenum GL_SAMPLER_3D_ARB				= 0x8B5F;
		public const GLenum GL_SAMPLER_CUBE_ARB				= 0x8B60;
		public const GLenum GL_SAMPLER_1D_SHADOW_ARB		= 0x8B61;
		public const GLenum GL_SAMPLER_2D_SHADOW_ARB		= 0x8B62;
		public const GLenum GL_SAMPLER_2D_RECT_ARB			= 0x8B63;
		public const GLenum GL_SAMPLER_2D_RECT_SHADOW_ARB	= 0x8B64;
		public const GLenum GL_OBJECT_DELETE_STATUS_ARB		= 0x8B80;
		public const GLenum GL_OBJECT_COMPILE_STATUS_ARB	= 0x8B81;
		public const GLenum GL_OBJECT_LINK_STATUS_ARB		= 0x8B82;
		public const GLenum GL_OBJECT_VALIDATE_STATUS_ARB	= 0x8B83;
		public const GLenum GL_OBJECT_INFO_LOG_LENGTH_ARB	= 0x8B84;
		public const GLenum GL_OBJECT_ATTACHED_OBJECTS_ARB	= 0x8B85;
		public const GLenum GL_OBJECT_ACTIVE_UNIFORMS_ARB	= 0x8B86;
		public const GLenum GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB = 0x8B87;
		public const GLenum GL_OBJECT_SHADER_SOURCE_LENGTH_ARB = 0x8B88;

		[GL_ARB_shader_objects]	public static PFNGLDELETEOBJECTARBPROC glDeleteObjectARB;
		public delegate void PFNGLDELETEOBJECTARBPROC(GLhandleARB obj);
		public static void DeleteObjectARB(GLhandleARB obj)
		{
			glDeleteObjectARB(obj);
		}

		[GL_ARB_shader_objects]	public static PFNGLGETHANDLEARBPROC glGetHandleARB;
		public delegate GLhandleARB PFNGLGETHANDLEARBPROC(GLenum pname);
		public static GLhandleARB GetHandleARB(GLenum pname)
		{
			return glGetHandleARB(pname);
		}

		[GL_ARB_shader_objects]	public delegate void PFNGLDETACHOBJECTARBPROC(GLhandleARB containerObj, GLhandleARB attachedObj);
		[GL_ARB_shader_objects]	public delegate GLhandleARB PFNGLCREATESHADEROBJECTARBPROC(GLenum shaderType);
		[GL_ARB_shader_objects]	unsafe public delegate void PFNGLSHADERSOURCEARBPROC(GLhandleARB shaderObj, GLsizei count, ref GLstringARB str, void* unused);
		[GL_ARB_shader_objects]	public delegate void PFNGLCOMPILESHADERARBPROC(GLhandleARB shaderObj);
		[GL_ARB_shader_objects]	public delegate GLhandleARB PFNGLCREATEPROGRAMOBJECTARBPROC();
		[GL_ARB_shader_objects]	public delegate void PFNGLATTACHOBJECTARBPROC(GLhandleARB containerObj, GLhandleARB obj);
		[GL_ARB_shader_objects]	public delegate void PFNGLLINKPROGRAMARBPROC(GLhandleARB programObj);
		[GL_ARB_shader_objects]	public delegate void PFNGLUSEPROGRAMOBJECTARBPROC(GLhandleARB programObj);
		[GL_ARB_shader_objects]	public delegate void PFNGLVALIDATEPROGRAMARBPROC(GLhandleARB programObj);
		[GL_ARB_shader_objects]	public delegate void PFNGLUNIFORM1FARBPROC(GLint location, GLfloat v0);
		[GL_ARB_shader_objects]	public delegate void PFNGLUNIFORM2FARBPROC(GLint location, GLfloat v0, GLfloat v1);
		[GL_ARB_shader_objects]	public delegate void PFNGLUNIFORM3FARBPROC(GLint location, GLfloat v0, GLfloat v1, GLfloat v2);
		[GL_ARB_shader_objects]	public delegate void PFNGLUNIFORM4FARBPROC(GLint location, GLfloat v0, GLfloat v1, GLfloat v2, GLfloat v3);
		[GL_ARB_shader_objects]	public delegate void PFNGLUNIFORM1IARBPROC(GLint location, GLint v0);
		[GL_ARB_shader_objects]	public delegate void PFNGLUNIFORM2IARBPROC(GLint location, GLint v0, GLint v1);
		[GL_ARB_shader_objects]	public delegate void PFNGLUNIFORM3IARBPROC(GLint location, GLint v0, GLint v1, GLint v2);
		[GL_ARB_shader_objects]	public delegate void PFNGLUNIFORM4IARBPROC(GLint location, GLint v0, GLint v1, GLint v2, GLint v3);
		[GL_ARB_shader_objects]	public delegate void PFNGLUNIFORM1FVARBPROC(GLint location, GLsizei count, GLfloat []value);
		[GL_ARB_shader_objects]	public delegate void PFNGLUNIFORM2FVARBPROC(GLint location, GLsizei count, GLfloat []value);
		[GL_ARB_shader_objects]	public delegate void PFNGLUNIFORM3FVARBPROC(GLint location, GLsizei count, GLfloat []value);
		[GL_ARB_shader_objects]	public delegate void PFNGLUNIFORM4FVARBPROC(GLint location, GLsizei count, GLfloat []value);
		[GL_ARB_shader_objects]	public delegate void PFNGLUNIFORM1IVARBPROC(GLint location, GLsizei count, GLint []value);
		[GL_ARB_shader_objects]	public delegate void PFNGLUNIFORM2IVARBPROC(GLint location, GLsizei count, GLint []value);
		[GL_ARB_shader_objects]	public delegate void PFNGLUNIFORM3IVARBPROC(GLint location, GLsizei count, GLint []value);
		[GL_ARB_shader_objects]	public delegate void PFNGLUNIFORM4IVARBPROC(GLint location, GLsizei count, GLint []value);
		[GL_ARB_shader_objects]	public delegate void PFNGLUNIFORMMATRIX2FVARBPROC(GLint location, GLsizei count, GLboolean transpose, GLfloat []value);
		[GL_ARB_shader_objects]	public delegate void PFNGLUNIFORMMATRIX3FVARBPROC(GLint location, GLsizei count, GLboolean transpose, GLfloat []value);
		[GL_ARB_shader_objects]	public delegate void PFNGLUNIFORMMATRIX4FVARBPROC(GLint location, GLsizei count, GLboolean transpose, GLfloat []value);
		[GL_ARB_shader_objects]	public delegate void PFNGLGETOBJECTPARAMETERFVARBPROC(GLhandleARB obj, GLenum pname, GLfloat []args);
		[GL_ARB_shader_objects]	public delegate void PFNGLGETOBJECTPARAMETERIVARBPROC(GLhandleARB obj, GLenum pname, GLint []args);
		[GL_ARB_shader_objects]	public delegate void PFNGLGETINFOLOGARBPROC(GLhandleARB obj, GLsizei maxLength, GLsizei []length, GLstringARB infoLog);
		[GL_ARB_shader_objects]	public delegate void PFNGLGETATTACHEDOBJECTSARBPROC(GLhandleARB containerObj, GLsizei maxCount, GLsizei []count, GLhandleARB []obj);
		[GL_ARB_shader_objects]	public delegate GLint PFNGLGETUNIFORMLOCATIONARBPROC(GLhandleARB programObj, GLstringARB name);
		[GL_ARB_shader_objects]	public delegate void PFNGLGETACTIVEUNIFORMARBPROC(GLhandleARB programObj, GLuint index, GLsizei maxLength, GLsizei []length, GLint []size, GLenum []type, GLstringARB name);
		[GL_ARB_shader_objects]	public delegate void PFNGLGETUNIFORMFVARBPROC(GLhandleARB programObj, GLint location, GLfloat []args);
		[GL_ARB_shader_objects]	public delegate void PFNGLGETUNIFORMIVARBPROC(GLhandleARB programObj, GLint location, GLint []args);
		[GL_ARB_shader_objects]	public delegate void PFNGLGETSHADERSOURCEARBPROC(GLhandleARB obj, GLsizei maxLength, GLsizei []length, GLstringARB source);

		[GL_ARB_shader_objects]	public static PFNGLDETACHOBJECTARBPROC glDetachObjectARB;
		[GL_ARB_shader_objects]	public static PFNGLCREATESHADEROBJECTARBPROC glCreateShaderObjectARB;
		[GL_ARB_shader_objects]	public static PFNGLSHADERSOURCEARBPROC glShaderSourceARB;
		[GL_ARB_shader_objects]	public static PFNGLCOMPILESHADERARBPROC glCompileShaderARB;
		[GL_ARB_shader_objects]	public static PFNGLCREATEPROGRAMOBJECTARBPROC glCreateProgramObjectARB;
		[GL_ARB_shader_objects]	public static PFNGLATTACHOBJECTARBPROC glAttachObjectARB;
		[GL_ARB_shader_objects]	public static PFNGLLINKPROGRAMARBPROC glLinkProgramARB;
		[GL_ARB_shader_objects]	public static PFNGLUSEPROGRAMOBJECTARBPROC glUseProgramObjectARB;
		[GL_ARB_shader_objects]	public static PFNGLVALIDATEPROGRAMARBPROC glValidateProgramARB;
		[GL_ARB_shader_objects]	public static PFNGLUNIFORM1FARBPROC glUniform1fARB;
		[GL_ARB_shader_objects]	public static PFNGLUNIFORM2FARBPROC glUniform2fARB;
		[GL_ARB_shader_objects]	public static PFNGLUNIFORM3FARBPROC glUniform3fARB;
		[GL_ARB_shader_objects]	public static PFNGLUNIFORM4FARBPROC glUniform4fARB;
		[GL_ARB_shader_objects]	public static PFNGLUNIFORM1IARBPROC glUniform1iARB;
		[GL_ARB_shader_objects]	public static PFNGLUNIFORM2IARBPROC glUniform2iARB;
		[GL_ARB_shader_objects]	public static PFNGLUNIFORM3IARBPROC glUniform3iARB;
		[GL_ARB_shader_objects]	public static PFNGLUNIFORM4IARBPROC glUniform4iARB;
		[GL_ARB_shader_objects]	public static PFNGLUNIFORM1FVARBPROC glUniform1fvARB;
		[GL_ARB_shader_objects]	public static PFNGLUNIFORM2FVARBPROC glUniform2fvARB;
		[GL_ARB_shader_objects]	public static PFNGLUNIFORM3FVARBPROC glUniform3fvARB;
		[GL_ARB_shader_objects]	public static PFNGLUNIFORM4FVARBPROC glUniform4fvARB;
		[GL_ARB_shader_objects]	public static PFNGLUNIFORM1IVARBPROC glUniform1ivARB;
		[GL_ARB_shader_objects]	public static PFNGLUNIFORM2IVARBPROC glUniform2ivARB;
		[GL_ARB_shader_objects]	public static PFNGLUNIFORM3IVARBPROC glUniform3ivARB;
		[GL_ARB_shader_objects]	public static PFNGLUNIFORM4IVARBPROC glUniform4ivARB;
		[GL_ARB_shader_objects]	public static PFNGLUNIFORMMATRIX2FVARBPROC glUniformMatrix2fvARB;
		[GL_ARB_shader_objects]	public static PFNGLUNIFORMMATRIX3FVARBPROC glUniformMatrix3fvARB;
		[GL_ARB_shader_objects]	public static PFNGLUNIFORMMATRIX4FVARBPROC glUniformMatrix4fvARB;
		[GL_ARB_shader_objects]	public static PFNGLGETOBJECTPARAMETERFVARBPROC glGetObjectParameterfvARB;
		[GL_ARB_shader_objects]	public static PFNGLGETOBJECTPARAMETERIVARBPROC glGetObjectParameterivARB;
		[GL_ARB_shader_objects]	public static PFNGLGETINFOLOGARBPROC glGetInfoLogARB;
		[GL_ARB_shader_objects]	public static PFNGLGETATTACHEDOBJECTSARBPROC glGetAttachedObjectsARB;
		[GL_ARB_shader_objects]	public static PFNGLGETUNIFORMLOCATIONARBPROC glGetUniformLocationARB;
		[GL_ARB_shader_objects]	public static PFNGLGETACTIVEUNIFORMARBPROC glGetActiveUniformARB;
		[GL_ARB_shader_objects]	public static PFNGLGETUNIFORMFVARBPROC glGetUniformfvARB;
		[GL_ARB_shader_objects]	public static PFNGLGETUNIFORMIVARBPROC glGetUniformivARB;
		[GL_ARB_shader_objects]	public static PFNGLGETSHADERSOURCEARBPROC glGetShaderSourceARB;

		// glUniform?fARB
		public static void UniformARB(GLint location, GLfloat v0) {
			glUniform1fARB(location, v0);
		}
		public static void UniformARB(GLint location, GLfloat v0, GLfloat v1) {
			glUniform2fARB(location, v0, v1);
		}
		public static void UniformARB(GLint location, GLfloat v0, GLfloat v1, GLfloat v2) {
			glUniform3fARB(location, v0, v1, v2);
		}
		public static void UniformARB(GLint location, GLfloat v0, GLfloat v1, GLfloat v2, GLfloat v3) {
			glUniform4fARB(location, v0, v1, v2, v3);
		}
		// glUniform?iARB
		public static void UniformARB(GLint location, GLint v0) {
			glUniform1iARB(location, v0);
		}
		public static void UniformARB(GLint location, GLint v0, GLint v1) {
			glUniform2iARB(location, v0, v1);
		}
		public static void UniformARB(GLint location, GLint v0, GLint v1, GLint v2) {
			glUniform3iARB(location, v0, v1, v2);
		}
		public static void UniformARB(GLint location, GLint v0, GLint v1, GLint v2, GLint v3) {
			glUniform4iARB(location, v0, v1, v2, v3);
		}
	}

	/// <summary>
	/// GL_ARB_vertex_shader
	/// </summary>
	public partial class OpenGL
	{
		class GL_ARB_vertex_shader : OpenGLExtension {
			public GL_ARB_vertex_shader() : base("GL_ARB_vertex_shader") { }
		}

		public const GLenum GL_VERTEX_SHADER_ARB = 0x8B31;
		public const GLenum GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB = 0x8B4A;
		public const GLenum GL_MAX_VARYING_FLOATS_ARB = 0x8B4B;
		public const GLenum GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB = 0x8B4C;
		public const GLenum GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB = 0x8B4D;
		public const GLenum GL_OBJECT_ACTIVE_ATTRIBUTES_ARB = 0x8B89;
		public const GLenum GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB = 0x8B8A;

		[GL_ARB_vertex_shader] public static PFNGLBINDATTRIBLOCATIONARBPROC glBindAttribLocationARB;
		[GL_ARB_vertex_shader] public static PFNGLGETACTIVEATTRIBARBPROC glGetActiveAttribARB;
		[GL_ARB_vertex_shader] public static PFNGLGETATTRIBLOCATIONARBPROC glGetAttribLocationARB;

		public unsafe delegate void PFNGLBINDATTRIBLOCATIONARBPROC(GLhandleARB programObj, GLuint index, GLstringARB name);
		public unsafe delegate void PFNGLGETACTIVEATTRIBARBPROC(GLhandleARB programObj, GLuint index, GLsizei maxLength, GLsizei* length, GLint* size, GLenum* type, GLstringARB name);
		public unsafe delegate GLint PFNGLGETATTRIBLOCATIONARBPROC(GLhandleARB programObj, GLstringARB name);
	}

	/// <summary>
	/// GL_ARB_fragment_shader
	/// </summary>
	public partial class OpenGL
	{
		class GL_ARB_fragment_shader : OpenGLExtension {
			public GL_ARB_fragment_shader() : base("GL_ARB_fragment_shader") { }
		}

		public const GLenum GL_FRAGMENT_SHADER_ARB = 0x8B30;
		public const GLenum GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB = 0x8B49;
		public const GLenum GL_FRAGMENT_SHADER_DERIVATIVE_HINT_ARB = 0x8B8B;
	}
	
	/// <summary>
	/// GL_ARB_shading_language_100
	/// </summary>
	public partial class OpenGL
	{
		class GL_ARB_shading_language_100 : OpenGLExtension {
			public GL_ARB_shading_language_100() : base("GL_ARB_shading_language_100") { }
		}

		public const GLenum GL_SHADING_LANGUAGE_VERSION_ARB = 0x8B8C;


	}


	public partial class OpenGL
	{
		public class Cubemap
		{
			uint texture = 0;
			int size = 0;
			public Cubemap(int size)
			{
				this.size = size;

				OpenGL.glGenTextures(1, out texture);
				OpenGL.glBindTexture(OpenGL.GL_TEXTURE_CUBE_MAP, texture);

				// todo: вынести в настройки
				OpenGL.glTexParameteri(OpenGL.GL_TEXTURE_CUBE_MAP, OpenGL.GL_TEXTURE_WRAP_S, OpenGL.GL_CLAMP_TO_EDGE);
				OpenGL.glTexParameteri(OpenGL.GL_TEXTURE_CUBE_MAP, OpenGL.GL_TEXTURE_WRAP_T, OpenGL.GL_CLAMP_TO_EDGE);
				OpenGL.glTexParameteri(OpenGL.GL_TEXTURE_CUBE_MAP, OpenGL.GL_TEXTURE_WRAP_R, OpenGL.GL_CLAMP_TO_EDGE);
				OpenGL.glTexParameteri(OpenGL.GL_TEXTURE_CUBE_MAP, OpenGL.GL_TEXTURE_MAG_FILTER, OpenGL.GL_LINEAR);
				OpenGL.glTexParameteri(OpenGL.GL_TEXTURE_CUBE_MAP, OpenGL.GL_TEXTURE_MIN_FILTER, OpenGL.GL_LINEAR);

				// null подразумевает просто распределение места для текстуры (значение цветов точек не определено)
				unsafe {
					OpenGL.glTexImage2D(OpenGL.GL_TEXTURE_CUBE_MAP_POSITIVE_X, 0, OpenGL.GL_RGBA8, 256, 256, 0, OpenGL.GL_BGRA, OpenGL.GL_UNSIGNED_BYTE, (void*)null);
					OpenGL.glTexImage2D(OpenGL.GL_TEXTURE_CUBE_MAP_NEGATIVE_X, 0, OpenGL.GL_RGBA8, 256, 256, 0, OpenGL.GL_BGRA, OpenGL.GL_UNSIGNED_BYTE, (void*)null);
					OpenGL.glTexImage2D(OpenGL.GL_TEXTURE_CUBE_MAP_POSITIVE_Y, 0, OpenGL.GL_RGBA8, 256, 256, 0, OpenGL.GL_BGRA, OpenGL.GL_UNSIGNED_BYTE, (void*)null);
					OpenGL.glTexImage2D(OpenGL.GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, 0, OpenGL.GL_RGBA8, 256, 256, 0, OpenGL.GL_BGRA, OpenGL.GL_UNSIGNED_BYTE, (void*)null);
					OpenGL.glTexImage2D(OpenGL.GL_TEXTURE_CUBE_MAP_POSITIVE_Z, 0, OpenGL.GL_RGBA8, 256, 256, 0, OpenGL.GL_BGRA, OpenGL.GL_UNSIGNED_BYTE, (void*)null);
					OpenGL.glTexImage2D(OpenGL.GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, 0, OpenGL.GL_RGBA8, 256, 256, 0, OpenGL.GL_BGRA, OpenGL.GL_UNSIGNED_BYTE, (void*)null);
				}
				// а дальше стоит вообще-то разобраться со всеми этими параметрами!
//				OpenGL.glTexEnvf(OpenGL.GL_TEXTURE_ENV, OpenGL.GL_TEXTURE_ENV_MODE, OpenGL.GL_MODULATE); 
			}

			/// <summary>
			/// Построить кубическую текстуру
			/// </summary>
			/// <param name="action">рендерер</param>
			public void Build(System.Action action)
			{
				OpenGL.glViewport(0, 0, size, size);
				OpenGL.MatrixMode(OpenGL.GL_PROJECTION);
				OpenGL.LoadIdentity();
				OpenGL.gluPerspective(90, 1, 0.1f, 3e10f);	// todo: вынести в настройки
				OpenGL.MatrixMode(OpenGL.GL_MODELVIEW);

				OpenGL.ClearColor(0.0f, 0.0f, 0.0f, 0.0f);

				// todo: развернуть цикл (оптимизация скорости)
				for (uint i = OpenGL.GL_TEXTURE_CUBE_MAP_POSITIVE_X; i <= OpenGL.GL_TEXTURE_CUBE_MAP_NEGATIVE_Z; i++)
				{
					OpenGL.Clear(OpenGL.GL_COLOR_BUFFER_BIT | OpenGL.GL_DEPTH_BUFFER_BIT);
					OpenGL.LoadIdentity();
					switch (i)
					{
						case OpenGL.GL_TEXTURE_CUBE_MAP_POSITIVE_X:
							OpenGL.glRotatef(180, 0, 0, 1);
							OpenGL.glRotatef(90, 0, 1, 0);
							break;
						case OpenGL.GL_TEXTURE_CUBE_MAP_POSITIVE_Y:
							OpenGL.glRotatef(-90, 1, 0, 0);
							break;
						case OpenGL.GL_TEXTURE_CUBE_MAP_POSITIVE_Z:
							OpenGL.glRotatef(180, 1, 0, 0);
							break;
						case OpenGL.GL_TEXTURE_CUBE_MAP_NEGATIVE_X:
							OpenGL.glRotatef(180, 0, 0, 1);
							OpenGL.glRotatef(-90, 0, 1, 0);
							break;
						case OpenGL.GL_TEXTURE_CUBE_MAP_NEGATIVE_Y:
							OpenGL.glRotatef(90, 1, 0, 0);
							break;
						case OpenGL.GL_TEXTURE_CUBE_MAP_NEGATIVE_Z:
							OpenGL.glRotatef(180, 0, 0, 1);
							break;
					}
					action();
					// все, сохраним картинку в соответствующей части кубической текстуры
					OpenGL.glBindTexture(OpenGL.GL_TEXTURE_CUBE_MAP, texture);
					OpenGL.glCopyTexSubImage2D(i, 0, 0, 0, 0, 0, size, size);	// todo: задавать level?
				}
			}

			// 
			/// <summary>
			/// Возвращает идентификатор кубической текстуры (для биндинга в шейдер как "[uniform] int tex для textureCube()")
			/// </summary>
			public static implicit operator int(Cubemap me)
			{
				return (int)me.texture;
			}
		}
	}
}
