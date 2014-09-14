using System;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Collections.Generic;

using iaaa.framework.math;

namespace iaaa.framework.opengl
{
	// native types
	using HDC			= System.IntPtr;
	using HGLRC			= System.IntPtr;
	// opengl types
	using GLhandleARB	= System.IntPtr;
	using GLenum		= System.UInt32;
	using GLuint		= System.UInt32;

	public partial class OpenGL
	{
		public class UnknownExtension : Exception {
			public UnknownExtension(String message, params object[] args) : base(String.Format(message, args)) { }
		}
		public class ProcedureNotLoaded : Exception {
			public ProcedureNotLoaded(String message, params object[] args) : base(String.Format(message, args)) { }
		}


		private static class Extensions
		{
			#region << Native (linux/windows/osx) Part >>
			// windows
			[DllImport("opengl32.dll")]
			unsafe static extern IntPtr wglGetProcAddress(string function);

			// linux
			[DllImport("libGL.so")]
			unsafe static extern IntPtr glXGetProcAddress(string function);
			// also linux, for our ARB-y friends
			[DllImport("libGL.so")]
			unsafe static extern IntPtr glXGetProcAddressARB(string function);

			// osx gets complicated
			[DllImport("libdl.dylib")]
			unsafe static extern bool NSIsSymbolNameDefined(string function);
			[DllImport("libdl.dylib")]
			unsafe static extern IntPtr NSLookupAndBindSymbol(string function);
			[DllImport("libdl.dylib")]
			unsafe static extern IntPtr NSAddressOfSymbol(IntPtr symbol);

			// 1.0 get extension name
			[DllImport("opengl32")]
			unsafe static extern void glGetIntegerv(uint pname, int* someParams);
			[DllImport("opengl32")]
			unsafe static extern IntPtr glGetString(uint name);
			// 3.0 get extension name
			delegate IntPtr PFNGLGETSTRINGIPROC(GLenum name, GLuint index);
			unsafe static PFNGLGETSTRINGIPROC glGetStringi;

			// high level
			static String GetString(GLenum name) {				// GL_VERSION_1_0
				return Marshal.PtrToStringAnsi(glGetString(name));
			}
			static String GetString(GLenum name, GLuint index)	// GL_VERSION_3_0
			{
				return Marshal.PtrToStringAnsi(glGetStringi(name, index));
			}
			static Int32 GetInteger(GLenum name)
			{
				int i = 0;
				unsafe {
					glGetIntegerv(name, &i);
				}
				return i;
			}

			static IntPtr aglGetProcAddress(string s)
			{
				string fname = "_" + s;
				if (!NSIsSymbolNameDefined(fname))
					return IntPtr.Zero;

				IntPtr symbol = NSLookupAndBindSymbol(fname);
				if (symbol != IntPtr.Zero)
					symbol = NSAddressOfSymbol(symbol);

				return symbol;
			}

			static IEnumerable<String> EnumerateExtensions()
			{
				string gl_version = GetString(GL_VERSION);
				if (gl_version.StartsWith("1.") ||
					gl_version.StartsWith("2.")
				) {
					String extensions = GetString(OpenGL.GL_EXTENSIONS);
					foreach (var ext in extensions.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries))
						yield return ext;
				}
				else {	// Version >= 3.0
					int num = GetInteger(OpenGL.GL_NUM_EXTENSIONS);
					for (int i = 0; i < num; i++) {
						string ext = GetString(OpenGL.GL_EXTENSIONS, (uint)i);
						yield return ext;
					}
				}
			}

			#endregion

			public delegate IntPtr GetProcAddressDelegate(String function);
			public static GetProcAddressDelegate GetProcAddress;

			public static T Load<T>(String procName)
				where T:class
			{
				IntPtr p = GetProcAddress(procName);
				return (p == IntPtr.Zero ? null : Marshal.GetDelegateForFunctionPointer(p, typeof(T))) as T;
			}

			static Extensions()
			{
				for (;;) {
	                // WGL?
					try {
						wglGetProcAddress("-+-");
						GetProcAddress = wglGetProcAddress;
						break;
					}
					catch (Exception) { }
					// AGL? (before X11, since GLX might exist on OSX)
					try {
						aglGetProcAddress("-+-");
						GetProcAddress = aglGetProcAddress;
						break;
					}
					catch (Exception) { }
					// X11?
					try {
						glXGetProcAddress("-+-");
						GetProcAddress = glXGetProcAddress;
						break;
					}
					catch (Exception) { }
					// X11 ARB?
					try {
						glXGetProcAddressARB("-+-");
						GetProcAddress = glXGetProcAddressARB;
						break;
					}
					catch (Exception) { }
					// Unknown system?
					throw new NotSupportedException("No OpenGL 'glGetProcAddress' for this platform found!");
				}
				// попытаемся найти функцию перебора OpenGL3.0 расширений
				glGetStringi = Load<PFNGLGETSTRINGIPROC>("glGetStringi");
			}

			static HashSet<string> extensions = new HashSet<string>();
			public static void Load(String extension)
			{
				foreach (var ext in EnumerateExtensions()) {
					if (extension != null && ext.Equals(extension)) {
						LoadInternal(ext);
						break;
					}
				}
			}

			/// <summary>
			/// Отображение функций расширения на реальные адреса
			/// </summary>
			/// <param name="extension"></param>
			static void LoadInternal(String extension)
			{
				Type Extension = Type.GetType(typeof(OpenGL).FullName + "+" + extension);
				// проверка на "поддерживаем" расширение, пока что отключена
				if (Attribute.GetCustomAttribute(typeof(OpenGL), Extension) == null)
					throw new UnknownExtension("Unknown '{0}' extension", extension);

				FieldInfo[] fis = typeof(OpenGL).GetFields(
					BindingFlags.Public | BindingFlags.NonPublic |
					BindingFlags.DeclaredOnly |
					BindingFlags.Static);
				foreach (FieldInfo fi in fis) {
					OpenGLExtension attribute = Attribute.GetCustomAttribute(fi, typeof(OpenGLExtension)) as OpenGLExtension;
					if (attribute == null)	// не найден такой атрибут
						continue;
					if (attribute.Equals(extension)) {	// расширение подходит
						String procname = attribute.EntryPoint != null ? attribute.EntryPoint : fi.Name;
						IntPtr procaddr = Extensions.GetProcAddress(procname);
						if (procaddr == IntPtr.Zero)
							throw new ProcedureNotLoaded(procname);
						else
							fi.SetValue(null, Marshal.GetDelegateForFunctionPointer(procaddr, fi.FieldType));
					}
				}
				// Все ок, расширение загрузилось!
				extensions.Add(extension);
			}

			public static void Load()
			{
				if (Config.Version == null) {	// еси версию не попросили - поюзаем актуальную
					Config.Version = Marshal.PtrToStringAnsi(glGetString(GL_VERSION));	// why not OpenGL.glGetString ???
				}

				if (OpenGL.Config.Version >= 1.1) {
					extensions.Add("GL_VERSION_1_1");	// вроде как загружать не надо, все экспортуется из opengl32.dll
				}
				if (OpenGL.Config.Version >= 1.2)
					LoadInternal("GL_VERSION_1_2");
				if (OpenGL.Config.Version >= 1.3)
					LoadInternal("GL_VERSION_1_3");
				if (OpenGL.Config.Version >= 1.4)
					LoadInternal("GL_VERSION_1_4");
				if (OpenGL.Config.Version >= 1.5)
					LoadInternal("GL_VERSION_1_5");
				if (OpenGL.Config.Version >= 2.0)
					LoadInternal("GL_VERSION_2_0");
				if (OpenGL.Config.Version >= 2.1)
					LoadInternal("GL_VERSION_2_1");

				// И подгрузим расширения (все или только требуемые)
				if (Config.Extensions == null)
					Load(null);
				else
					foreach (var ext in Config.Extensions)
						Load(ext);
			}

			public static bool IsSupported(String extension)
			{
				return extensions.Contains(extension);
			}
		}

		#region WGL_ARB_create_context
		// http://www.opengl.org/registry/specs/ARB/wgl_create_context.txt

		public const GLenum WGL_CONTEXT_DEBUG_BIT_ARB = 0x0001;
		public const GLenum WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB = 0x0002;
		public const GLenum WGL_CONTEXT_MAJOR_VERSION_ARB = 0x2091;
		public const GLenum WGL_CONTEXT_MINOR_VERSION_ARB = 0x2092;
		public const GLenum WGL_CONTEXT_LAYER_PLANE_ARB = 0x2093;
		public const GLenum WGL_CONTEXT_FLAGS_ARB = 0x2094;

		public const GLenum GL_NUM_EXTENSIONS = 0x821D;

		public delegate HGLRC PFNWGLCREATECONTEXTATTRIBSARBPROC(HDC hDC, HGLRC hShareContext, uint[] attribList);

//		[OpenGLExtension("GL_ARB_shader_objects")]
//		public static PFNWGLCREATECONTEXTATTRIBSARBPROC wglCreateContextAttribsARB;

		#endregion
	}

    /// <summary>
    /// GL_VERSION_
    /// </summary>
    public partial class OpenGL
    {
        /*
        */
    }
    /// <summary>
    /// GL_VERSION_
    /// </summary>
    public partial class OpenGL
    {
        /*
        */
    }
}
