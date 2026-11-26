(() => {
const colored = "color: dodgerblue;"
console.log("%cwebGL.js loaded.", colored);

// setup canvas
const canvas = document.getElementById("glcanvas"); // todo: think about custom property
if (!canvas)
  throw new Error("no GL canvas 'glcanvas' found");

// init context
gl = canvas.getContext('webgl2', { preserveDrawingBuffer: true })
  || canvas.getContext('webgl', { preserveDrawingBuffer: true });

if (!gl)
  throw new Error("no GL context");

// map window
gl.viewport(0, 0, canvas.width, canvas.height); // setup basic viewport

// ------------------------------------------------
// setup wasm helper functions
const setCString = wasm.setCString;

// string buffer for temp values:
const wasmstr64 = wasm.malloc(64);


// ---gl2es ---------------------------------------------
    const vsSource = `
      attribute vec2 aPos;
      attribute vec3 aColor;
      varying vec3 vColor;
      void main() {
        vColor = aColor;
        gl_Position = vec4(aPos, 0.0, 1.0);
      }
    `;
    const fsSource = `
      precision mediump float;
      varying vec3 vColor;
      void main() {
        gl_FragColor = vec4(vColor, 1.0);
      }
    `;

    function compileShader(type, src) {
      const sh = gl.createShader(type);
      gl.shaderSource(sh, src);
      gl.compileShader(sh);
      if (!gl.getShaderParameter(sh, gl.COMPILE_STATUS)) {
        const info = gl.getShaderInfoLog(sh);
        gl.deleteShader(sh);
        throw new Error(info);
      }
      return sh;
    }

    const program = gl.createProgram();
    gl.attachShader(program, compileShader(gl.VERTEX_SHADER, vsSource));
    gl.attachShader(program, compileShader(gl.FRAGMENT_SHADER, fsSource));
    gl.linkProgram(program);
    if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
      throw new Error(gl.getProgramInfoLog(program));
    }
    gl.useProgram(program);

    const stride = 5 * Float32Array.BYTES_PER_ELEMENT;
    const locPos = gl.getAttribLocation(program, "aPos");
    const locColor = gl.getAttribLocation(program, "aColor");

// x, y, r, g, b для 3 вершин
let _vertices = [];
let _drawMode = 0;
let _color = null;

// ------------------------------------------------------
registerDynamicLibrary("webGL.js", {
  // https://registry.khronos.org/webgl/specs/latest/1.0/

  glGetString:  [ "i->s", (name) => {
      // additional data (if exists)
      // const ext = gl.getExtension('WEBGL_debug_renderer_info');
      // if (ext) {
      //   const realVendor = gl.getParameter(ext.UNMASKED_VENDOR_WEBGL);
      //   const realRenderer = gl.getParameter(ext.UNMASKED_RENDERER_WEBGL);
      // }
      let str = gl.getParameter(name);
      if (str)
        return setCString(wasmstr64, str, 64);
      return 0;
    }],

  glHint: [ "ii->", (target, mode) => 0 ],
  glViewport: [ "iiii->" , (x, y, width, height) => 0 ], // do nothing at this moment
  glGetIntegerv: [ "iv->" , (pname, data) => 0 ],


  glClearColor: [ "ffff->", gl.clearColor.bind(gl) ],
  glClear:      [ "i->",    (mask) => { window.toggleGL(true); gl.clear(mask) } ],

  glBegin: [ "i->", (mode) => {
      _vertices.length = 0;
    }],
  glEnd:  [ "->", () => {
      const floats = new Float32Array(_vertices);

      const buf = gl.createBuffer();
      gl.bindBuffer(gl.ARRAY_BUFFER, buf);
      gl.bufferData(gl.ARRAY_BUFFER, floats, gl.STATIC_DRAW);

      gl.enableVertexAttribArray(locPos);
      gl.vertexAttribPointer(locPos, 2, gl.FLOAT, false, stride, 0);

      gl.enableVertexAttribArray(locColor);
      gl.vertexAttribPointer(locColor, 3, gl.FLOAT, false, stride, 2 * Float32Array.BYTES_PER_ELEMENT);    

      gl.drawArrays(gl.TRIANGLES, 0, _vertices.length / 5);
      // clear old data
      _vertices.length = 0;
    }],

  glColor3f: [ "fff->", (r,g,b) => {
      _color = [r, g, b];
    }],
  glVertex2f: [ "ff->", (x,y) => {
      _vertices.push(x, y, ..._color);
  }]

})
})();
