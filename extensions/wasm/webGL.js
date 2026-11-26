(() => {
const colored = "color: dodgerblue;"
console.log("%cwebGL.js loaded.", colored);

const canvas = document.getElementById("glcanvas"); // todo: think about custom property
if (!canvas)
  throw new Error("no GL canvas 'glcanvas' found");

gl = canvas.getContext('webgl2', { preserveDrawingBuffer: true })
  || canvas.getContext('webgl', { preserveDrawingBuffer: true });

if (!gl)
  throw new Error("no GL context");

gl.viewport(0, 0, canvas.width, canvas.height); // setup basic viewport
gl.clearColor(0.0, 0.0, 0.0, 1.0);
gl.clear(gl.COLOR_BUFFER_BIT);

registerDynamicLibrary("webGL.js", {
  // https://registry.khronos.org/webgl/specs/latest/1.0/
  glClearColor: [ "ffff->", gl.clearColor.bind(gl) ],
  glClear :     [ "i->",    (mask) => { window.toggleGL(true); gl.clear(mask) } ],

})
})();
