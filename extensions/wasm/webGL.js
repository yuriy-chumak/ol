(() => {
const blue = "color: blue;"
console.log("%cwebGL.js loaded.", blue);

const canvas = document.getElementById("glcanvas"); // todo: think about custom property
if (!canvas) {
  console.error("no GL canvas 'glcanvas' found!")
  return;
}

gl = canvas.getContext('webgl2', { preserveDrawingBuffer: true })
  || canvas.getContext('webgl', { preserveDrawingBuffer: true });

console.log("gl: ", gl)
if (!gl) return;

gl.viewport(0, 0, canvas.width, canvas.height);
gl.clearColor(0.0, 0.0, 0.0, 1.0);
gl.clear(gl.COLOR_BUFFER_BIT);

registerDynamicLibrary("webGL.js", {
  // https://registry.khronos.org/webgl/specs/latest/1.0/
  // glClearColor: [ "ffff->", (r, g, b, a) => gl.clearColor(r, g, b, a) ],
  // glClear : [ "i->", (mask) => gl.clear(mask) ],
  glClearColor: [ "ffff->", gl.clearColor.bind(gl) ],
  glClear : [ "i->", (mask) => { console.log("%cMASK " + mask, "color: green;"); gl.clear(gl.COLOR_BUFFER_BIT) } ],

})
})();
