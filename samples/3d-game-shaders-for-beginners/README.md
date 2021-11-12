3D Game Shaders For Beginners
=============================

This is a try to translate a [lettier/3d-game-shaders-for-beginners](https://github.com/lettier/3d-game-shaders-for-beginners) to the Otus Lisp.

Interested in adding
textures,
lighting,
shadows,
normal maps,
glowing objects,
ambient occlusion,
reflections,
refractions,
and more to your 3D game?
Great!
Below is a collection of shading techniques that will take your game visuals to new heights.
I've explained each technique in such a way that you can take what you learn here and apply/port it to
whatever stack you use—be it Godot, Unity, Unreal, or something else.
For the glue in between the shaders,
I've chosen the fabulous Panda3D game engine and the OpenGL Shading Language (GLSL).
So if that is your stack, then you'll also get the benefit of learning how to use these
shading techniques with Panda3D and OpenGL specifically.

## Table Of Contents

- :heavy_check_mark: [Setup](sections/setup.md)
- :heavy_check_mark: [Dependencies](sections/dependencies.md)
- :heavy_check_mark: [Running The Demo](sections/running-the-demo.md)
- :heavy_check_mark: [Reference Frames](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/reference-frames.md)
  - :heavy_check_mark: [note](sections/reference-frames.md)
- :heavy_multiplication_x: [GLSL](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/glsl.md)
- :heavy_multiplication_x: [Render To Texture](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/render-to-texture.md)
- :heavy_multiplication_x: [Texturing](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/texturing.md)
- :heavy_multiplication_x: [Lighting](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/lighting.md)
- :heavy_multiplication_x: [Blinn-Phong](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/blinn-phong.md)
- :heavy_multiplication_x: [Fresnel Factor](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/fresnel-factor.md)
- :heavy_multiplication_x: [Rim Lighting](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/rim-lighting.md)
- :heavy_multiplication_x: [Cel Shading](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/cel-shading.md)
- :heavy_multiplication_x: [Normal Mapping](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/normal-mapping.md)
- :heavy_multiplication_x: [Deferred Rendering](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/deferred-rendering.md)
- :heavy_multiplication_x: [Fog](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/fog.md)
- :heavy_multiplication_x: [Blur](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/blur.md)
- :heavy_multiplication_x: [Bloom](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/bloom.md)
- :heavy_multiplication_x: [SSAO](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/ssao.md)
- :heavy_multiplication_x: [Motion Blur](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/motion-blur.md)
- :heavy_multiplication_x: [Chromatic Aberration](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/chromatic-aberration.md)
- :heavy_multiplication_x: [Screen Space Reflection](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/screen-space-reflection.md)
- :heavy_multiplication_x: [Screen Space Refraction](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/screen-space-refraction.md)
- :heavy_multiplication_x: [Foam](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/foam.md)
- :heavy_multiplication_x: [Flow Mapping](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/flow-mapping.md)
- :heavy_multiplication_x: [Outlining](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/outlining.md)
- :heavy_multiplication_x: [Depth Of Field](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/depth-of-field.md)
- :heavy_multiplication_x: [Posterization](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/posterization.md)
- :heavy_multiplication_x: [Pixelization](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/pixelization.md)
- :heavy_multiplication_x: [Sharpen](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/sharpen.md)
- :heavy_multiplication_x: [Dilation](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/dilation.md)
- :heavy_multiplication_x: [Film Grain](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/film-grain.md)
- :heavy_multiplication_x: [Lookup Table (LUT)](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/lookup-table.md)
- :heavy_multiplication_x: [Gamma Correction](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/gamma-correction.md)

## License

The included license applies only to the software portion of 3D Game Shaders For Beginners —
specifically the `.lisp`, `.vert`, and `.frag` source code files.
No other portion of 3D Game Shaders For Beginners has been licensed for use.

[The included media](https://opengameart.org/content/lowpoly-medieval-village-pack) is licensed under Public Domain (CC0).

## Copyright

3D Game Shaders For Beginners (c) 2019 David Lettier ([lettier.com](https://www.lettier.com))
<br>
Otus Lisp examples source code (c) 2021 Yuriy Chumak
