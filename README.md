htracer
=======

htracer is a simple haskell ray tracer written for entertainment purposes. Currently this ray tracer supports multithreading, supersampling, refraction with Fresnel reflectance, simple relativistic ray tracing, and various customization options.

## Usage
Read htracer.hs. Modify as you please. Then

    make

## Sample
Generated with msaa16. Took about 7 seconds each on i7-3930k.

![sample image](https://github.com/shaunren/htracer/raw/master/render.png)
*Camera stationary. Note that there is a sphere on the back of the camera, as seen on the reflection on the red sphere.*

![image v=0.5c](https://github.com/shaunren/htracer/raw/master/render_0.5c.png)
*Camera moving with v = 0.5c (moving forward) relative to the object frame.*

![image v=0.75c](https://github.com/shaunren/htracer/raw/master/render_0.75c.png)
*Camera moving with v = 0.75c. The green sphere is visible now.*

![image v=-0.999c](https://github.com/shaunren/htracer/raw/master/render_-0.999c.png)
*Camera moving with v = -0.999c (moving backward).*
