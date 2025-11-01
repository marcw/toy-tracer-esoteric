# AGENTS.md  
## Educational Raytracer Project Specification  

**Target audience:** Master 1 students discovering C programming  
**Project type:** Minimal CPU-based Raytracer in C  

---

## 1. Pedagogical Context

This project introduces students to:
- Core C programming (structs, functions, headers, modularity)
- Basic mathematical reasoning in 3D (vectors, rays, normals)
- Rendering logic (ray-primitive intersection, lighting)
- File output (writing an image to disk)

The final result is a **portable and minimal raytracer** that generates a simple `.tga` image on any computer (Windows, macOS, Linux) with **no dependencies** and **no external libraries**.  

---

## 2. Technical Goals

The raytracer must:
- Be written **entirely in standard C (C99 or later)**  
- Be compiled with a single command line such as:  
  ```bash
  gcc main.c tracer.c scene.c -o raytracer
  ```
- Produce a `.tga` image as output, containing the rendered scene  
- Avoid any dynamic dependency (no SDL, OpenGL, stb, etc.)  
- Require **no makefiles**, **no cmake**, **no external build system**

---

## 3. File Structure

```
/project-root
│
├── main.c        // Entry point; handles image buffer, invokes renderer
├── tracer.c      // Raytracing core: intersections, shading, lighting
├── tracer.h      // API definitions: structs, function prototypes
├── scene.c       // Scene definition: where the user places objects
├── scene.h       // Scene-level types and constants
└── output.tga    // Generated image (result)
```

---

## 4. Core Components

### 4.1. Main Renderer Loop
Responsible for:
- Setting up camera parameters (position, FOV, aspect ratio)
- Creating an image buffer (RGBA 32-bit)
- Looping over each pixel, generating a ray, and invoking `trace_ray()`
- Writing the final image as uncompressed TGA

### 4.2. Supported Primitives
The renderer must handle **only these 5 geometric primitives**:
- Plane  
- Cube  
- Sphere  
- Cylinder  
- Torus  

Each primitive has:
- A world-space position and orientation  
- A function returning ray intersection (boolean + hit distance)  
- A normal computation function  

Each primitive type is identified by an enum (e.g. `OBJ_PLANE`, `OBJ_SPHERE`, etc.)

### 4.3. Lighting
- Only **point lights** are supported.  
- A single `struct Light` defines position and intensity.  
- Shadows may be implemented by checking occlusion between hit point and light.  

### 4.4. Material System
Each primitive references a `Material` struct:
```c
typedef struct {

    Vec3 albedo;     // base color (0-1)
    float roughness; // surface roughness
    float metalness; // metal factor
} Material;
```

Shading uses a simplified **"Principled PBR"** model inspired by Blender, but implemented directly in C (no external math libs).  
Approximate Fresnel/roughness effects are acceptable.  

Textures are **not supported**.

---

## 5. Rendering Model

- Camera: simple pinhole camera at origin, looking along +Z  
- Rays: primary rays only (no recursion, no reflections, no refraction)  
- Shading: direct illumination from one point light  
- Shadows: optional (cast shadow rays)  
- Background: uniform color or gradient  

---

## 6. Image Output

Output must be a **TGA 32-bit (uncompressed)** file.  
Header and footer are written manually (no external library).  
Each pixel is written as BGRA (1 byte per channel).  
File name: `output.tga`.

---

## 7. Math Utilities

Students will need to implement:
- Vector operations: addition, subtraction, normalization, dot, cross
- Basic math helpers: clamp, mix, reflect
- Intersection math for all primitives listed above  

All math should be written in plain C — no `glm`, no external libs.

---

## 8. Scene Definition

All scene setup happens in `scene.c`:
- Create objects by filling an array of `Primitive` structs
- Each `Primitive` has a position, size, material, and type
- Example default scene: a **cube on a checkerboard plane**, one point light, camera at origin

Example:
```c
void setup_scene(Scene* scene) {
    // Ground
    scene->objects[0] = make_plane(vec3(0,-1,0), vec3(0,1,0), material_checkerboard());

    // Cube
    scene->objects[1] = make_cube(vec3(0,0,5), 1.0f, material_metal());

    // Light
    scene->light = make_light(vec3(5,5,0), 1.0f);
}
```

---

## 9. Expected Learning Outcomes

Students will:
- Understand compilation and linking in C  
- Practice modular design through `.c` / `.h` files  
- Grasp vector algebra in 3D  
- Implement geometric algorithms and lighting models  
- Learn how to debug numerically (without visual UI)
- Produce tangible visual results (TGA images)

---

## 10. Extensions (Optional)

For motivated students, optional improvements may include:
- Multiple lights  
- Ambient occlusion approximation  
- Reflection/refraction recursion  
- Multi-threading  
- Anti-aliasing by sampling multiple rays per pixel  
- Gamma correction

---

## 11. Platform Notes

| Platform | Compiler | Command Example |
|-----------|-----------|----------------|
| **Windows** | MinGW GCC | `gcc main.c tracer.c scene.c -o raytracer.exe` |
| **macOS** | Clang | `clang main.c tracer.c scene.c -o raytracer` |
| **Linux** | GCC | `gcc main.c tracer.c scene.c -o raytracer` |

No IDE required, but the code should compile and debug easily in **Visual Studio**, **VS Code**, or **Code::Blocks**.

---

## 12. Deliverables

Each student must:
1. Write their own `scene.c` (personalized scene)
2. Compile and run their code to produce `output.tga`
3. Comment their code clearly (in English)
4. Submit:
   - All `.c` and `.h` files
   - One `output.tga` file

---

## 13. Code Commenting Rules

- All comments **must be written in English**.  
- Comments should explain *why* the code exists, not only *what* it does.  
- Example:
  ```c
  // Compute the intersection between a ray and a sphere
  // Returns true if hit, and updates the hit record with distance and normal
  ```

---

## 14. Evaluation Criteria

| Criterion | Weight | Description |
|------------|---------|-------------|
| Code structure | 25% | Proper separation in `.c`/`.h` files |
| Readability | 20% | Clear naming and English comments |

| Correctness | 25% | Raytracer produces a valid image |
| Creativity | 15% | Interesting or aesthetic scene composition |
| Bonus | 15% | Optional features (shadows, reflections, etc.) |

---

## 15. References

- Shirley & Morley, *Fundamentals of Computer Graphics*  
- Smallpt path tracer (Kevin Beason, 2008) – reference for minimalism  
- Blender Principled BSDF documentation  
- Paul Bourke’s geometry intersection formulas


---

## 16. Constraints Summary

✅ **Allowed:**  
- Standard C (C99 or later)  
- Standard libraries (`math.h`, `stdio.h`, etc.)  
- Hardcoded data (no external assets)  

❌ **Not allowed:**  
- C++ or object-oriented constructs  
- External dependencies or libraries  
- Textures, GUI, or real-time display  
- Build systems (Make, CMake, etc.)

---

**End of AGENTS.md**

