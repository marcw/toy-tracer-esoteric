#ifndef TRACER_H
#define TRACER_H

#include <math.h>
#include <stdbool.h>

/* ========================================================================
   Vec3: 3D Vector / Point / Color
   ======================================================================== */

typedef struct {
    float x, y, z;
} Vec3;

/* Construct a Vec3 from three floats */
static inline Vec3 vec3(float x, float y, float z) {
    Vec3 v = {x, y, z};
    return v;
}

/* Vector addition */
static inline Vec3 vec3_add(Vec3 a, Vec3 b) {
    return vec3(a.x + b.x, a.y + b.y, a.z + b.z);
}

/* Vector subtraction */
static inline Vec3 vec3_sub(Vec3 a, Vec3 b) {
    return vec3(a.x - b.x, a.y - b.y, a.z - b.z);
}

/* Scalar multiplication */
static inline Vec3 vec3_mul(Vec3 v, float s) {
    return vec3(v.x * s, v.y * s, v.z * s);
}

/* Component-wise multiplication (for colors) */
static inline Vec3 vec3_mul_vec(Vec3 a, Vec3 b) {
    return vec3(a.x * b.x, a.y * b.y, a.z * b.z);
}

/* Dot product: measures alignment between vectors */
static inline float vec3_dot(Vec3 a, Vec3 b) {
    return a.x * b.x + a.y * b.y + a.z * b.z;
}

/* Cross product: computes perpendicular vector */
static inline Vec3 vec3_cross(Vec3 a, Vec3 b) {
    return vec3(a.y * b.z - a.z * b.y,
                a.z * b.x - a.x * b.z,
                a.x * b.y - a.y * b.x);
}

/* Vector length squared (avoids expensive sqrt) */
static inline float vec3_length_sq(Vec3 v) {
    return vec3_dot(v, v);
}

/* Vector length */
static inline float vec3_length(Vec3 v) {
    return sqrtf(vec3_length_sq(v));
}

/* Normalize vector to unit length */
static inline Vec3 vec3_normalize(Vec3 v) {
    float len = vec3_length(v);
    if (len > 0.0f) {
        return vec3_mul(v, 1.0f / len);
    }
    return v;
}

/* Linear interpolation between two vectors */
static inline Vec3 vec3_lerp(Vec3 a, Vec3 b, float t) {
    return vec3_add(vec3_mul(a, 1.0f - t), vec3_mul(b, t));
}

/* Clamp vector components to [0, 1] */
static inline Vec3 vec3_clamp(Vec3 v) {
    float x = v.x < 0.0f ? 0.0f : (v.x > 1.0f ? 1.0f : v.x);
    float y = v.y < 0.0f ? 0.0f : (v.y > 1.0f ? 1.0f : v.y);
    float z = v.z < 0.0f ? 0.0f : (v.z > 1.0f ? 1.0f : v.z);
    return vec3(x, y, z);
}

/* ========================================================================
   Mat3: 3x3 Matrix for Rotations
   ======================================================================== */

typedef struct {
    float m[3][3];  // Row-major: m[row][col]
} Mat3;

/* Identity matrix */
static inline Mat3 mat3_identity(void) {
    Mat3 mat = {{{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}};
    return mat;
}

/* Matrix-vector multiplication: transforms a vector */
static inline Vec3 mat3_mul_vec(Mat3 m, Vec3 v) {
    return vec3(
        m.m[0][0] * v.x + m.m[0][1] * v.y + m.m[0][2] * v.z,
        m.m[1][0] * v.x + m.m[1][1] * v.y + m.m[1][2] * v.z,
        m.m[2][0] * v.x + m.m[2][1] * v.y + m.m[2][2] * v.z
    );
}

/* Matrix transpose: swaps rows and columns */
static inline Mat3 mat3_transpose(Mat3 m) {
    Mat3 t;
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            t.m[i][j] = m.m[j][i];
        }
    }
    return t;
}

/* Create rotation matrix from Euler angles (degrees)
   Order: Y (yaw) -> X (pitch) -> Z (roll) */
Mat3 mat3_from_euler(float pitch, float yaw, float roll);

/* ========================================================================
   Ray: Origin + Direction
   ======================================================================== */

typedef struct {
    Vec3 origin;
    Vec3 direction;  // Should be normalized
} Ray;

/* Compute point along ray at distance t: origin + t * direction */
static inline Vec3 ray_at(Ray r, float t) {
    return vec3_add(r.origin, vec3_mul(r.direction, t));
}

/* ========================================================================
   Material: Surface Appearance
   ======================================================================== */

typedef struct {
    Vec3 albedo;      // Base color (0-1 range)
    float roughness;  // Surface roughness (0 = smooth, 1 = rough)
    float metalness;  // Metallic factor (0 = dielectric, 1 = metal)
} Material;

/* Create a diffuse (non-metallic) material */
static inline Material material_diffuse(Vec3 color) {
    Material mat = {color, 0.9f, 0.0f};
    return mat;
}

/* Create a metallic material */
static inline Material material_metal(Vec3 color, float roughness) {
    Material mat = {color, roughness, 1.0f};
    return mat;
}

/* ========================================================================
   Primitive Types
   ======================================================================== */

typedef enum {
    OBJ_PLANE,
    OBJ_CUBE,
    OBJ_SPHERE,
    OBJ_CYLINDER,
    OBJ_TORUS
} PrimitiveType;

/* Plane: defined by a point and a normal */
typedef struct {
    Vec3 point;
    Vec3 normal;
} PlaneData;

/* Cube: axis-aligned box centered at position, with rotation */
typedef struct {
    Vec3 position;
    Mat3 rotation;
    float size;  // Half-size of the cube
} CubeData;

/* Sphere: center and radius */
typedef struct {
    Vec3 center;
    float radius;
} SphereData;

/* Cylinder: capped cylinder with position, rotation, and dimensions */
typedef struct {
    Vec3 position;
    Mat3 rotation;
    float radius;
    float height;  // Total height (extends ±height/2 from center)
} CylinderData;

/* Torus: donut shape with major/minor radii */
typedef struct {
    Vec3 position;
    Mat3 rotation;
    float major_radius;  // Distance from center to tube center
    float minor_radius;  // Tube radius
} TorusData;

/* Tagged union for all primitive types */
typedef struct {
    PrimitiveType type;
    Material material;
    union {
        PlaneData plane;
        CubeData cube;
        SphereData sphere;
        CylinderData cylinder;
        TorusData torus;
    } data;
} Primitive;

/* Primitive constructors */
Primitive make_plane(Vec3 point, Vec3 normal, Material mat);
Primitive make_cube(Vec3 position, Mat3 rotation, float size, Material mat);
Primitive make_sphere(Vec3 center, float radius, Material mat);
Primitive make_cylinder(Vec3 position, Mat3 rotation, float radius, float height, Material mat);
Primitive make_torus(Vec3 position, Mat3 rotation, float major_radius, float minor_radius, Material mat);

/* ========================================================================
   Light
   ======================================================================== */

typedef struct {
    Vec3 position;
    float intensity;  // Brightness multiplier
} Light;

/* Create a point light */
static inline Light make_light(Vec3 position, float intensity) {
    Light light = {position, intensity};
    return light;
}

/* ========================================================================
   Hit Record: Stores intersection information
   ======================================================================== */

typedef struct {
    float t;         // Distance along ray
    Vec3 point;      // Hit position in world space
    Vec3 normal;     // Surface normal at hit point
    Material material;  // Material of the hit surface
} HitRecord;

/* ========================================================================
   Ray Tracing Functions
   ======================================================================== */

/* Test if ray intersects a primitive
   Returns true if hit, and fills hit_rec with intersection data */
bool intersect_primitive(Ray ray, Primitive *prim, float t_min, float t_max, HitRecord *hit_rec);

/* Compute surface normal for a primitive at a given point */
Vec3 compute_normal(Primitive *prim, Vec3 point);

/* Trace a ray through the scene and return the color */
Vec3 trace_ray(Ray ray, Primitive *objects, int object_count, Light light, Vec3 background);

#endif /* TRACER_H */
