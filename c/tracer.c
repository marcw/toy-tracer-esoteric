#include "tracer.h"
#include <float.h>
#include <math.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

/* ========================================================================
   Matrix Operations
   ======================================================================== */

/* Create a 3x3 rotation matrix from Euler angles in degrees
   Rotation order: Y (yaw) -> X (pitch) -> Z (roll)
   This is commonly used in 3D graphics applications */
Mat3 mat3_from_euler(float pitch, float yaw, float roll) {
    /* Convert degrees to radians */
    float p = pitch * (float)M_PI / 180.0f;
    float y = yaw * (float)M_PI / 180.0f;
    float r = roll * (float)M_PI / 180.0f;

    float cp = cosf(p), sp = sinf(p);
    float cy = cosf(y), sy = sinf(y);
    float cr = cosf(r), sr = sinf(r);

    /* Combine rotations: Rz * Rx * Ry */
    Mat3 mat;
    mat.m[0][0] = cy * cr + sy * sp * sr;
    mat.m[0][1] = -cy * sr + sy * sp * cr;
    mat.m[0][2] = sy * cp;

    mat.m[1][0] = sr * cp;
    mat.m[1][1] = cr * cp;
    mat.m[1][2] = -sp;

    mat.m[2][0] = -sy * cr + cy * sp * sr;
    mat.m[2][1] = sy * sr + cy * sp * cr;
    mat.m[2][2] = cy * cp;

    return mat;
}

/* ========================================================================
   Primitive Constructors
   ======================================================================== */

Primitive make_plane(Vec3 point, Vec3 normal, Material mat) {
    Primitive prim;
    prim.type = OBJ_PLANE;
    prim.material = mat;
    prim.data.plane.point = point;
    prim.data.plane.normal = vec3_normalize(normal);
    return prim;
}

Primitive make_cube(Vec3 position, Mat3 rotation, float size, Material mat) {
    Primitive prim;
    prim.type = OBJ_CUBE;
    prim.material = mat;
    prim.data.cube.position = position;
    prim.data.cube.rotation = rotation;
    prim.data.cube.size = size;
    return prim;
}

Primitive make_sphere(Vec3 center, float radius, Material mat) {
    Primitive prim;
    prim.type = OBJ_SPHERE;
    prim.material = mat;
    prim.data.sphere.center = center;
    prim.data.sphere.radius = radius;
    return prim;
}

Primitive make_cylinder(Vec3 position, Mat3 rotation, float radius, float height, Material mat) {
    Primitive prim;
    prim.type = OBJ_CYLINDER;
    prim.material = mat;
    prim.data.cylinder.position = position;
    prim.data.cylinder.rotation = rotation;
    prim.data.cylinder.radius = radius;
    prim.data.cylinder.height = height;
    return prim;
}

Primitive make_torus(Vec3 position, Mat3 rotation, float major_radius, float minor_radius, Material mat) {
    Primitive prim;
    prim.type = OBJ_TORUS;
    prim.material = mat;
    prim.data.torus.position = position;
    prim.data.torus.rotation = rotation;
    prim.data.torus.major_radius = major_radius;
    prim.data.torus.minor_radius = minor_radius;
    return prim;
}

/* ========================================================================
   Ray-Primitive Intersection Functions
   ======================================================================== */

/* Ray-Plane intersection
   A plane is defined by a point P and normal N
   Ray equation: R(t) = origin + t * direction
   Plane equation: dot(point - P, N) = 0
   Solve for t: t = dot(P - origin, N) / dot(direction, N) */
static bool intersect_plane(Ray ray, PlaneData *plane, float t_min, float t_max, HitRecord *rec) {
    float denom = vec3_dot(ray.direction, plane->normal);

    /* Ray is parallel to plane if denominator is near zero */
    if (fabsf(denom) < 1e-6f) {
        return false;
    }

    Vec3 to_plane = vec3_sub(plane->point, ray.origin);
    float t = vec3_dot(to_plane, plane->normal) / denom;

    if (t < t_min || t > t_max) {
        return false;
    }

    rec->t = t;
    rec->point = ray_at(ray, t);
    rec->normal = plane->normal;

    /* Flip normal to face the ray */
    if (vec3_dot(ray.direction, rec->normal) > 0.0f) {
        rec->normal = vec3_mul(rec->normal, -1.0f);
    }

    return true;
}

/* Ray-Sphere intersection
   Sphere equation: |point - center|^2 = radius^2
   Substitute ray equation and solve quadratic for t
   Discriminant determines if ray hits sphere */
static bool intersect_sphere(Ray ray, SphereData *sphere, float t_min, float t_max, HitRecord *rec) {
    Vec3 oc = vec3_sub(ray.origin, sphere->center);

    float a = vec3_dot(ray.direction, ray.direction);
    float b = 2.0f * vec3_dot(oc, ray.direction);
    float c = vec3_dot(oc, oc) - sphere->radius * sphere->radius;

    float discriminant = b * b - 4.0f * a * c;

    if (discriminant < 0.0f) {
        return false;
    }

    /* Find nearest intersection in valid range */
    float sqrt_disc = sqrtf(discriminant);
    float t = (-b - sqrt_disc) / (2.0f * a);

    if (t < t_min || t > t_max) {
        t = (-b + sqrt_disc) / (2.0f * a);
        if (t < t_min || t > t_max) {
            return false;
        }
    }

    rec->t = t;
    rec->point = ray_at(ray, t);
    rec->normal = vec3_normalize(vec3_sub(rec->point, sphere->center));

    return true;
}

/* Ray-Cube (Oriented Box) intersection
   Uses slab method: intersect ray with 3 pairs of parallel planes
   Transform ray to box's local space, test against axis-aligned box */
static bool intersect_cube(Ray ray, CubeData *cube, float t_min, float t_max, HitRecord *rec) {
    /* Transform ray to cube's local space (inverse rotation) */
    Mat3 inv_rot = mat3_transpose(cube->rotation);
    Vec3 local_origin = mat3_mul_vec(inv_rot, vec3_sub(ray.origin, cube->position));
    Vec3 local_dir = mat3_mul_vec(inv_rot, ray.direction);

    /* AABB (Axis-Aligned Bounding Box) slab test */
    float tmin = t_min;
    float tmax = t_max;
    int hit_axis = -1;
    int hit_sign = 0;

    for (int i = 0; i < 3; i++) {
        float inv_dir = 1.0f / ((float*)&local_dir)[i];
        float t0 = (-cube->size - ((float*)&local_origin)[i]) * inv_dir;
        float t1 = (cube->size - ((float*)&local_origin)[i]) * inv_dir;

        int sign = 0;
        if (t0 > t1) {
            float tmp = t0; t0 = t1; t1 = tmp;
            sign = 1;
        } else {
            sign = -1;
        }

        if (t0 > tmin) {
            tmin = t0;
            hit_axis = i;
            hit_sign = sign;
        }
        tmax = (t1 < tmax) ? t1 : tmax;

        if (tmax < tmin) {
            return false;
        }
    }

    if (tmin < t_min || tmin > t_max) {
        return false;
    }

    rec->t = tmin;
    rec->point = ray_at(ray, tmin);

    /* Compute normal in local space, then transform to world space */
    Vec3 local_normal = vec3(0, 0, 0);
    ((float*)&local_normal)[hit_axis] = (float)hit_sign;
    rec->normal = vec3_normalize(mat3_mul_vec(cube->rotation, local_normal));

    return true;
}

/* Ray-Cylinder (capped) intersection
   Cylinder is aligned along Y-axis in local space
   Test ray against infinite cylinder, then check caps */
static bool intersect_cylinder(Ray ray, CylinderData *cyl, float t_min, float t_max, HitRecord *rec) {
    /* Transform ray to cylinder's local space */
    Mat3 inv_rot = mat3_transpose(cyl->rotation);
    Vec3 local_origin = mat3_mul_vec(inv_rot, vec3_sub(ray.origin, cyl->position));
    Vec3 local_dir = mat3_mul_vec(inv_rot, ray.direction);

    float half_height = cyl->height * 0.5f;
    float best_t = FLT_MAX;
    Vec3 best_normal = vec3(0, 1, 0);
    bool hit = false;

    /* Test infinite cylinder (ignore Y component) */
    float a = local_dir.x * local_dir.x + local_dir.z * local_dir.z;
    float b = 2.0f * (local_origin.x * local_dir.x + local_origin.z * local_dir.z);
    float c = local_origin.x * local_origin.x + local_origin.z * local_origin.z - cyl->radius * cyl->radius;

    float disc = b * b - 4.0f * a * c;

    if (disc >= 0.0f && fabsf(a) > 1e-6f) {
        float sqrt_disc = sqrtf(disc);
        float t1 = (-b - sqrt_disc) / (2.0f * a);
        float t2 = (-b + sqrt_disc) / (2.0f * a);

        /* Check both intersections */
        for (int i = 0; i < 2; i++) {
            float t = (i == 0) ? t1 : t2;
            if (t >= t_min && t < best_t) {
                Vec3 p = vec3_add(local_origin, vec3_mul(local_dir, t));
                if (fabsf(p.y) <= half_height) {
                    best_t = t;
                    best_normal = vec3_normalize(vec3(p.x, 0, p.z));
                    hit = true;
                }
            }
        }
    }

    /* Test top and bottom caps */
    for (int cap = 0; cap < 2; cap++) {
        float y_plane = (cap == 0) ? -half_height : half_height;

        if (fabsf(local_dir.y) > 1e-6f) {
            float t = (y_plane - local_origin.y) / local_dir.y;
            if (t >= t_min && t < best_t) {
                Vec3 p = vec3_add(local_origin, vec3_mul(local_dir, t));
                float dist_sq = p.x * p.x + p.z * p.z;
                if (dist_sq <= cyl->radius * cyl->radius) {
                    best_t = t;
                    best_normal = vec3(0, (cap == 0) ? -1 : 1, 0);
                    hit = true;
                }
            }
        }
    }

    if (!hit || best_t > t_max) {
        return false;
    }

    rec->t = best_t;
    rec->point = ray_at(ray, best_t);
    rec->normal = vec3_normalize(mat3_mul_vec(cyl->rotation, best_normal));

    return true;
}

/* Solve quartic equation: a*x^4 + b*x^3 + c*x^2 + d*x + e = 0
   Returns number of real roots and fills roots array
   Uses Ferrari's method for quartic equations */
static int solve_quartic(float a, float b, float c, float d, float e, float roots[4]) {
    /* Normalize coefficients */
    if (fabsf(a) < 1e-10f) return 0;

    b /= a;
    c /= a;
    d /= a;
    e /= a;

    /* Substitute x = y - b/4 to eliminate cubic term */
    float b2 = b * b;
    float p = c - 3.0f * b2 / 8.0f;
    float q = b2 * b / 8.0f - b * c / 2.0f + d;
    float r = -3.0f * b2 * b2 / 256.0f + b2 * c / 16.0f - b * d / 4.0f + e;

    /* Special case: biquadratic equation */
    if (fabsf(q) < 1e-10f) {
        float disc = p * p - 4.0f * r;
        if (disc < 0.0f) return 0;

        float sqrt_disc = sqrtf(disc);
        float u1 = (-p + sqrt_disc) / 2.0f;
        float u2 = (-p - sqrt_disc) / 2.0f;

        int n = 0;
        if (u1 >= 0.0f) {
            float sqrt_u1 = sqrtf(u1);
            roots[n++] = sqrt_u1 - b / 4.0f;
            roots[n++] = -sqrt_u1 - b / 4.0f;
        }
        if (u2 >= 0.0f && fabsf(u1 - u2) > 1e-10f) {
            float sqrt_u2 = sqrtf(u2);
            roots[n++] = sqrt_u2 - b / 4.0f;
            roots[n++] = -sqrt_u2 - b / 4.0f;
        }
        return n;
    }

    /* Solve resolvent cubic to find y */
    float p2 = p * p;
    float A = -p;
    float B = -r;
    float C = p * r - q * q / 4.0f;

    /* Solve cubic: y^3 + A*y^2 + B*y + C = 0 using Cardano's formula */
    float A2 = A * A;
    float Q = (3.0f * B - A2) / 9.0f;
    float R = (9.0f * A * B - 27.0f * C - 2.0f * A * A2) / 54.0f;
    float D = Q * Q * Q + R * R;

    float y;
    if (D >= 0.0f) {
        float sqrt_D = sqrtf(D);
        float S = cbrtf(R + sqrt_D);
        float T = cbrtf(R - sqrt_D);
        y = S + T - A / 3.0f;
    } else {
        float theta = acosf(R / sqrtf(-Q * Q * Q));
        y = 2.0f * sqrtf(-Q) * cosf(theta / 3.0f) - A / 3.0f;
    }

    /* Compute quadratic factors */
    float sqrt_term = y * y - r;
    if (sqrt_term < 0.0f) return 0;

    float s = sqrtf(sqrt_term);
    float t = (fabsf(s) < 1e-10f) ? y : (p + y * y - q / s);

    /* Solve two quadratics */
    int n = 0;
    float disc1 = y * y - 4.0f * (t / 2.0f);
    if (disc1 >= 0.0f) {
        float sqrt_disc1 = sqrtf(disc1);
        roots[n++] = (-y + sqrt_disc1) / 2.0f - b / 4.0f;
        roots[n++] = (-y - sqrt_disc1) / 2.0f - b / 4.0f;
    }

    float disc2 = y * y - 4.0f * (-t / 2.0f);
    if (disc2 >= 0.0f) {
        float sqrt_disc2 = sqrtf(disc2);
        roots[n++] = (y + sqrt_disc2) / 2.0f - b / 4.0f;
        roots[n++] = (y - sqrt_disc2) / 2.0f - b / 4.0f;
    }

    return n;
}

/* Ray-Torus intersection
   Torus equation in local space: (R - sqrt(x^2 + z^2))^2 + y^2 = r^2
   where R = major_radius, r = minor_radius
   Substituting ray equation yields a quartic equation */
static bool intersect_torus(Ray ray, TorusData *torus, float t_min, float t_max, HitRecord *rec) {
    /* Transform ray to torus's local space */
    Mat3 inv_rot = mat3_transpose(torus->rotation);
    Vec3 local_origin = mat3_mul_vec(inv_rot, vec3_sub(ray.origin, torus->position));
    Vec3 local_dir = mat3_mul_vec(inv_rot, ray.direction);

    float R = torus->major_radius;
    float r = torus->minor_radius;
    float R2 = R * R;
    float r2 = r * r;

    /* Build quartic coefficients */
    float ox = local_origin.x, oy = local_origin.y, oz = local_origin.z;
    float dx = local_dir.x, dy = local_dir.y, dz = local_dir.z;

    float sum_d_sq = dx * dx + dy * dy + dz * dz;
    float e = ox * ox + oy * oy + oz * oz - R2 - r2;
    float f = ox * dx + oy * dy + oz * dz;
    float four_R2 = 4.0f * R2;

    float a = sum_d_sq * sum_d_sq;
    float b = 4.0f * sum_d_sq * f;
    float c = 2.0f * sum_d_sq * e + 4.0f * f * f + four_R2 * dy * dy;
    float d = 4.0f * f * e + 2.0f * four_R2 * oy * dy;
    float e_coeff = e * e - four_R2 * (r2 - oy * oy);

    float roots[4];
    int num_roots = solve_quartic(a, b, c, d, e_coeff, roots);

    /* Find closest valid intersection */
    float best_t = FLT_MAX;
    for (int i = 0; i < num_roots; i++) {
        if (roots[i] >= t_min && roots[i] < best_t && roots[i] <= t_max) {
            best_t = roots[i];
        }
    }

    if (best_t >= FLT_MAX) {
        return false;
    }

    rec->t = best_t;
    rec->point = ray_at(ray, best_t);

    /* Compute normal in local space */
    Vec3 local_point = vec3_add(local_origin, vec3_mul(local_dir, best_t));
    float param = sqrtf(local_point.x * local_point.x + local_point.z * local_point.z);
    Vec3 local_normal;

    if (param > 1e-6f) {
        local_normal = vec3(
            local_point.x * (1.0f - R / param),
            local_point.y,
            local_point.z * (1.0f - R / param)
        );
    } else {
        local_normal = vec3(local_point.x, local_point.y, local_point.z);
    }

    rec->normal = vec3_normalize(mat3_mul_vec(torus->rotation, local_normal));

    return true;
}

/* ========================================================================
   Main Intersection Dispatcher
   ======================================================================== */

bool intersect_primitive(Ray ray, Primitive *prim, float t_min, float t_max, HitRecord *rec) {
    bool hit = false;

    switch (prim->type) {
        case OBJ_PLANE:
            hit = intersect_plane(ray, &prim->data.plane, t_min, t_max, rec);
            break;
        case OBJ_CUBE:
            hit = intersect_cube(ray, &prim->data.cube, t_min, t_max, rec);
            break;
        case OBJ_SPHERE:
            hit = intersect_sphere(ray, &prim->data.sphere, t_min, t_max, rec);
            break;
        case OBJ_CYLINDER:
            hit = intersect_cylinder(ray, &prim->data.cylinder, t_min, t_max, rec);
            break;
        case OBJ_TORUS:
            hit = intersect_torus(ray, &prim->data.torus, t_min, t_max, rec);
            break;
    }

    if (hit) {
        rec->material = prim->material;
    }

    return hit;
}

/* ========================================================================
   Lighting and Shading
   ======================================================================== */

/* Simplified Blinn-Phong shading model
   Combines diffuse (Lambertian) and specular components
   Easier for students than full PBR but gives good results */
static Vec3 compute_lighting(HitRecord *rec, Light light, Vec3 view_dir, bool in_shadow) {
    Vec3 to_light = vec3_sub(light.position, rec->point);
    float distance_sq = vec3_length_sq(to_light);
    to_light = vec3_normalize(to_light);

    /* Diffuse component: N dot L (Lambert's cosine law) */
    float n_dot_l = fmaxf(0.0f, vec3_dot(rec->normal, to_light));

    /* Specular component: Blinn-Phong (halfway vector) */
    Vec3 halfway = vec3_normalize(vec3_add(to_light, view_dir));
    float n_dot_h = fmaxf(0.0f, vec3_dot(rec->normal, halfway));

    /* Roughness affects specular power (smoother = sharper highlights) */
    float shininess = (1.0f - rec->material.roughness) * 128.0f + 4.0f;
    float spec = powf(n_dot_h, shininess);

    /* Metalness affects how color works:
       - Metals have colored specular reflections
       - Dielectrics have white specular, colored diffuse */
    Vec3 diffuse_color = vec3_mul(rec->material.albedo, 1.0f - rec->material.metalness);
    Vec3 specular_color = vec3_lerp(vec3(1, 1, 1), rec->material.albedo, rec->material.metalness);

    /* Combine diffuse and specular with light attenuation */
    float attenuation = light.intensity / (distance_sq + 1.0f);

    if (in_shadow) {
        /* In shadow, only ambient term remains */
        return vec3_mul(rec->material.albedo, 0.05f);
    }

    Vec3 diffuse = vec3_mul(diffuse_color, n_dot_l);
    Vec3 specular = vec3_mul(specular_color, spec * (1.0f - rec->material.roughness * 0.8f));
    Vec3 color = vec3_add(diffuse, specular);
    color = vec3_mul(color, attenuation);

    /* Add small ambient term to avoid pure black shadows */
    color = vec3_add(color, vec3_mul(rec->material.albedo, 0.05f));

    return color;
}

/* ========================================================================
   Ray Tracing Entry Point
   ======================================================================== */

/* Trace a ray through the scene and compute pixel color
   This is the main rendering function called for each pixel */
Vec3 trace_ray(Ray ray, Primitive *objects, int object_count, Light light, Vec3 background) {
    HitRecord closest_hit;
    closest_hit.t = FLT_MAX;
    bool hit_anything = false;

    /* Find closest intersection */
    for (int i = 0; i < object_count; i++) {
        HitRecord temp_hit;
        if (intersect_primitive(ray, &objects[i], 0.001f, closest_hit.t, &temp_hit)) {
            closest_hit = temp_hit;
            hit_anything = true;
        }
    }

    if (!hit_anything) {
        return background;
    }

    /* Check if point is in shadow by casting ray toward light */
    Vec3 to_light = vec3_sub(light.position, closest_hit.point);
    float distance_to_light = vec3_length(to_light);
    to_light = vec3_normalize(to_light);

    Ray shadow_ray;
    shadow_ray.origin = vec3_add(closest_hit.point, vec3_mul(closest_hit.normal, 0.001f));
    shadow_ray.direction = to_light;

    bool in_shadow = false;
    for (int i = 0; i < object_count; i++) {
        HitRecord shadow_hit;
        if (intersect_primitive(shadow_ray, &objects[i], 0.001f, distance_to_light, &shadow_hit)) {
            in_shadow = true;
            break;
        }
    }

    /* Compute lighting at hit point */
    Vec3 view_dir = vec3_mul(ray.direction, -1.0f);
    Vec3 color = compute_lighting(&closest_hit, light, view_dir, in_shadow);

    return color;
}
