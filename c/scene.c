#include "scene.h"

#include <string.h>

void setup_scene(Scene *scene) {
    memset(scene, 0, sizeof(*scene));

    scene->background = vec3(0.08f, 0.10f, 0.16f);

    /* Plane anchors the composition so the other shapes have visible contact shadows. */
    scene->objects[scene->object_count++] =
        make_plane(vec3(0.0f, -1.0f, 0.0f), vec3(0.0f, 1.0f, 0.0f), material_diffuse(vec3(0.9f, 0.9f, 0.9f)));

    /* Slightly tilted cube demonstrates oriented box intersections. */
    scene->objects[scene->object_count++] =
        make_cube(vec3(1.4f, -0.3f, 5.5f), mat3_from_euler(0.0f, 30.0f, 10.0f), 1.8f,
                  material_metal(vec3(0.85f, 0.84f, 0.80f), 0.35f));

    /* Red sphere keeps the math approachable for students when debugging normals. */
    scene->objects[scene->object_count++] =
        make_sphere(vec3(-1.6f, -0.1f, 4.5f), 0.9f, material_diffuse(vec3(0.75f, 0.15f, 0.12f)));

    /* Upright cylinder gives an example of capped primitives. */
    scene->objects[scene->object_count++] =
        make_cylinder(vec3(-3.0f, 0.0f, 7.0f), mat3_identity(), 0.6f, 2.2f,
                      material_diffuse(vec3(0.15f, 0.35f, 0.70f)));

    /* Torus is rotated to highlight the quartic intersection solver. */
    scene->objects[scene->object_count++] =
        make_torus(vec3(0.0f, 0.5f, 8.0f), mat3_from_euler(20.0f, 35.0f, 0.0f), 1.6f, 0.4f,
                   material_metal(vec3(0.95f, 0.65f, 0.25f), 0.25f));

    scene->light = make_light(vec3(6.0f, 5.0f, -2.0f), 55.0f);
}
