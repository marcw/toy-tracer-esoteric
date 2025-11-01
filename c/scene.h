#ifndef SCENE_H
#define SCENE_H

#include "tracer.h"

/* ========================================================================
   Scene Configuration Constants
   ======================================================================== */

#define MAX_OBJECTS 32      // Maximum number of primitives in the scene
#define IMAGE_WIDTH 800     // Output image width in pixels
#define IMAGE_HEIGHT 600    // Output image height in pixels

/* ========================================================================
   Scene: Contains all objects, lights, and rendering settings
   ======================================================================== */

typedef struct {
    Primitive objects[MAX_OBJECTS];  // Array of primitives in the scene
    int object_count;                // Number of primitives currently in scene
    Light light;                     // Single point light
    Vec3 background;                 // Background color (sky)
} Scene;

/* Set up the default scene with all primitive types */
void setup_scene(Scene *scene);

#endif /* SCENE_H */
