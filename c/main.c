#include "scene.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

/* ========================================================================
   TGA Image Writer
   TGA (Targa) is a simple uncompressed image format, easy to write manually
   Format: 18-byte header + BGRA pixel data + 26-byte footer
   ======================================================================== */

/* Write a 32-bit TGA file (BGRA format, uncompressed)
   This function creates the entire TGA file from scratch
   Returns true on success, false on error */
static bool write_tga(const char *filename, uint8_t *pixels, int width, int height) {
    FILE *file = fopen(filename, "wb");
    if (!file) {
        fprintf(stderr, "Error: Could not open %s for writing\n", filename);
        return false;
    }

    /* TGA Header (18 bytes) */
    uint8_t header[18] = {0};
    header[2] = 2;                  // Image type: uncompressed true-color
    header[12] = width & 0xFF;       // Width low byte
    header[13] = (width >> 8) & 0xFF; // Width high byte
    header[14] = height & 0xFF;      // Height low byte
    header[15] = (height >> 8) & 0xFF; // Height high byte
    header[16] = 32;                 // Bits per pixel (BGRA = 32)
    header[17] = 0x28;               // Image descriptor: origin top-left, 8-bit alpha

    fwrite(header, 1, 18, file);

    /* Write pixel data (BGRA format) */
    fwrite(pixels, 1, width * height * 4, file);

    /* TGA Footer (optional but recommended for TGA 2.0 compliance) */
    const char footer[26] = "TRUEVISION-XFILE.\0\0\0\0\0\0\0\0";
    fwrite(footer, 1, 26, file);

    fclose(file);
    return true;
}

/* Convert linear RGB color to gamma-corrected sRGB (gamma = 2.2)
   This makes the image look correct on standard monitors */
static uint8_t linear_to_srgb(float linear) {
    if (linear <= 0.0f) return 0;
    if (linear >= 1.0f) return 255;

    /* Apply gamma correction: sRGB = linear^(1/2.2) ≈ linear^0.4545 */
    float srgb = powf(linear, 1.0f / 2.2f);
    return (uint8_t)(srgb * 255.0f + 0.5f);
}

/* ========================================================================
   Camera and Ray Generation
   ======================================================================== */

/* Generate a ray from the camera through pixel (x, y)
   Camera is positioned at origin, looking down +Z axis
   Field of view (FOV) controls perspective */
static Ray generate_ray(int x, int y, int width, int height, float fov) {
    /* Convert pixel coordinates to normalized device coordinates [-1, 1] */
    float aspect_ratio = (float)width / (float)height;
    float px = (2.0f * (x + 0.5f) / width - 1.0f) * aspect_ratio;
    float py = 1.0f - 2.0f * (y + 0.5f) / height;  // Flip Y (top-left origin)

    /* Apply field of view to compute direction */
    float fov_radians = fov * (float)M_PI / 180.0f;
    float scale = tanf(fov_radians * 0.5f);

    Ray ray;
    ray.origin = vec3(0, 0, 0);  // Camera at origin
    ray.direction = vec3_normalize(vec3(px * scale, py * scale, 1.0f));

    return ray;
}

/* ========================================================================
   Main Rendering Loop
   ======================================================================== */

int main(void) {
    /* Set up the scene */
    Scene scene;
    setup_scene(&scene);

    /* Camera parameters */
    const float fov = 50.0f;  // Field of view in degrees

    /* Allocate image buffer (BGRA format, 4 bytes per pixel) */
    int num_pixels = IMAGE_WIDTH * IMAGE_HEIGHT;
    uint8_t *pixels = (uint8_t *)malloc(num_pixels * 4);
    if (!pixels) {
        fprintf(stderr, "Error: Could not allocate image buffer\n");
        return 1;
    }

    printf("Rendering %dx%d image...\n", IMAGE_WIDTH, IMAGE_HEIGHT);

    /* Render each pixel */
    for (int y = 0; y < IMAGE_HEIGHT; y++) {
        /* Progress indicator (print every 10 rows) */
        if (y % 10 == 0) {
            printf("Progress: %d%%\r", (y * 100) / IMAGE_HEIGHT);
            fflush(stdout);
        }

        for (int x = 0; x < IMAGE_WIDTH; x++) {
            /* Generate ray for this pixel */
            Ray ray = generate_ray(x, y, IMAGE_WIDTH, IMAGE_HEIGHT, fov);

            /* Trace ray through scene to get color */
            Vec3 color = trace_ray(ray, scene.objects, scene.object_count, scene.light, scene.background);

            /* Clamp color to valid range [0, 1] */
            color = vec3_clamp(color);

            /* Convert to 8-bit BGRA and store in buffer */
            int pixel_index = (y * IMAGE_WIDTH + x) * 4;
            pixels[pixel_index + 0] = linear_to_srgb(color.z);  // Blue
            pixels[pixel_index + 1] = linear_to_srgb(color.y);  // Green
            pixels[pixel_index + 2] = linear_to_srgb(color.x);  // Red
            pixels[pixel_index + 3] = 255;                      // Alpha (fully opaque)
        }
    }

    printf("Progress: 100%%\n");

    /* Write image to disk */
    const char *output_file = "output.tga";
    if (write_tga(output_file, pixels, IMAGE_WIDTH, IMAGE_HEIGHT)) {
        printf("Successfully wrote %s\n", output_file);
    } else {
        fprintf(stderr, "Failed to write image\n");
        free(pixels);
        return 1;
    }

    /* Clean up */
    free(pixels);

    return 0;
}
