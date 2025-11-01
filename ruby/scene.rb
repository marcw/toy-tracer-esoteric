# frozen_string_literal: true

require_relative 'tracer'

# Setup the reference scene matching scene.c specification
# This scene demonstrates all five primitive types with various materials
def setup_scene
  scene = Scene.new

  # Background color - dark blue-grey atmosphere
  scene.background = vec3(0.08, 0.10, 0.16)

  # Plane anchors the composition so the other shapes have visible contact shadows
  scene.objects << make_plane(
    vec3(0.0, -1.0, 0.0),
    vec3(0.0, 1.0, 0.0),
    material_diffuse(vec3(0.9, 0.9, 0.9))
  )

  # Slightly tilted cube demonstrates oriented box intersections
  scene.objects << make_cube(
    vec3(1.4, -0.3, 5.5),
    mat3_from_euler(0.0, 30.0, 10.0),
    1.8,
    material_metal(vec3(0.85, 0.84, 0.80), 0.35)
  )

  # Red sphere keeps the math approachable for students when debugging normals
  scene.objects << make_sphere(
    vec3(-1.6, -0.1, 4.5),
    0.9,
    material_diffuse(vec3(0.75, 0.15, 0.12))
  )

  # Upright cylinder gives an example of capped primitives
  scene.objects << make_cylinder(
    vec3(-3.0, 0.0, 7.0),
    mat3_identity,
    0.6,
    2.2,
    material_diffuse(vec3(0.15, 0.35, 0.70))
  )

  # Torus is rotated to highlight the quartic intersection solver
  scene.objects << make_torus(
    vec3(0.0, 0.5, 8.0),
    mat3_from_euler(20.0, 35.0, 0.0),
    1.6,
    0.4,
    material_metal(vec3(0.95, 0.65, 0.25), 0.25)
  )

  # Point light positioned to create visible shadows and highlights
  scene.light = make_light(vec3(6.0, 5.0, -2.0), 55.0)

  scene.object_count = scene.objects.length
  scene
end
