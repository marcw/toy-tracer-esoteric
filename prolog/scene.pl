% scene.pl
% Scene definition: objects, lights, camera, and materials

:- ['tracer.pl'].

% =============================================================================
% MATERIALS
% =============================================================================

% Create a diffuse (non-metallic) material
material_diffuse(Color, material(Color, 0.8, 0.0)).

% Create a metallic material
material_metal(Color, Roughness, material(Color, Roughness, 1.0)).

% =============================================================================
% SCENE SETUP
% =============================================================================

% Setup the default scene matching scene.c
setup_scene(Scene) :-
    % Background color (dark blue-gray)
    Background = vec3(0.08, 0.10, 0.16),

    % Camera position
    CameraPos = vec3(0, 0, 0),

    % Light (position and intensity)
    % Moved to Z=3 (in front of objects) for proper lighting
    Light = light(vec3(6.0, 5.0, 3.0), 55.0),

    % Objects list
    setup_objects(Objects),

    % Complete scene
    Scene = scene(Background, Light, CameraPos, Objects).

% Define all objects in the scene
setup_objects(Objects) :-
    % Plane - provides ground and contact shadows
    material_diffuse(vec3(0.9, 0.9, 0.9), PlaneMat),
    Plane = plane(vec3(0.0, -1.0, 0.0), vec3(0.0, 1.0, 0.0), PlaneMat),

    % Cube - slightly tilted to demonstrate oriented box intersections
    mat3_from_euler(30.0, 10.0, 0.0, CubeRot),
    material_metal(vec3(0.85, 0.84, 0.80), 0.35, CubeMat),
    Cube = cube(vec3(1.4, -0.3, 5.5), CubeRot, 1.8, CubeMat),

    % Sphere - red diffuse, helps with debugging normals
    material_diffuse(vec3(0.75, 0.15, 0.12), SphereMat),
    Sphere = sphere(vec3(-1.6, -0.1, 4.5), 0.9, SphereMat),

    % Cylinder - upright, demonstrates capped primitives
    mat3_identity(CylRot),
    material_diffuse(vec3(0.15, 0.35, 0.70), CylMat),
    Cylinder = cylinder(vec3(-3.0, 0.0, 7.0), CylRot, 0.6, 2.2, CylMat),

    % Torus - rotated to highlight quartic intersection
    mat3_from_euler(35.0, 20.0, 0.0, TorusRot),
    material_metal(vec3(0.95, 0.65, 0.25), 0.25, TorusMat),
    Torus = torus(vec3(0.0, 0.5, 8.0), TorusRot, 1.6, 0.4, TorusMat),

    Objects = [Plane, Cube, Sphere, Cylinder, Torus].
