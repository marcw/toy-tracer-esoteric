% raytrace.pl
% Main entry point: handles image buffer, camera setup, and TGA output

:- ['scene.pl'].
:- initialization(main, main).

% =============================================================================
% IMAGE CONFIGURATION
% =============================================================================

% Image dimensions (smaller for faster testing)
image_width(400).
image_height(300).

% Camera parameters
camera_fov(60.0).  % Field of view in degrees

% =============================================================================
% CAMERA RAY GENERATION
% =============================================================================

% Generate a ray for pixel (X, Y)
generate_ray(X, Y, Width, Height, FOV, Ray) :-
    % Convert pixel coordinates to normalized device coordinates [-1, 1]
    AspectRatio is Width / Height,
    HalfWidth is Width / 2.0,
    HalfHeight is Height / 2.0,
    NDC_X is (X - HalfWidth) / HalfWidth,
    NDC_Y is -(Y - HalfHeight) / HalfHeight,  % Flip Y (image origin is top-left)

    % Convert to camera space
    FOV_Rad is FOV * pi / 180.0,
    HalfFOV is FOV_Rad / 2.0,
    ViewportHeight is 2.0 * tan(HalfFOV),
    ViewportWidth is ViewportHeight * AspectRatio,

    CamX is NDC_X * ViewportWidth / 2.0,
    CamY is NDC_Y * ViewportHeight / 2.0,
    CamZ is 1.0,  % Look along +Z axis

    % Create ray
    Origin = vec3(0, 0, 0),
    Direction_unnorm = vec3(CamX, CamY, CamZ),
    vec3_normalize(Direction_unnorm, Direction),
    Ray = ray(Origin, Direction).

% =============================================================================
% COLOR CONVERSION
% =============================================================================

% Convert floating-point color [0,1] to byte [0,255]
color_to_byte(F, B) :-
    clamp(F, 0.0, 1.0, Clamped),
    % Apply simple gamma correction (gamma = 2.2)
    Gamma is Clamped ** (1.0 / 2.2),
    B is floor(Gamma * 255.0).

% =============================================================================
% RENDERING LOOP
% =============================================================================

% Render the entire image
render_image(Scene, Width, Height, FOV, Pixels) :-
    render_rows(Scene, Width, Height, FOV, 0, Height, [], Pixels).

% Render all rows
render_rows(_, _, Height, _, Height, Height, Acc, Pixels) :-
    reverse(Acc, Pixels).

render_rows(Scene, Width, Height, FOV, Y, MaxY, Acc, Pixels) :-
    Y < MaxY,
    render_row(Scene, Width, Height, FOV, 0, Width, Y, [], RowPixels),
    NextY is Y + 1,
    append(Acc, RowPixels, NewAcc),
    render_rows(Scene, Width, Height, FOV, NextY, MaxY, NewAcc, Pixels).

% Render a single row
render_row(_, Width, _, _, Width, Width, _, Acc, RowPixels) :-
    reverse(Acc, RowPixels).

render_row(Scene, Width, Height, FOV, X, MaxX, Y, Acc, RowPixels) :-
    X < MaxX,
    generate_ray(X, Y, Width, Height, FOV, Ray),
    trace_ray(Ray, Scene, Color),
    Color = vec3(R, G, B),
    color_to_byte(R, BR),
    color_to_byte(G, BG),
    color_to_byte(B, BB),
    Pixel = pixel(BR, BG, BB),
    NextX is X + 1,
    render_row(Scene, Width, Height, FOV, NextX, MaxX, Y, [Pixel|Acc], RowPixels).

% =============================================================================
% TGA FILE OUTPUT
% =============================================================================

% Write TGA header
write_tga_header(Stream, Width, Height) :-
    % TGA header (18 bytes)
    put_byte(Stream, 0),    % ID length
    put_byte(Stream, 0),    % Color map type
    put_byte(Stream, 2),    % Image type (2 = uncompressed true-color)

    % Color map specification (5 bytes, all zeros)
    put_byte(Stream, 0), put_byte(Stream, 0),
    put_byte(Stream, 0), put_byte(Stream, 0),
    put_byte(Stream, 0),

    % Image specification
    put_byte(Stream, 0), put_byte(Stream, 0),  % X origin
    put_byte(Stream, 0), put_byte(Stream, 0),  % Y origin

    % Width (little-endian)
    WidthLow is Width mod 256,
    WidthHigh is Width // 256,
    put_byte(Stream, WidthLow),
    put_byte(Stream, WidthHigh),

    % Height (little-endian)
    HeightLow is Height mod 256,
    HeightHigh is Height // 256,
    put_byte(Stream, HeightLow),
    put_byte(Stream, HeightHigh),

    put_byte(Stream, 24),   % Bits per pixel (24 = RGB)
    put_byte(Stream, 0).    % Image descriptor

% Write pixel data (BGR format for TGA)
write_pixels(_, []).
write_pixels(Stream, [pixel(R, G, B)|Rest]) :-
    put_byte(Stream, B),  % Blue
    put_byte(Stream, G),  % Green
    put_byte(Stream, R),  % Red
    write_pixels(Stream, Rest).

% Write complete TGA file
write_tga(Filename, Width, Height, Pixels) :-
    open(Filename, write, Stream, [type(binary)]),
    write_tga_header(Stream, Width, Height),
    write_pixels(Stream, Pixels),
    close(Stream).

% =============================================================================
% MAIN ENTRY POINT
% =============================================================================

main :-
    write('Starting raytracer...'), nl,

    % Get image dimensions
    image_width(Width),
    image_height(Height),
    camera_fov(FOV),

    write('Image size: '), write(Width), write('x'), write(Height), nl,
    write('Setting up scene...'), nl,

    % Setup scene
    setup_scene(Scene),

    write('Rendering...'), nl,

    % Render image
    statistics(walltime, [Start|_]),
    render_image(Scene, Width, Height, FOV, Pixels),
    statistics(walltime, [End|_]),
    RenderTime is End - Start,

    write('Render time: '), write(RenderTime), write(' ms'), nl,
    write('Writing output.tga...'), nl,

    % Write TGA file
    write_tga('output.tga', Width, Height, Pixels),

    write('Done!'), nl,
    halt.

main :-
    write('Error: Rendering failed'), nl,
    halt(1).
