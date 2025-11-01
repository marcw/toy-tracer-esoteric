% tracer.pl
% Core raytracing functionality: math utilities, intersections, shading

% =============================================================================
% VECTOR OPERATIONS
% =============================================================================

% Vector addition
vec3_add(vec3(X1,Y1,Z1), vec3(X2,Y2,Z2), vec3(X,Y,Z)) :-
    X is X1 + X2,
    Y is Y1 + Y2,
    Z is Z1 + Z2.

% Vector subtraction
vec3_sub(vec3(X1,Y1,Z1), vec3(X2,Y2,Z2), vec3(X,Y,Z)) :-
    X is X1 - X2,
    Y is Y1 - Y2,
    Z is Z1 - Z2.

% Scalar multiplication
vec3_scale(vec3(X1,Y1,Z1), S, vec3(X,Y,Z)) :-
    X is X1 * S,
    Y is Y1 * S,
    Z is Z1 * S.

% Dot product
vec3_dot(vec3(X1,Y1,Z1), vec3(X2,Y2,Z2), Result) :-
    Result is X1*X2 + Y1*Y2 + Z1*Z2.

% Cross product
vec3_cross(vec3(X1,Y1,Z1), vec3(X2,Y2,Z2), vec3(X,Y,Z)) :-
    X is Y1*Z2 - Z1*Y2,
    Y is Z1*X2 - X1*Z2,
    Z is X1*Y2 - Y1*X2.

% Vector length squared
vec3_length_sq(vec3(X,Y,Z), L) :-
    L is X*X + Y*Y + Z*Z.

% Vector length
vec3_length(V, L) :-
    vec3_length_sq(V, LSq),
    L is sqrt(LSq).

% Vector normalization
vec3_normalize(V, N) :-
    vec3_length(V, L),
    L > 0.0001,
    InvL is 1.0 / L,
    vec3_scale(V, InvL, N).

% Component-wise multiplication
vec3_mul(vec3(X1,Y1,Z1), vec3(X2,Y2,Z2), vec3(X,Y,Z)) :-
    X is X1 * X2,
    Y is Y1 * Y2,
    Z is Z1 * Z2.

% Reflect vector V around normal N
vec3_reflect(V, N, R) :-
    vec3_dot(V, N, D),
    Twice is 2.0 * D,
    vec3_scale(N, Twice, ScaledN),
    vec3_sub(V, ScaledN, R).

% =============================================================================
% MATRIX OPERATIONS (3x3 for rotations)
% =============================================================================

% Matrix represented as mat3(R0, R1, R2) where each Ri is a vec3 (row)

% Identity matrix
mat3_identity(mat3(vec3(1,0,0), vec3(0,1,0), vec3(0,0,1))).

% Matrix-vector multiplication
mat3_mul_vec3(mat3(R0,R1,R2), V, vec3(X,Y,Z)) :-
    vec3_dot(R0, V, X),
    vec3_dot(R1, V, Y),
    vec3_dot(R2, V, Z).

% Transpose matrix (needed for proper rotation application)
mat3_transpose(mat3(vec3(A,B,C), vec3(D,E,F), vec3(G,H,I)),
               mat3(vec3(A,D,G), vec3(B,E,H), vec3(C,F,I))).

% Create rotation matrix from Euler angles (degrees)
% Simplified: Y-axis rotation only for now
mat3_from_euler(Yaw, Pitch, Roll, M) :-
    YawRad is Yaw * pi / 180.0,
    PitchRad is Pitch * pi / 180.0,
    RollRad is Roll * pi / 180.0,

    % Rotation around Y axis (yaw)
    CY is cos(YawRad),
    SY is sin(YawRad),

    % Rotation around X axis (pitch)
    CP is cos(PitchRad),
    SP is sin(PitchRad),

    % Rotation around Z axis (roll)
    CR is cos(RollRad),
    SR is sin(RollRad),

    % Combined rotation matrix (ZYX order)
    M00 is CY * CR + SY * SP * SR,
    M01 is -CY * SR + SY * SP * CR,
    M02 is SY * CP,

    M10 is CP * SR,
    M11 is CP * CR,
    M12 is -SP,

    M20 is -SY * CR + CY * SP * SR,
    M21 is SY * SR + CY * SP * CR,
    M22 is CY * CP,

    M = mat3(vec3(M00,M01,M02), vec3(M10,M11,M12), vec3(M20,M21,M22)).

% =============================================================================
% UTILITY FUNCTIONS
% =============================================================================

% Clamp value between min and max
clamp(V, Min, Max, R) :-
    (V < Min -> R = Min ; (V > Max -> R = Max ; R = V)).

% Mix (linear interpolation)
mix(A, B, T, R) :-
    OneMinusT is 1.0 - T,
    R is A * OneMinusT + B * T.

% =============================================================================
% RAY-PRIMITIVE INTERSECTIONS
% =============================================================================

% Ray defined as ray(Origin, Direction)
% Hit record: hit(T, Point, Normal, Material)

% Ray-Plane intersection
% Plane defined as plane(Point, Normal, Material)
intersect_plane(ray(RayOrigin, RayDir), plane(PlanePoint, PlaneNormal, Material), hit(T, HitPoint, PlaneNormal, Material)) :-
    vec3_dot(RayDir, PlaneNormal, Denom),
    abs(Denom) > 0.0001,  % Ray not parallel to plane
    vec3_sub(PlanePoint, RayOrigin, Diff),
    vec3_dot(Diff, PlaneNormal, Num),
    T is Num / Denom,
    T > 0.0001,  % Ray hits plane in front of origin
    vec3_scale(RayDir, T, Offset),
    vec3_add(RayOrigin, Offset, HitPoint).

% Ray-Sphere intersection
% Sphere defined as sphere(Center, Radius, Material)
intersect_sphere(ray(RayOrigin, RayDir), sphere(Center, Radius, Material), hit(T, HitPoint, Normal, Material)) :-
    vec3_sub(RayOrigin, Center, OC),
    vec3_dot(RayDir, RayDir, A),
    vec3_dot(OC, RayDir, B),
    B2 is 2.0 * B,
    vec3_dot(OC, OC, C1),
    C is C1 - Radius * Radius,
    Discriminant is B2*B2 - 4.0*A*C,
    Discriminant >= 0.0,
    SqrtD is sqrt(Discriminant),
    T1 is (-B2 - SqrtD) / (2.0 * A),
    T2 is (-B2 + SqrtD) / (2.0 * A),
    % Choose nearest positive t
    (T1 > 0.0001 -> T = T1 ; (T2 > 0.0001 -> T = T2 ; fail)),
    vec3_scale(RayDir, T, Offset),
    vec3_add(RayOrigin, Offset, HitPoint),
    vec3_sub(HitPoint, Center, NormalUnnorm),
    vec3_normalize(NormalUnnorm, Normal).

% Ray-Cube intersection (axis-aligned box)
% Cube defined as cube(Center, Rotation, Size, Material)
intersect_cube(ray(RayOrigin, RayDir), cube(Center, Rotation, Size, Material), hit(T, HitPoint, Normal, Material)) :-
    % Transform ray to object space
    mat3_transpose(Rotation, RotInv),
    vec3_sub(RayOrigin, Center, OriginRel),
    mat3_mul_vec3(RotInv, OriginRel, LocalOrigin),
    mat3_mul_vec3(RotInv, RayDir, LocalDir),

    % Box bounds
    HalfSize is Size * 0.5,
    BoxMin = vec3(-HalfSize, -HalfSize, -HalfSize),
    BoxMax = vec3(HalfSize, HalfSize, HalfSize),

    % AABB intersection in local space
    intersect_aabb(ray(LocalOrigin, LocalDir), BoxMin, BoxMax, T, LocalNormal),
    T > 0.0001,

    % Transform back to world space
    vec3_scale(LocalDir, T, LocalOffset),
    vec3_add(LocalOrigin, LocalOffset, LocalHitPoint),
    mat3_mul_vec3(Rotation, LocalHitPoint, HitPointRel),
    vec3_add(HitPointRel, Center, HitPoint),
    mat3_mul_vec3(Rotation, LocalNormal, Normal).

% Helper: AABB (axis-aligned bounding box) intersection
intersect_aabb(ray(vec3(OX,OY,OZ), vec3(DX,DY,DZ)), vec3(MinX,MinY,MinZ), vec3(MaxX,MaxY,MaxZ), T, Normal) :-
    % Compute intersection distances for each axis
    (abs(DX) > 0.0001 -> (
        T1X is (MinX - OX) / DX,
        T2X is (MaxX - OX) / DX,
        (T1X < T2X -> (TMinX = T1X, TMaxX = T2X) ; (TMinX = T2X, TMaxX = T1X))
    ) ; (TMinX = -1e30, TMaxX = 1e30)),

    (abs(DY) > 0.0001 -> (
        T1Y is (MinY - OY) / DY,
        T2Y is (MaxY - OY) / DY,
        (T1Y < T2Y -> (TMinY = T1Y, TMaxY = T2Y) ; (TMinY = T2Y, TMaxY = T1Y))
    ) ; (TMinY = -1e30, TMaxY = 1e30)),

    (abs(DZ) > 0.0001 -> (
        T1Z is (MinZ - OZ) / DZ,
        T2Z is (MaxZ - OZ) / DZ,
        (T1Z < T2Z -> (TMinZ = T1Z, TMaxZ = T2Z) ; (TMinZ = T2Z, TMaxZ = T1Z))
    ) ; (TMinZ = -1e30, TMaxZ = 1e30)),

    % Find the largest tMin and smallest tMax
    max_of_three(TMinX, TMinY, TMinZ, TMin),
    min_of_three(TMaxX, TMaxY, TMaxZ, TMax),

    TMax >= TMin,
    TMin > 0.0001,
    T = TMin,

    % Determine which face was hit
    (abs(TMin - TMinX) < 0.0001 -> (
        DX > 0 -> Normal = vec3(-1,0,0) ; Normal = vec3(1,0,0)
    ) ; (abs(TMin - TMinY) < 0.0001 -> (
        DY > 0 -> Normal = vec3(0,-1,0) ; Normal = vec3(0,1,0)
    ) ; (
        DZ > 0 -> Normal = vec3(0,0,-1) ; Normal = vec3(0,0,1)
    ))).

max_of_three(A, B, C, Max) :- max(A, B, AB), max(AB, C, Max).
min_of_three(A, B, C, Min) :- min(A, B, AB), min(AB, C, Min).
max(A, B, M) :- (A > B -> M = A ; M = B).
min(A, B, M) :- (A < B -> M = A ; M = B).

% Ray-Cylinder intersection (capped, with rotation)
% Cylinder defined as cylinder(Center, Rotation, Radius, Height, Material)
intersect_cylinder(ray(RayOrigin, RayDir), cylinder(Center, Rotation, Radius, Height, Material), hit(T, HitPoint, Normal, Material)) :-
    % Transform ray to object space (cylinder along Y axis)
    mat3_transpose(Rotation, RotInv),
    vec3_sub(RayOrigin, Center, OriginRel),
    mat3_mul_vec3(RotInv, OriginRel, vec3(OX,OY,OZ)),
    mat3_mul_vec3(RotInv, RayDir, vec3(DX,DY,DZ)),

    % Intersect infinite cylinder (ignore Y component)
    A is DX*DX + DZ*DZ,
    B is 2.0 * (OX*DX + OZ*DZ),
    C is OX*OX + OZ*OZ - Radius*Radius,
    Discriminant is B*B - 4.0*A*C,
    Discriminant >= 0.0,
    SqrtD is sqrt(Discriminant),
    T1 is (-B - SqrtD) / (2.0 * A),
    T2 is (-B + SqrtD) / (2.0 * A),

    % Check which intersection is valid (within height bounds)
    HalfHeight is Height * 0.5,
    (
        (T1 > 0.0001, Y1 is OY + T1*DY, abs(Y1) =< HalfHeight) -> (
            T = T1,
            LocalHitPoint = vec3(OX + T1*DX, Y1, OZ + T1*DZ),
            vec3(_, Y1, _) = LocalHitPoint,
            LocalNormal = vec3(OX + T1*DX, 0, OZ + T1*DZ)
        ) ;
        (T2 > 0.0001, Y2 is OY + T2*DY, abs(Y2) =< HalfHeight) -> (
            T = T2,
            LocalHitPoint = vec3(OX + T2*DX, Y2, OZ + T2*DZ),
            vec3(_, Y2, _) = LocalHitPoint,
            LocalNormal = vec3(OX + T2*DX, 0, OZ + T2*DZ)
        ) ;
        fail
    ),

    % Transform back to world space
    vec3_normalize(LocalNormal, LocalNormalNorm),
    mat3_mul_vec3(Rotation, LocalHitPoint, HitPointRel),
    vec3_add(HitPointRel, Center, HitPoint),
    mat3_mul_vec3(Rotation, LocalNormalNorm, Normal).

% Ray-Torus intersection (simplified)
% Torus defined as torus(Center, Rotation, MajorRadius, MinorRadius, Material)
% This is a placeholder - full quartic solver is complex
intersect_torus(ray(RayOrigin, RayDir), torus(Center, Rotation, MajorR, MinorR, Material), hit(T, HitPoint, Normal, Material)) :-
    % Transform ray to object space
    mat3_transpose(Rotation, RotInv),
    vec3_sub(RayOrigin, Center, OriginRel),
    mat3_mul_vec3(RotInv, OriginRel, LocalOrigin),
    mat3_mul_vec3(RotInv, RayDir, LocalDir),

    % Torus intersection requires solving quartic equation
    % For now, use numerical approximation (ray marching)
    torus_raymarch(LocalOrigin, LocalDir, MajorR, MinorR, 0.01, 100.0, 100, T),
    T > 0.0001,

    % Compute hit point and normal
    vec3_scale(LocalDir, T, LocalOffset),
    vec3_add(LocalOrigin, LocalOffset, LocalHitPoint),
    torus_normal(LocalHitPoint, MajorR, MinorR, LocalNormal),

    % Transform back to world space
    mat3_mul_vec3(Rotation, LocalHitPoint, HitPointRel),
    vec3_add(HitPointRel, Center, HitPoint),
    mat3_mul_vec3(Rotation, LocalNormal, Normal).

% Torus SDF (signed distance function)
torus_sdf(vec3(X,Y,Z), MajorR, MinorR, Dist) :-
    Q is sqrt(X*X + Z*Z) - MajorR,
    Dist is sqrt(Q*Q + Y*Y) - MinorR.

% Torus normal computation
% MinorR not used in gradient calculation but kept for consistency with SDF
torus_normal(Pos, MajorR, _MinorR, Normal) :-
    vec3(X, Y, Z) = Pos,
    Q is sqrt(X*X + Z*Z),
    (Q > 0.0001 -> (
        Factor is (Q - MajorR) / Q,
        NX is X * Factor,
        NY is Y,
        NZ is Z * Factor,
        vec3_normalize(vec3(NX, NY, NZ), Normal)
    ) ; (
        Normal = vec3(0, 1, 0)
    )).

% Ray marching for torus
torus_raymarch(Origin, Dir, MajorR, MinorR, Epsilon, MaxDist, MaxSteps, T) :-
    torus_raymarch_step(Origin, Dir, MajorR, MinorR, Epsilon, MaxDist, MaxSteps, 0.0, T).

torus_raymarch_step(Origin, Dir, MajorR, MinorR, Epsilon, MaxDist, Steps, CurrentT, T) :-
    Steps > 0,
    CurrentT < MaxDist,
    vec3_scale(Dir, CurrentT, Offset),
    vec3_add(Origin, Offset, Point),
    torus_sdf(Point, MajorR, MinorR, Dist),
    (
        Dist < Epsilon -> T = CurrentT ;
        (
            NextT is CurrentT + Dist,
            NextSteps is Steps - 1,
            torus_raymarch_step(Origin, Dir, MajorR, MinorR, Epsilon, MaxDist, NextSteps, NextT, T)
        )
    ).

% =============================================================================
% SHADING AND LIGHTING
% =============================================================================

% Material defined as material(Albedo, Roughness, Metalness)
% Light defined as light(Position, Intensity)

% Compute shading for a hit point
shade(HitPoint, Normal, Material, LightPos, LightIntensity, CameraPos, Color) :-
    material(Albedo, Roughness, Metalness) = Material,

    % Light direction
    vec3_sub(LightPos, HitPoint, ToLight),
    vec3_length(ToLight, LightDist),
    vec3_normalize(ToLight, L),

    % Compute lighting terms
    vec3_dot(Normal, L, NdotL_raw),
    max(0.0, NdotL_raw, NdotL),  % Clamp to positive

    % Simple diffuse lighting (simplified for debugging)
    vec3_scale(Albedo, NdotL, DiffuseUnnorm),

    % Attenuation (inverse square law)
    LightDistSq is LightDist * LightDist,
    Attenuation is LightIntensity / max(LightDistSq, 1.0),  % Avoid division by zero
    vec3_scale(DiffuseUnnorm, Attenuation, Color).

% Trace a ray through the scene
trace_ray(Ray, Scene, Color) :-
    find_nearest_hit(Ray, Scene, Hit),
    !,  % Cut: we found a hit
    Hit = hit(_, HitPoint, Normal, Material),
    Scene = scene(_, Light, CameraPos, _),
    Light = light(LightPos, LightIntensity),

    % Check for shadows
    vec3_sub(LightPos, HitPoint, ToLight),
    vec3_normalize(ToLight, LightDir),
    vec3_scale(Normal, 0.001, Bias),  % Small bias to avoid self-intersection
    vec3_add(HitPoint, Bias, BiasedPoint),
    ShadowRay = ray(BiasedPoint, LightDir),

    (is_shadowed(ShadowRay, Scene) ->
        % In shadow: return very dark color
        material_albedo(Material, Albedo),
        vec3_scale(Albedo, 0.05, Color)
    ;
        % Compute full shading
        shade(HitPoint, Normal, Material, LightPos, LightIntensity, CameraPos, Color)
    ).

% No hit: return background color
trace_ray(_, scene(Background, _, _, _), Background).

% Find nearest intersection in scene
find_nearest_hit(Ray, Scene, NearestHit) :-
    Scene = scene(_, _, _, Objects),
    find_nearest_in_list(Ray, Objects, none, NearestHit),
    NearestHit \= none.

% Base case: empty list returns current best
find_nearest_in_list(_, [], Best, Best).

% Recursive case: check current object and continue
find_nearest_in_list(Ray, [Obj|Rest], CurrentBest, FinalBest) :-
    (intersect_object(Ray, Obj, Hit) ->
        % Got a hit, check if it's better than current best
        Hit = hit(T, _, _, _),
        (CurrentBest = none ->
            % First hit found
            find_nearest_in_list(Ray, Rest, Hit, FinalBest)
        ;
            % Compare with existing best
            CurrentBest = hit(BestT, _, _, _),
            (T < BestT ->
                % New hit is closer
                find_nearest_in_list(Ray, Rest, Hit, FinalBest)
            ;
                % Keep current best
                find_nearest_in_list(Ray, Rest, CurrentBest, FinalBest)
            )
        )
    ;
        % No hit, continue with current best
        find_nearest_in_list(Ray, Rest, CurrentBest, FinalBest)
    ).

% Dispatch intersection based on object type
intersect_object(Ray, plane(P, N, M), Hit) :- intersect_plane(Ray, plane(P, N, M), Hit).
intersect_object(Ray, sphere(C, R, M), Hit) :- intersect_sphere(Ray, sphere(C, R, M), Hit).
intersect_object(Ray, cube(C, Rot, S, M), Hit) :- intersect_cube(Ray, cube(C, Rot, S, M), Hit).
intersect_object(Ray, cylinder(C, Rot, R, H, M), Hit) :- intersect_cylinder(Ray, cylinder(C, Rot, R, H, M), Hit).
intersect_object(Ray, torus(C, Rot, MajR, MinR, M), Hit) :- intersect_torus(Ray, torus(C, Rot, MajR, MinR, M), Hit).

% Check if a point is in shadow
is_shadowed(ShadowRay, Scene) :-
    Scene = scene(_, _, _, Objects),
    member(Obj, Objects),
    intersect_object(ShadowRay, Obj, _),
    !.  % Cut: we found an occluder

% Helper to extract albedo from material
material_albedo(material(Albedo, _, _), Albedo).
