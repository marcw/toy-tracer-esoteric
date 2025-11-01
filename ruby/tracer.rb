# frozen_string_literal: true

# Core raytracing module containing all mathematical operations,
# data structures, intersection algorithms, and shading logic

# ============================================================================
# VECTOR MATH
# ============================================================================

# 3D Vector class for all spatial calculations
class Vec3
  attr_accessor :x, :y, :z

  def initialize(x = 0.0, y = 0.0, z = 0.0)
    @x = x.to_f
    @y = y.to_f
    @z = z.to_f
  end

  # Vector addition
  def +(other)
    Vec3.new(@x + other.x, @y + other.y, @z + other.z)
  end

  # Vector subtraction
  def -(other)
    Vec3.new(@x - other.x, @y - other.y, @z - other.z)
  end

  # Scalar multiplication
  def *(scalar)
    Vec3.new(@x * scalar, @y * scalar, @z * scalar)
  end

  # Component-wise multiplication (for colors)
  def mul_vec(other)
    Vec3.new(@x * other.x, @y * other.y, @z * other.z)
  end

  # Scalar division
  def /(scalar)
    inv = 1.0 / scalar
    Vec3.new(@x * inv, @y * inv, @z * inv)
  end

  # Dot product
  def dot(other)
    @x * other.x + @y * other.y + @z * other.z
  end

  # Cross product
  def cross(other)
    Vec3.new(
      @y * other.z - @z * other.y,
      @z * other.x - @x * other.z,
      @x * other.y - @y * other.x
    )
  end

  # Vector length squared (avoids sqrt when possible)
  def length_squared
    @x * @x + @y * @y + @z * @z
  end

  # Vector length
  def length
    Math.sqrt(length_squared)
  end

  # Normalize to unit length
  def normalize
    len = length
    return Vec3.new(0, 0, 0) if len < 1e-8
    self / len
  end

  # Reflect vector across normal
  def reflect(normal)
    self - normal * (2.0 * dot(normal))
  end

  # Clamp each component to [0, 1]
  def clamp01
    Vec3.new(
      [[0.0, @x].max, 1.0].min,
      [[0.0, @y].max, 1.0].min,
      [[0.0, @z].max, 1.0].min
    )
  end

  def to_s
    "(#{@x}, #{@y}, #{@z})"
  end
end

# Helper function to create Vec3
def vec3(x, y, z)
  Vec3.new(x, y, z)
end

# ============================================================================
# MATRIX MATH (3x3 rotation matrices)
# ============================================================================

# 3x3 Matrix class for rotation transformations
class Mat3
  attr_accessor :m

  # Initialize with 3x3 array or flat 9-element array
  def initialize(values = nil)
    if values
      @m = values.is_a?(Array) && values.first.is_a?(Array) ? values : reshape(values)
    else
      @m = Array.new(3) { Array.new(3, 0.0) }
    end
  end

  # Identity matrix
  def self.identity
    Mat3.new([
      [1.0, 0.0, 0.0],
      [0.0, 1.0, 0.0],
      [0.0, 0.0, 1.0]
    ])
  end

  # Create rotation matrix from Euler angles (in degrees)
  # Order: X -> Y -> Z rotations
  def self.from_euler(pitch, yaw, roll)
    # Convert degrees to radians
    p = pitch * Math::PI / 180.0
    y = yaw * Math::PI / 180.0
    r = roll * Math::PI / 180.0

    # Precompute trig values
    cp = Math.cos(p)
    sp = Math.sin(p)
    cy = Math.cos(y)
    sy = Math.sin(y)
    cr = Math.cos(r)
    sr = Math.sin(r)

    # Combined rotation matrix (Rz * Ry * Rx)
    Mat3.new([
      [cy * cr, -cy * sr, sy],
      [cp * sr + sp * sy * cr, cp * cr - sp * sy * sr, -sp * cy],
      [sp * sr - cp * sy * cr, sp * cr + cp * sy * sr, cp * cy]
    ])
  end

  # Transform vector by this matrix
  def transform(v)
    Vec3.new(
      @m[0][0] * v.x + @m[0][1] * v.y + @m[0][2] * v.z,
      @m[1][0] * v.x + @m[1][1] * v.y + @m[1][2] * v.z,
      @m[2][0] * v.x + @m[2][1] * v.y + @m[2][2] * v.z
    )
  end

  # Transpose (also gives inverse for rotation matrices)
  def transpose
    Mat3.new([
      [@m[0][0], @m[1][0], @m[2][0]],
      [@m[0][1], @m[1][1], @m[2][1]],
      [@m[0][2], @m[1][2], @m[2][2]]
    ])
  end

  private

  def reshape(flat)
    [flat[0..2], flat[3..5], flat[6..8]]
  end
end

# Helper functions
def mat3_identity
  Mat3.identity
end

def mat3_from_euler(pitch, yaw, roll)
  Mat3.from_euler(pitch, yaw, roll)
end

# ============================================================================
# DATA STRUCTURES
# ============================================================================

# Material definition for PBR shading
class Material
  attr_accessor :albedo, :roughness, :metalness

  def initialize(albedo, roughness = 0.8, metalness = 0.0)
    @albedo = albedo      # Vec3 - base color
    @roughness = roughness # Float 0-1
    @metalness = metalness # Float 0-1
  end
end

# Helper constructors for common materials
def material_diffuse(color)
  Material.new(color, 1.0, 0.0)
end

def material_metal(color, roughness)
  Material.new(color, roughness, 1.0)
end

# Point light source
class Light
  attr_accessor :position, :intensity

  def initialize(position, intensity)
    @position = position   # Vec3
    @intensity = intensity # Float
  end
end

def make_light(position, intensity)
  Light.new(position, intensity)
end

# Ray for tracing
class Ray
  attr_accessor :origin, :direction

  def initialize(origin, direction)
    @origin = origin       # Vec3
    @direction = direction # Vec3 (should be normalized)
  end

  # Get point along ray at distance t
  def at(t)
    @origin + @direction * t
  end
end

# Primitive types
module PrimitiveType
  PLANE = 0
  SPHERE = 1
  CUBE = 2
  CYLINDER = 3
  TORUS = 4
end

# Geometric primitive in the scene
class Primitive
  attr_accessor :type, :position, :rotation, :size, :size2, :normal, :material

  def initialize(type, position, rotation, material)
    @type = type           # PrimitiveType enum
    @position = position   # Vec3
    @rotation = rotation   # Mat3
    @material = material   # Material
    @size = 0.0           # Radius, half-extent, etc.
    @size2 = 0.0          # Secondary size (height, minor radius, etc.)
    @normal = vec3(0, 1, 0) # For planes
  end
end

# Primitive constructors
def make_plane(position, normal, material)
  prim = Primitive.new(PrimitiveType::PLANE, position, mat3_identity, material)
  prim.normal = normal.normalize
  prim
end

def make_sphere(position, radius, material)
  prim = Primitive.new(PrimitiveType::SPHERE, position, mat3_identity, material)
  prim.size = radius
  prim
end

def make_cube(position, rotation, half_extent, material)
  prim = Primitive.new(PrimitiveType::CUBE, position, rotation, material)
  prim.size = half_extent
  prim
end

def make_cylinder(position, rotation, radius, height, material)
  prim = Primitive.new(PrimitiveType::CYLINDER, position, rotation, material)
  prim.size = radius
  prim.size2 = height
  prim
end

def make_torus(position, rotation, major_radius, minor_radius, material)
  prim = Primitive.new(PrimitiveType::TORUS, position, rotation, material)
  prim.size = major_radius
  prim.size2 = minor_radius
  prim
end

# Scene container
class Scene
  attr_accessor :objects, :object_count, :light, :background

  def initialize
    @objects = []
    @object_count = 0
    @light = nil
    @background = vec3(0, 0, 0)
  end
end

# Hit record for intersection results
class HitRecord
  attr_accessor :hit, :t, :point, :normal, :material

  def initialize
    @hit = false
    @t = Float::INFINITY
    @point = vec3(0, 0, 0)
    @normal = vec3(0, 1, 0)
    @material = nil
  end
end

# ============================================================================
# INTERSECTION ALGORITHMS
# ============================================================================

# Intersect ray with plane
# Returns HitRecord with hit information
def intersect_plane(ray, plane)
  rec = HitRecord.new

  denom = ray.direction.dot(plane.normal)
  return rec if denom.abs < 1e-6 # Ray parallel to plane

  t = (plane.position - ray.origin).dot(plane.normal) / denom
  return rec if t < 0.001 # Behind ray origin or too close

  rec.hit = true
  rec.t = t
  rec.point = ray.at(t)
  rec.normal = plane.normal
  rec.material = plane.material
  rec
end

# Intersect ray with sphere using quadratic formula
def intersect_sphere(ray, sphere)
  rec = HitRecord.new

  oc = ray.origin - sphere.position
  a = ray.direction.length_squared
  half_b = oc.dot(ray.direction)
  c = oc.length_squared - sphere.size * sphere.size

  discriminant = half_b * half_b - a * c
  return rec if discriminant < 0 # No intersection

  sqrt_d = Math.sqrt(discriminant)
  root = (-half_b - sqrt_d) / a

  # Check if root is in valid range
  if root < 0.001
    root = (-half_b + sqrt_d) / a
    return rec if root < 0.001
  end

  rec.hit = true
  rec.t = root
  rec.point = ray.at(root)
  rec.normal = (rec.point - sphere.position) / sphere.size
  rec.material = sphere.material
  rec
end

# Intersect ray with axis-aligned or oriented bounding box (cube)
def intersect_cube(ray, cube)
  rec = HitRecord.new

  # Transform ray into cube's local space
  inv_rotation = cube.rotation.transpose
  local_origin = inv_rotation.transform(ray.origin - cube.position)
  local_dir = inv_rotation.transform(ray.direction)

  # AABB intersection in local space
  half = cube.size
  inv_dir = vec3(
    local_dir.x.abs > 1e-8 ? 1.0 / local_dir.x : 1e30,
    local_dir.y.abs > 1e-8 ? 1.0 / local_dir.y : 1e30,
    local_dir.z.abs > 1e-8 ? 1.0 / local_dir.z : 1e30
  )

  t1 = (-half - local_origin.x) * inv_dir.x
  t2 = (half - local_origin.x) * inv_dir.x
  t3 = (-half - local_origin.y) * inv_dir.y
  t4 = (half - local_origin.y) * inv_dir.y
  t5 = (-half - local_origin.z) * inv_dir.z
  t6 = (half - local_origin.z) * inv_dir.z

  tmin = [t1, t2].min
  tmax = [t1, t2].max
  tmin = [[tmin, [t3, t4].min].max, [t5, t6].min].max
  tmax = [[tmax, [t3, t4].max].min, [t5, t6].max].min

  return rec if tmax < 0 || tmin > tmax

  t = tmin > 0.001 ? tmin : tmax
  return rec if t < 0.001

  # Compute local hit point and normal
  local_point = local_origin + local_dir * t
  local_normal = vec3(0, 0, 0)

  # Determine which face was hit
  eps = 0.0001
  if (local_point.x - half).abs < eps
    local_normal = vec3(1, 0, 0)
  elsif (local_point.x + half).abs < eps
    local_normal = vec3(-1, 0, 0)
  elsif (local_point.y - half).abs < eps
    local_normal = vec3(0, 1, 0)
  elsif (local_point.y + half).abs < eps
    local_normal = vec3(0, -1, 0)
  elsif (local_point.z - half).abs < eps
    local_normal = vec3(0, 0, 1)
  elsif (local_point.z + half).abs < eps
    local_normal = vec3(0, 0, -1)
  end

  # Transform normal back to world space
  rec.hit = true
  rec.t = t
  rec.point = ray.at(t)
  rec.normal = cube.rotation.transform(local_normal).normalize
  rec.material = cube.material
  rec
end

# Intersect ray with capped cylinder
def intersect_cylinder(ray, cyl)
  rec = HitRecord.new

  # Transform to local space
  inv_rotation = cyl.rotation.transpose
  local_origin = inv_rotation.transform(ray.origin - cyl.position)
  local_dir = inv_rotation.transform(ray.direction)

  # Cylinder aligned along Y axis, caps at ±size2/2
  radius = cyl.size
  half_height = cyl.size2 / 2.0

  # Solve quadratic for infinite cylinder (ignoring Y)
  a = local_dir.x * local_dir.x + local_dir.z * local_dir.z
  b = 2.0 * (local_origin.x * local_dir.x + local_origin.z * local_dir.z)
  c = local_origin.x * local_origin.x + local_origin.z * local_origin.z - radius * radius

  t_min = Float::INFINITY

  # Check cylinder body
  if a > 1e-8
    discriminant = b * b - 4.0 * a * c
    if discriminant >= 0
      sqrt_d = Math.sqrt(discriminant)
      t1 = (-b - sqrt_d) / (2.0 * a)
      t2 = (-b + sqrt_d) / (2.0 * a)

      [t1, t2].each do |t|
        next if t < 0.001
        y = local_origin.y + local_dir.y * t
        if y.abs <= half_height && t < t_min
          t_min = t
        end
      end
    end
  end

  # Check top cap (y = +half_height)
  if local_dir.y.abs > 1e-8
    t = (half_height - local_origin.y) / local_dir.y
    if t > 0.001
      x = local_origin.x + local_dir.x * t
      z = local_origin.z + local_dir.z * t
      if x * x + z * z <= radius * radius && t < t_min
        t_min = t
      end
    end
  end

  # Check bottom cap (y = -half_height)
  if local_dir.y.abs > 1e-8
    t = (-half_height - local_origin.y) / local_dir.y
    if t > 0.001
      x = local_origin.x + local_dir.x * t
      z = local_origin.z + local_dir.z * t
      if x * x + z * z <= radius * radius && t < t_min
        t_min = t
      end
    end
  end

  return rec if t_min == Float::INFINITY

  # Compute local hit point and normal
  local_point = local_origin + local_dir * t_min
  local_normal = vec3(0, 0, 0)

  # Determine if hit was on caps or body
  if (local_point.y - half_height).abs < 0.0001
    local_normal = vec3(0, 1, 0)
  elsif (local_point.y + half_height).abs < 0.0001
    local_normal = vec3(0, -1, 0)
  else
    local_normal = vec3(local_point.x, 0, local_point.z).normalize
  end

  rec.hit = true
  rec.t = t_min
  rec.point = ray.at(t_min)
  rec.normal = cyl.rotation.transform(local_normal).normalize
  rec.material = cyl.material
  rec
end

# Solve quartic equation for torus intersection
# Uses Ferrari's method for quartic solution
def solve_quartic(coeffs)
  # Normalize by leading coefficient
  a = coeffs[1] / coeffs[0]
  b = coeffs[2] / coeffs[0]
  c = coeffs[3] / coeffs[0]
  d = coeffs[4] / coeffs[0]

  # Depress the quartic
  p = b - (3.0 * a * a) / 8.0
  q = (a * a * a) / 8.0 - (a * b) / 2.0 + c
  r = (-3.0 * a * a * a * a) / 256.0 + (a * a * b) / 16.0 - (a * c) / 4.0 + d

  # Solve resolvent cubic (simplified)
  # For simplicity, use a numerical approach for the general case
  # This is a simplified solver - may not find all roots in edge cases

  roots = []

  # Ferrari's method requires solving a cubic
  # For educational purposes, we'll use a simplified Newton-Raphson approach
  # Testing multiple starting points

  (-3.0..3.0).step(0.5) do |x0|
    x = x0
    10.times do
      fx = x**4 + a * x**3 + b * x**2 + c * x + d
      fpx = 4.0 * x**3 + 3.0 * a * x**2 + 2.0 * b * x + c
      break if fpx.abs < 1e-10
      x_new = x - fx / fpx
      break if (x_new - x).abs < 1e-6
      x = x_new
    end

    # Check if this is actually a root
    fx = x**4 + a * x**3 + b * x**2 + c * x + d
    if fx.abs < 0.01 && !roots.any? { |r| (r - x).abs < 0.01 }
      roots << x
    end
  end

  roots.sort
end

# Intersect ray with torus (most complex primitive)
def intersect_torus(ray, torus)
  rec = HitRecord.new

  # Transform to local space
  inv_rotation = torus.rotation.transpose
  local_origin = inv_rotation.transform(ray.origin - torus.position)
  local_dir = inv_rotation.transform(ray.direction)

  r_major = torus.size  # Major radius (center to tube center)
  r_minor = torus.size2 # Minor radius (tube radius)

  # Build quartic coefficients
  # Torus equation: (x² + y² + z² + R² - r²)² = 4R²(x² + z²)
  # where R = major radius, r = minor radius
  # Torus aligned in XZ plane, centered at origin

  ox = local_origin.x
  oy = local_origin.y
  oz = local_origin.z
  dx = local_dir.x
  dy = local_dir.y
  dz = local_dir.z

  sum_d_sqr = dx * dx + dy * dy + dz * dz
  e = ox * ox + oy * oy + oz * oz - r_major * r_major - r_minor * r_minor
  f = ox * dx + oy * dy + oz * dz
  four_a_sqr = 4.0 * r_major * r_major

  # Quartic coefficients
  c4 = sum_d_sqr * sum_d_sqr
  c3 = 4.0 * sum_d_sqr * f
  c2 = 2.0 * sum_d_sqr * e + 4.0 * f * f + four_a_sqr * (dx * dx + dz * dz)
  c1 = 4.0 * f * e + 2.0 * four_a_sqr * (ox * dx + oz * dz)
  c0 = e * e - four_a_sqr * (r_minor * r_minor - ox * ox - oz * oz)

  # Solve quartic equation
  roots = solve_quartic([c4, c3, c2, c1, c0])

  # Find smallest positive root
  t_min = Float::INFINITY
  roots.each do |t|
    t_min = t if t > 0.001 && t < t_min
  end

  return rec if t_min == Float::INFINITY

  # Compute local hit point and normal
  local_point = local_origin + local_dir * t_min

  # Torus normal calculation
  # Project point onto XZ plane, find closest point on major circle
  xz_dist = Math.sqrt(local_point.x**2 + local_point.z**2)
  return rec if xz_dist < 1e-8

  # Point on major circle
  major_x = (r_major / xz_dist) * local_point.x
  major_z = (r_major / xz_dist) * local_point.z

  # Normal points from major circle point to hit point
  local_normal = vec3(
    local_point.x - major_x,
    local_point.y,
    local_point.z - major_z
  ).normalize

  rec.hit = true
  rec.t = t_min
  rec.point = ray.at(t_min)
  rec.normal = torus.rotation.transform(local_normal).normalize
  rec.material = torus.material
  rec
end

# Main intersection dispatch
def intersect(ray, primitive)
  case primitive.type
  when PrimitiveType::PLANE
    intersect_plane(ray, primitive)
  when PrimitiveType::SPHERE
    intersect_sphere(ray, primitive)
  when PrimitiveType::CUBE
    intersect_cube(ray, primitive)
  when PrimitiveType::CYLINDER
    intersect_cylinder(ray, primitive)
  when PrimitiveType::TORUS
    intersect_torus(ray, primitive)
  else
    HitRecord.new
  end
end

# ============================================================================
# SHADING AND LIGHTING
# ============================================================================

# Simplified Fresnel-Schlick approximation
def fresnel_schlick(cos_theta, f0)
  f0 + (vec3(1, 1, 1) - f0) * ((1.0 - cos_theta)**5)
end

# Simplified PBR shading inspired by Blender's Principled BSDF
def shade(hit_rec, light, view_dir, scene)
  mat = hit_rec.material
  n = hit_rec.normal
  v = view_dir * -1.0 # View direction toward camera

  # Light direction and distance
  light_vec = light.position - hit_rec.point
  light_dist = light_vec.length
  l = light_vec.normalize

  # Check for shadows
  shadow_ray = Ray.new(hit_rec.point + n * 0.001, l)
  in_shadow = false

  scene.objects.each do |obj|
    shadow_hit = intersect(shadow_ray, obj)
    if shadow_hit.hit && shadow_hit.t < light_dist
      in_shadow = true
      break
    end
  end

  return vec3(0, 0, 0) if in_shadow

  # Lighting calculations
  n_dot_l = [n.dot(l), 0.0].max
  return vec3(0, 0, 0) if n_dot_l < 1e-6

  # Attenuation
  attenuation = light.intensity / (light_dist * light_dist + 1.0)

  # Diffuse component
  diffuse = mat.albedo * n_dot_l

  # Specular component (simplified Cook-Torrance)
  h = (v + l).normalize
  n_dot_h = [n.dot(h), 0.0].max
  n_dot_v = [n.dot(v), 0.0].max

  # Roughness affects specular highlight size
  roughness = [[mat.roughness, 0.01].max, 1.0].min
  alpha = roughness * roughness
  specular_power = 2.0 / (alpha * alpha) - 2.0

  # Base reflectivity (F0)
  f0 = vec3(0.04, 0.04, 0.04)
  f0 = mat.albedo if mat.metalness > 0.5 # Metals use albedo as F0

  # Fresnel
  fresnel = fresnel_schlick([n_dot_v, 1.0].min, f0)

  # Specular highlight
  spec_strength = n_dot_h**specular_power
  specular = fresnel * spec_strength

  # Combine diffuse and specular based on metalness
  # Metals have no diffuse component
  diffuse_contrib = diffuse * (1.0 - mat.metalness)
  specular_contrib = specular

  # Final color
  color = (diffuse_contrib + specular_contrib) * attenuation
  color.clamp01
end

# Trace a ray through the scene
def trace_ray(ray, scene)
  # Find closest intersection
  closest = HitRecord.new

  scene.objects.each do |obj|
    hit = intersect(ray, obj)
    if hit.hit && hit.t < closest.t
      closest = hit
    end
  end

  # If no hit, return background
  return scene.background unless closest.hit

  # Shade the hit point
  shade(closest, scene.light, ray.direction, scene)
end
