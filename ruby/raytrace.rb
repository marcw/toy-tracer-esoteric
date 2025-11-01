#!/usr/bin/env ruby
# frozen_string_literal: true

require_relative 'tracer'
require_relative 'scene'

# ============================================================================
# TGA FILE OUTPUT
# ============================================================================

# Write image buffer as uncompressed 32-bit TGA file
# TGA format: simple raster format, no compression, BGRA byte order
def write_tga(filename, width, height, pixels)
  File.open(filename, 'wb') do |f|
    # TGA Header (18 bytes)
    f.write([0].pack('C'))          # ID length
    f.write([0].pack('C'))          # Color map type (0 = no color map)
    f.write([2].pack('C'))          # Image type (2 = uncompressed true-color)
    f.write([0].pack('v'))          # Color map first entry index
    f.write([0].pack('v'))          # Color map length
    f.write([0].pack('C'))          # Color map entry size
    f.write([0].pack('v'))          # X-origin
    f.write([0].pack('v'))          # Y-origin
    f.write([width].pack('v'))      # Image width
    f.write([height].pack('v'))     # Image height
    f.write([32].pack('C'))         # Pixel depth (32 bits = BGRA)
    f.write([8].pack('C'))          # Image descriptor (8 = origin at top-left, 8-bit alpha)

    # Write pixel data (BGRA format)
    # TGA expects bottom-to-top, but we write top-to-bottom with descriptor bit
    pixels.each do |color|
      # Convert float [0,1] to byte [0,255]
      r = (color.x * 255.0).clamp(0, 255).to_i
      g = (color.y * 255.0).clamp(0, 255).to_i
      b = (color.z * 255.0).clamp(0, 255).to_i
      a = 255

      # Write as BGRA
      f.write([b, g, r, a].pack('C4'))
    end
  end

  puts "Image written to #{filename}"
end

# ============================================================================
# CAMERA AND RENDERING
# ============================================================================

# Camera configuration
class Camera
  attr_accessor :position, :look_at, :up, :fov, :aspect

  def initialize
    @position = vec3(0, 0, 0)       # Camera position
    @look_at = vec3(0, 0, 1)        # Look direction
    @up = vec3(0, 1, 0)             # Up vector
    @fov = 60.0                     # Field of view in degrees
    @aspect = 16.0 / 9.0            # Aspect ratio
  end

  # Generate ray for pixel coordinates (u, v in [0, 1])
  def get_ray(u, v)
    # Calculate viewport dimensions
    theta = @fov * Math::PI / 180.0
    viewport_height = 2.0 * Math.tan(theta / 2.0)
    viewport_width = @aspect * viewport_height

    # Camera basis vectors
    w = (@position - @look_at).normalize  # Camera looks along -w
    u_vec = @up.cross(w).normalize        # Right vector
    v_vec = w.cross(u_vec)                # Corrected up vector

    # Viewport corners
    half_width = viewport_width / 2.0
    half_height = viewport_height / 2.0

    # Ray direction
    # Map from [0,1] to [-1,1] and scale by viewport dimensions
    horizontal = u_vec * viewport_width
    vertical = v_vec * viewport_height
    lower_left = @position - w - horizontal / 2.0 - vertical / 2.0

    # Generate ray
    direction = (lower_left + horizontal * u + vertical * v - @position).normalize
    Ray.new(@position, direction)
  end
end

# Main rendering function
def render(scene, width, height)
  pixels = []
  camera = Camera.new

  puts "Rendering #{width}x#{height} image..."
  puts "Camera at #{camera.position}, FOV #{camera.fov}°"
  puts "Scene contains #{scene.objects.length} objects"

  start_time = Time.now

  # Render each pixel
  height.times do |y|
    # Progress indicator
    if y % 20 == 0
      progress = (y.to_f / height * 100).to_i
      puts "Progress: #{progress}% (row #{y}/#{height})"
    end

    width.times do |x|
      # Calculate normalized coordinates [0, 1]
      u = x.to_f / (width - 1)
      v = 1.0 - (y.to_f / (height - 1)) # Flip Y for top-to-bottom raster

      # Generate ray
      ray = camera.get_ray(u, v)

      # Trace ray
      color = trace_ray(ray, scene)

      pixels << color
    end
  end

  elapsed = Time.now - start_time
  puts "Rendering complete in #{elapsed.round(2)} seconds"

  pixels
end

# ============================================================================
# MAIN PROGRAM
# ============================================================================

def main
  puts '=' * 60
  puts 'Ruby Educational Raytracer'
  puts 'Rendering scene with 5 primitive types'
  puts '=' * 60
  puts

  # Setup scene
  scene = setup_scene

  # Render configuration
  # Start with small resolution for testing
  # Students can increase to 1920x1080 for final renders
  width = 640
  height = 360

  # Render
  pixels = render(scene, width, height)

  # Write output
  write_tga('output.tga', width, height, pixels)

  puts
  puts 'Done! Open output.tga to view the rendered image.'
  puts
end

# Run the raytracer
main if __FILE__ == $PROGRAM_NAME
