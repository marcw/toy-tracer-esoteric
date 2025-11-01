#!/bin/bash
# Simple build script for the raytracer
# Usage: ./build.sh

echo "Compiling raytracer..."
gcc main.c tracer.c scene.c -lm -o raytracer

if [ $? -eq 0 ]; then
    echo "Build successful! Run with: ./raytracer"
else
    echo "Build failed!"
    exit 1
fi
