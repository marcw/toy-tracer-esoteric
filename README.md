# Toy Tracer

A couple of experiments based on [astrofra/toy-tracer-c](astrofra/toy-tracer-c). 

Each directory contains an `AGENTS.md` file that contains instructions to build
a raytracer. The most esoteric the language for a raytracer, the better.

## Results

| Language  | Model      | Output                                        |
|-----------|------------|-----------------------------------------------|
| Reference | Sonnet 4.5 | ![output-reference.png](output-reference.png) |
| Erlang    | Sonnet 4.5 (multiple iterations) | ![erlang](erlang/output.png)                  |
| C         | Sonnet 4.5 (oneshot) | ![c](c/output.png)                            |
| Ruby      | Sonnet 4.5 (oneshot) | ![ruby](ruby/output.png)                      |

## Notes

`.tga` images are converted using `ffmpeg -i output.tga output.png`
