# animate
Animation for sprites

---

`animate` is a general purpose animation library as it's used for graphical 2D and terminal games.
Each key of an animation are left opened to custom types.
This is in an effort to avoid using numbers or strings as indices and be reusable.

---

[Example using SDL2](https://github.com/jxv/animate-sdl2/tree/master/example) (and includes loading from "Paths")

---

## Sprite Information

Sprite loaders are provided but aren't required.
While the loaders are opininated compared to the rest of the library, they aren't bound to any graphics library.
And many pieces are left exposed if you wish piece together some other variation.

The YAML (and JSON) files describe very typical sprite information:

```yaml
# File path of spritesheet
image: "dino.png"

# Colorkey for transparency
# Optional: It's a tuple of [Red, Green, Blue] between 0-255 values.
alpha: [255, 0, 255]

# Clip is a portion of the sprite sheet
# Optional: The offset is optional.
# [x, y, width, height]
# [x, y, width, height, offsetX, offsetY] 

clips:
# Idle                      # index
- [  0, 0, 48, 48]          # 0
- [ 48, 0, 48, 48]          # 1
- [ 96, 0, 48, 48, 24, 42]  # 2
- [144, 0, 48, 48, 24, 42]  # 3

# Move
- [192, 0, 48, 48, 24, 42]  # 4
- [240, 0, 48, 48, 24, 42]  # 5
- [288, 0, 48, 48, 24, 42]  # 6
- [336, 0, 48, 48, 24, 42]  # 7
- [384, 0, 48, 48, 24, 42]  # 8
- [432, 0, 48, 48, 24, 42]  # 9

# And so on...

# Animation frames are defined by the clip indices and delay time for each clip
animations:
  Idle:
  # [index, delay (seconds)]
  - [0, 0.2]
  - [1, 0.2]
  - [2, 0.2]
  - [3, 0.2]

  Move:
  - [4, 0.01]
  - [5, 0.01]
  - [6, 0.01]
  - [7, 0.01]
  - [8, 0.01]
  - [9, 0.01]

  # And so on...
```