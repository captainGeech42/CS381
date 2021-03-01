from PIL import Image
im = Image.open("eric2.png")
rgb_im = im.convert("RGB")

with open("../amazing.hs", "w") as f:
    f.write("module Amazing where\nimport MiniLogo\nimport Render\nimport HW5\namazing :: Prog\namazing = Program []\n")
    f.write("  [ Move (Lit 0) (Lit 0)\n")
    f.write("  , Pen Down\n")
    for x in range(rgb_im.width):
        for y in range(rgb_im.height):
            r, g, b = rgb_im.getpixel((x, y))
            color = (r << 16) + (g << 8) + b
            f.write(f"  , SetColor (Color {color})\n")
            f.write(f"  , Move (Lit {x}) (Lit {y+1})\n")
        
        f.write(f"  , Move (Lit {x+1}) (Lit 0)\n")
    f.write("  ]\n")

    f.write("main = do draw amazing")