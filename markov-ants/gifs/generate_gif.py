from PIL import Image
import numpy as np
import random
import os
import glob
from natsort import natsorted

# Number of pixels from the center of the square to the edge
# Change this to modify the size of the square
# Needs to be odd
n_pixels = 15

CUR_PATH = os.getcwd()
# Where the pngs are going to be stored prior to building the GIF
PNG_PATH = os.path.join(CUR_PATH, "gifs", "pngs")
# Where the gif is going to be stored / found for deletion when resetting
GIF_PATH = os.path.join(CUR_PATH, "gifs", "image.gif")

WHITE = [255, 255, 255]
BLACK = [0, 0, 0]

RED = [255, 0, 0]


def generate_gif(n_pixels: int):
    ''' 
    n := Number of pixels from the center of the square to the edge
    Merge generated pngs from @generate_frames into a gif 
    '''

    assert n_pixels % 2, "Error: n_pixels must be odd"

    generate_frames(n_pixels)

    pngs_folder_contents = os.path.join(PNG_PATH, "*.png")

    # Get an iterator of sorted image file paths
    imgs = (Image.open(f) for f in natsorted(glob.glob(pngs_folder_contents)))

    # Extract first image from iterator
    img = next(imgs)

    # Save the animated GIF
    img.save(
        fp=GIF_PATH,
        format='GIF',
        append_images=imgs,
        save_all=True,
        fps=50,
        loop=0
    )  # duration=200 -> better with setting fps


def generate_frames(n_pixels: int):
    ''' Stores a bunch of pngs in folder /pngs '''

    # Clear PNG_PATH folder if it exists, otherwise create it
    if os.path.exists(PNG_PATH):
        files = glob.glob(os.path.join(PNG_PATH, '*'))
        for file in files:
            os.remove(file)
    else:
        os.makedirs(PNG_PATH)

    # Clear previous GIF if it exists
    if os.path.exists(GIF_PATH):
        os.remove(GIF_PATH)

    frames = generate_frames_array(n_pixels)

    for idx, frame in enumerate(frames):
        img = Image.fromarray(frame, 'RGB').resize((200, 200))
        name = os.path.join(PNG_PATH, f'image_{idx}.png')
        img.save(name)


def generate_frames_array(n_pixels: int):
    ''' Returns a list of frames. A frame is a np.array object '''

    # offset between the white border and the edge of the GIF. NEEDS to be > 0
    ofs = 2
    # size of a side of the square
    side = 2 * n_pixels - 1
    side_ofs = side + 2 * ofs

    initial_frame = np.zeros((side_ofs, side_ofs, 3), dtype=np.uint8)

    # Colors the border in white
    for i in range(side_ofs):
        if ofs-1 <= i <= side_ofs-1-(ofs-1):
            initial_frame[ofs-1, i] = WHITE
            initial_frame[side_ofs-1-(ofs-1)][i] = WHITE
            initial_frame[i, ofs-1] = WHITE
            initial_frame[i][side_ofs-1-(ofs-1)] = WHITE

    # Initializes x, y and saves up the center coordinates
    center = [n_pixels - 1 + ofs, n_pixels - 1 + ofs]
    x, y = center
    xc, yc = center

    initial_frame[x, y] = WHITE
    frames = [initial_frame]

    # Degradation coefficient, the higher the faster the trail vanishes
    coef = 1.05

    while max(abs(x - xc), abs(y - yc)) < n_pixels:
        new_frame = frames[-1].copy()

        # Coloring of previously visited pixels for easier visualization:
        # Degradates the whole array, except for the border
        # -- also degradates the black, even though it's not relevant
        for i in range(side_ofs):
            for j in range(side_ofs):
                if ofs-1 < i < side_ofs-1-(ofs-1) and ofs-1 < j < side_ofs-1-(ofs-1):
                    r, g, b = new_frame[i, j]
                    new_frame[i, j] = [r / coef, g / coef, b / coef]

        new_frame[x, y] = RED

        rand = random.randrange(0, 3)
        if rand == 0:
            y += 1
        elif rand == 1:
            y -= 1
        elif rand == 2:
            x += 1
        elif rand == 3:
            x -= 1

        frames.append(new_frame)

    # End visualization: for easier spotting of the ant reaching the edge.
    # It flickers between RED and WHITE
    n_flickers = 20

    for idx in range(n_flickers):
        new_frame = frames[-1].copy()

        color = RED if idx % 2 else WHITE
        for i in range(side_ofs):
            if ofs-1 <= i <= side_ofs-1-(ofs-1):
                new_frame[ofs-1, i] = color
                new_frame[side_ofs-1-(ofs-1)][i] = color
                new_frame[i, ofs-1] = color
                new_frame[i][side_ofs-1-(ofs-1)] = color

        # Don't flicker the pixel where the ant reached the edge
        new_frame[x, y] = RED

        frames.append(new_frame)

    return frames


generate_gif(n_pixels)
