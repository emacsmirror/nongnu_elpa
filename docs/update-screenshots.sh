#!/usr/bin/env bash
# Picks the two most recent PNGs from ~/Pictures/Screenshots/ (newest = dark,
# second-newest = light) and crops them into ./img/.
#
# --- Sizing/positioning Emacs ---------------------------------------------
# Current frame is at (22, 26) with outer dims 2636x1016. Eval inside Emacs
# (M-:) to put a frame back in that spot. Use 2621x950 inner pixels to match
# the default crops below, or 2636x1016 for the gdbus-captured alt crops.
#
#   (progn (set-frame-position nil 22 26)
#          (set-frame-size     nil 2621 950 t))   ; t = pixels, not chars
#
# --- Auto-screenshot via GNOME Shell --------------------------------------
# Captures a screen region without the interactive picker. Uncomment and run
# once per theme (toggle light/dark in between) — the two newest files in
# ~/Pictures/Screenshots/ are picked up below.
#
#   gdbus call --session \
#     --dest org.gnome.Shell.Screenshot \
#     --object-path /org/gnome/Shell/Screenshot \
#     --method org.gnome.Shell.Screenshot.ScreenshotArea \
#     22 26 2636 1016 false \
#     "$HOME/Pictures/Screenshots/Screenshot from $(date '+%Y-%m-%d %H-%M-%S').png"
# --------------------------------------------------------------------------

set -euo pipefail
cd "$(dirname "$0")"

mapfile -t shots < <(ls -t "$HOME/Pictures/Screenshots/"*.png 2>/dev/null | head -2)
LIGHT="${shots[1]:?need at least two screenshots in ~/Pictures/Screenshots/}"
DARK="${shots[0]}"
echo "LIGHT: $LIGHT"
echo "DARK:  $DARK"

# Source images are 2621x950 (Emacs window with decorations).
# Crop syntax: WIDTHxHEIGHT+X_OFFSET+Y_OFFSET
convert "$LIGHT" -strip -crop 1304x944+8+4    +repage img/describe-light.png
convert "$DARK"  -strip -crop 1304x944+8+4    +repage img/describe-dark.png
convert "$LIGHT" -strip -crop 1304x944+1315+4 +repage img/flamegraph-light.png
convert "$DARK"  -strip -crop 1304x944+1315+4 +repage img/flamegraph-dark.png

# Alt crops for a 2636x1016 PNG captured via the gdbus command above. The
# captured region still includes the GTK decorations, so the 8/4 insets stay.
# convert "$LIGHT" -strip -crop 1311x1010+8+4    +repage img/describe-light.png
# convert "$DARK"  -strip -crop 1311x1010+8+4    +repage img/describe-dark.png
# convert "$LIGHT" -strip -crop 1311x1010+1322+4 +repage img/flamegraph-light.png
# convert "$DARK"  -strip -crop 1311x1010+1322+4 +repage img/flamegraph-dark.png
