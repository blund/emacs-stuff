

# https://cute-jumper.github.io/emacs/2016/02/22/my-simple-setup-to-avoid-rsi-in-emacs

# -- Hentet fra: https://cute-jumper.github.io/emacs/2016/02/22/my-simple-setup-to-avoid-rsi-in-emacs
# Map an unused modifier's keysym to the spacebar's keycode and make it a
# control modifier. It needs to be an existing key so that emacs won't
# spazz out when you press it. Hyper_L is a good candidate.
xmodmap -e "keycode 65  = Hyper_L"
xmodmap -e "remove mod4 = Hyper_L" # hyper_l is mod4 by default
xmodmap -e "add Control = Hyper_L"

# Map space to an unused keycode (to keep it around for xcape to
# use).
xmodmap -e "keycode any = space"

# Finally use xcape to cause the space bar to generate a space when tapped.
xcape -e "Hyper_L=space"

xmodmap -e "keycode 66 = Shift_L"

# -- Sett opp ctrl til å være escape når den tæppes :)
xcape -e 'Shift_L=Escape'


