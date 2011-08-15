import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.CustomKeys

main = do
  xmproc <- spawnPipe "~/bin/xmobar"
  xmonad $ defaultConfig
    { terminal = "urxvt"
    , modMask = mod4Mask
    , borderWidth = 2 
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#77dd77"
    , layoutHook = avoidStruts $ myLayout
    , keys = customKeys delkeys inskeys
    }

myLayout = Full ||| Mirror tiled ||| tiled
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

delkeys XConfig {modMask = modm} = []
inskeys conf@(XConfig {modMask = modm}) =
    [ ((mod4Mask, xK_F1), spawn "setxkbmap us")
    , ((mod4Mask, xK_F2), spawn "setxkbmap 'ca(fr)'")
    , ((mod4Mask, xK_p), spawn "gmrun")
    , ((mod4Mask .|. shiftMask, xK_q), spawn "gnome-session-quit --lo gout")
    ]
