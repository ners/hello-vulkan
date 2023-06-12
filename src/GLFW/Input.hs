{-# LANGUAGE LambdaCase #-}
module GLFW.Input where

-- GLFW-b
import Prelude
import Graphics.UI.GLFW qualified as GLFW

-- hagato:with-core
import Core qualified

mapKey :: GLFW.Key -> Core.Key
mapKey = \case
  GLFW.Key'Unknown      -> Core.Key'Unknown
  GLFW.Key'Space        -> Core.Key'Space
  GLFW.Key'Apostrophe   -> Core.Key'Apostrophe
  GLFW.Key'Comma        -> Core.Key'Comma
  GLFW.Key'Minus        -> Core.Key'Minus
  GLFW.Key'Period       -> Core.Key'Period
  GLFW.Key'Slash        -> Core.Key'Slash
  GLFW.Key'0            -> Core.Key'0
  GLFW.Key'1            -> Core.Key'1
  GLFW.Key'2            -> Core.Key'2
  GLFW.Key'3            -> Core.Key'3
  GLFW.Key'4            -> Core.Key'4
  GLFW.Key'5            -> Core.Key'5
  GLFW.Key'6            -> Core.Key'6
  GLFW.Key'7            -> Core.Key'7
  GLFW.Key'8            -> Core.Key'8
  GLFW.Key'9            -> Core.Key'9
  GLFW.Key'Semicolon    -> Core.Key'Semicolon
  GLFW.Key'Equal        -> Core.Key'Equal
  GLFW.Key'A            -> Core.Key'A
  GLFW.Key'B            -> Core.Key'B
  GLFW.Key'C            -> Core.Key'C
  GLFW.Key'D            -> Core.Key'D
  GLFW.Key'E            -> Core.Key'E
  GLFW.Key'F            -> Core.Key'F
  GLFW.Key'G            -> Core.Key'G
  GLFW.Key'H            -> Core.Key'H
  GLFW.Key'I            -> Core.Key'I
  GLFW.Key'J            -> Core.Key'J
  GLFW.Key'K            -> Core.Key'K
  GLFW.Key'L            -> Core.Key'L
  GLFW.Key'M            -> Core.Key'M
  GLFW.Key'N            -> Core.Key'N
  GLFW.Key'O            -> Core.Key'O
  GLFW.Key'P            -> Core.Key'P
  GLFW.Key'Q            -> Core.Key'Q
  GLFW.Key'R            -> Core.Key'R
  GLFW.Key'S            -> Core.Key'S
  GLFW.Key'T            -> Core.Key'T
  GLFW.Key'U            -> Core.Key'U
  GLFW.Key'V            -> Core.Key'V
  GLFW.Key'W            -> Core.Key'W
  GLFW.Key'X            -> Core.Key'X
  GLFW.Key'Y            -> Core.Key'Y
  GLFW.Key'Z            -> Core.Key'Z
  GLFW.Key'LeftBracket  -> Core.Key'LeftBracket
  GLFW.Key'Backslash    -> Core.Key'Backslash
  GLFW.Key'RightBracket -> Core.Key'RightBracket
  GLFW.Key'GraveAccent  -> Core.Key'GraveAccent
  GLFW.Key'World1       -> Core.Key'World1
  GLFW.Key'World2       -> Core.Key'World2
  GLFW.Key'Escape       -> Core.Key'Escape
  GLFW.Key'Enter        -> Core.Key'Enter
  GLFW.Key'Tab          -> Core.Key'Tab
  GLFW.Key'Backspace    -> Core.Key'Backspace
  GLFW.Key'Insert       -> Core.Key'Insert
  GLFW.Key'Delete       -> Core.Key'Delete
  GLFW.Key'Right        -> Core.Key'Right
  GLFW.Key'Left         -> Core.Key'Left
  GLFW.Key'Down         -> Core.Key'Down
  GLFW.Key'Up           -> Core.Key'Up
  GLFW.Key'PageUp       -> Core.Key'PageUp
  GLFW.Key'PageDown     -> Core.Key'PageDown
  GLFW.Key'Home         -> Core.Key'Home
  GLFW.Key'End          -> Core.Key'End
  GLFW.Key'CapsLock     -> Core.Key'CapsLock
  GLFW.Key'ScrollLock   -> Core.Key'ScrollLock
  GLFW.Key'NumLock      -> Core.Key'NumLock
  GLFW.Key'PrintScreen  -> Core.Key'PrintScreen
  GLFW.Key'Pause        -> Core.Key'Pause
  GLFW.Key'F1           -> Core.Key'F1
  GLFW.Key'F2           -> Core.Key'F2
  GLFW.Key'F3           -> Core.Key'F3
  GLFW.Key'F4           -> Core.Key'F4
  GLFW.Key'F5           -> Core.Key'F5
  GLFW.Key'F6           -> Core.Key'F6
  GLFW.Key'F7           -> Core.Key'F7
  GLFW.Key'F8           -> Core.Key'F8
  GLFW.Key'F9           -> Core.Key'F9
  GLFW.Key'F10          -> Core.Key'F10
  GLFW.Key'F11          -> Core.Key'F11
  GLFW.Key'F12          -> Core.Key'F12
  GLFW.Key'F13          -> Core.Key'F13
  GLFW.Key'F14          -> Core.Key'F14
  GLFW.Key'F15          -> Core.Key'F15
  GLFW.Key'F16          -> Core.Key'F16
  GLFW.Key'F17          -> Core.Key'F17
  GLFW.Key'F18          -> Core.Key'F18
  GLFW.Key'F19          -> Core.Key'F19
  GLFW.Key'F20          -> Core.Key'F20
  GLFW.Key'F21          -> Core.Key'F21
  GLFW.Key'F22          -> Core.Key'F22
  GLFW.Key'F23          -> Core.Key'F23
  GLFW.Key'F24          -> Core.Key'F24
  GLFW.Key'F25          -> Core.Key'F25
  GLFW.Key'Pad0         -> Core.Key'Pad0
  GLFW.Key'Pad1         -> Core.Key'Pad1
  GLFW.Key'Pad2         -> Core.Key'Pad2
  GLFW.Key'Pad3         -> Core.Key'Pad3
  GLFW.Key'Pad4         -> Core.Key'Pad4
  GLFW.Key'Pad5         -> Core.Key'Pad5
  GLFW.Key'Pad6         -> Core.Key'Pad6
  GLFW.Key'Pad7         -> Core.Key'Pad7
  GLFW.Key'Pad8         -> Core.Key'Pad8
  GLFW.Key'Pad9         -> Core.Key'Pad9
  GLFW.Key'PadDecimal   -> Core.Key'PadDecimal
  GLFW.Key'PadDivide    -> Core.Key'PadDivide
  GLFW.Key'PadMultiply  -> Core.Key'PadMultiply
  GLFW.Key'PadSubtract  -> Core.Key'PadSubtract
  GLFW.Key'PadAdd       -> Core.Key'PadAdd
  GLFW.Key'PadEnter     -> Core.Key'PadEnter
  GLFW.Key'PadEqual     -> Core.Key'PadEqual
  GLFW.Key'LeftShift    -> Core.Key'LeftShift
  GLFW.Key'LeftControl  -> Core.Key'LeftControl
  GLFW.Key'LeftAlt      -> Core.Key'LeftAlt
  GLFW.Key'LeftSuper    -> Core.Key'LeftSuper
  GLFW.Key'RightShift   -> Core.Key'RightShift
  GLFW.Key'RightControl -> Core.Key'RightControl
  GLFW.Key'RightAlt     -> Core.Key'RightAlt
  GLFW.Key'RightSuper   -> Core.Key'RightSuper
  GLFW.Key'Menu         -> Core.Key'Menu

mapKeyState :: GLFW.KeyState -> Core.KeyState
mapKeyState = \case
  GLFW.KeyState'Pressed   -> Core.Key'Pressed
  GLFW.KeyState'Released  -> Core.Key'Released
  GLFW.KeyState'Repeating -> Core.Key'Repeating

mapModifiers :: GLFW.ModifierKeys -> Core.Modifiers
mapModifiers = \case
  GLFW.ModifierKeys shift ctrl alt super caps num ->
    Core.Modifiers shift ctrl alt super caps num

mapMouseButton :: GLFW.MouseButton -> Core.MouseButton
mapMouseButton = \case
  GLFW.MouseButton'1 -> Core.Mouse'Left
  GLFW.MouseButton'2 -> Core.Mouse'Right
  GLFW.MouseButton'3 -> Core.Mouse'Middle
  GLFW.MouseButton'4 -> Core.Mouse'Extra1
  GLFW.MouseButton'5 -> Core.Mouse'Extra2
  GLFW.MouseButton'6 -> Core.Mouse'Extra3
  GLFW.MouseButton'7 -> Core.Mouse'Extra4
  GLFW.MouseButton'8 -> Core.Mouse'Extra5

mapMouseButtonState :: GLFW.MouseButtonState -> Core.MouseButtonState
mapMouseButtonState = \case
  GLFW.MouseButtonState'Pressed  -> Core.Mouse'Pressed
  GLFW.MouseButtonState'Released -> Core.Mouse'Released
