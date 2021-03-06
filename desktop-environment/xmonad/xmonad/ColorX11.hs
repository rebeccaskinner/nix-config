{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module ColorX11 where

import ColorType
import Data.Aeson
import GHC.Generics

data X11Color
  = AliceBlue
  | AntiqueWhite
  | Aqua
  | Aquamarine
  | Azure
  | Beige
  | Bisque
  | Black
  | BlanchedAlmond
  | Blue
  | BlueViolet
  | Brown
  | Burlywood
  | CadetBlue
  | Chartreuse
  | Chocolate
  | Coral
  | CornflowerBlue
  | Cornsilk
  | Crimson
  | Cyan
  | DarkBlue
  | DarkCyan
  | DarkGoldenrod
  | DarkGray
  | DarkGreen
  | DarkKhaki
  | DarkMagenta
  | DarkOlive
  | DarkOrange
  | DarkOrchid
  | DarkRed
  | DarkSalmon
  | DarkSea
  | DarkSlateBlue
  | DarkSlateGray
  | DarkTurquoise
  | DarkViolet
  | DeepPink
  | DeepSkyBlue
  | DimGray
  | DodgerBlue
  | Firebrick
  | FloralWhite
  | ForestGreen
  | Fuchsia
  | Gainsboro
  | GhostWhite
  | Gold
  | Goldenrod
  | Gray
  | WebGray
  | Green
  | WebGreen
  | GreenYellow
  | Honeydew
  | HotPink
  | Indian
  | Indigo
  | Ivory
  | Khaki
  | Lavender
  | LavenderBlush
  | LawnGreen
  | LemonChiffon
  | LightBlue
  | LightCoral
  | LightCyan
  | LightGoldenrod
  | LightGray
  | LightGreen
  | LightPink
  | LightSalmon
  | LightSea
  | LightSky
  | LightSlate
  | LightSteel
  | LightYellow
  | Lime
  | LimeGreen
  | Linen
  | Magenta
  | Maroon
  | Web
  | MediumAquamarine
  | MediumBlue
  | MediumOrchid
  | MediumPurple
  | MediumSeaGreen
  | MediumSlateBlue
  | MediumSpringGreen
  | MediumTurquoise
  | MediumVioletRed
  | MidnightBlue
  | MintCream
  | MistyRose
  | Moccasin
  | NavajoWhite
  | NavyBlue
  | OldLace
  | Olive
  | OliveDrab
  | Orange
  | OrangeRed
  | Orchid
  | PaleGoldenrod
  | PaleGreen
  | PaleTurquoise
  | PaleViolet
  | PapayaWhip
  | PeachPuff
  | Peru
  | Pink
  | Plum
  | Powder
  | Purple
  | WebPurple
  | RebeccaPurple
  | Red
  | RosyBrown
  | RoyalBlue
  | SaddleBrown
  | Salmon
  | SandyBrown
  | SeaGreen
  | Seashell
  | Sienna
  | Silver
  | SkyBlue
  | SlateBlue
  | SlateGray
  | Snow
  | SpringGreen
  | SteelBlue
  | Tan
  | Teal
  | Thistle
  | Tomato
  | Turquoise
  | Violet
  | Wheat
  | White
  | WhiteSmoke
  | Yellow
  | YellowGreen
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)

instance ToJSON X11Color

instance FromJSON X11Color where
  parseJSON = genericParseJSON defaultOptions

instance IsColor X11Color where
  toColor = \case
    AliceBlue -> RGBColor 0xF0 0xF8 0xFF
    AntiqueWhite -> RGBColor 0xFA 0xEB 0xD7
    Aqua -> RGBColor 0x00 0xFF 0xFF
    Aquamarine -> RGBColor 0x7F 0xFF 0xD4
    Azure -> RGBColor 0xF0 0xFF 0xFF
    Beige -> RGBColor 0xF5 0xF5 0xDC
    Bisque -> RGBColor 0xFF 0xE4 0xC4
    Black -> RGBColor 0x00 0x00 0x00
    BlanchedAlmond -> RGBColor 0xFF 0xEB 0xCD
    Blue -> RGBColor 0x00 0x00 0xFF
    BlueViolet -> RGBColor 0x8A 0x2B 0xE2
    Brown -> RGBColor 0xA5 0x2A 0x2A
    Burlywood -> RGBColor 0xDE 0xB8 0x87
    CadetBlue -> RGBColor 0x5F 0x9E 0xA0
    Chartreuse -> RGBColor 0x7F 0xFF 0x00
    Chocolate -> RGBColor 0xD2 0x69 0x1E
    Coral -> RGBColor 0xFF 0x7F 0x50
    CornflowerBlue -> RGBColor 0x64 0x95 0xED
    Cornsilk -> RGBColor 0xFF 0xF8 0xDC
    Crimson -> RGBColor 0xDC 0x14 0x3C
    Cyan -> RGBColor 0x00 0xFF 0xFF
    DarkBlue -> RGBColor 0x00 0x00 0x8B
    DarkCyan -> RGBColor 0x00 0x8B 0x8B
    DarkGoldenrod -> RGBColor 0xB8 0x86 0x0B
    DarkGray -> RGBColor 0xA9 0xA9 0xA9
    DarkGreen -> RGBColor 0x00 0x64 0x00
    DarkKhaki -> RGBColor 0xBD 0xB7 0x6B
    DarkMagenta -> RGBColor 0x8B 0x00 0x8B
    DarkOlive -> RGBColor 0x55 0x6B 0x2F
    DarkOrange -> RGBColor 0xFF 0x8C 0x00
    DarkOrchid -> RGBColor 0x99 0x32 0xCC
    DarkRed -> RGBColor 0x8B 0x00 0x00
    DarkSalmon -> RGBColor 0xE9 0x96 0x7A
    DarkSea -> RGBColor 0x8F 0xBC 0x8F
    DarkSlateBlue -> RGBColor 0x48 0x3D 0x8B
    DarkSlateGray -> RGBColor 0x2F 0x4F 0x4F
    DarkTurquoise -> RGBColor 0x00 0xCE 0xD1
    DarkViolet -> RGBColor 0x94 0x00 0xD3
    DeepPink -> RGBColor 0xFF 0x14 0x93
    DeepSkyBlue -> RGBColor 0x00 0xBF 0xFF
    DimGray -> RGBColor 0x69 0x69 0x69
    DodgerBlue -> RGBColor 0x1E 0x90 0xFF
    Firebrick -> RGBColor 0xB2 0x22 0x22
    FloralWhite -> RGBColor 0xFF 0xFA 0xF0
    ForestGreen -> RGBColor 0x22 0x8B 0x22
    Fuchsia -> RGBColor 0xFF 0x00 0xFF
    Gainsboro -> RGBColor 0xDC 0xDC 0xDC
    GhostWhite -> RGBColor 0xF8 0xF8 0xFF
    Gold -> RGBColor 0xFF 0xD7 0x00
    Goldenrod -> RGBColor 0xDA 0xA5 0x20
    Gray -> RGBColor 0xBE 0xBE 0xBE
    WebGray -> RGBColor 0x80 0x80 0x80
    Green -> RGBColor 0x00 0xFF 0x00
    WebGreen -> RGBColor 0x00 0x80 0x00
    GreenYellow -> RGBColor 0xAD 0xFF 0x2F
    Honeydew -> RGBColor 0xF0 0xFF 0xF0
    HotPink -> RGBColor 0xFF 0x69 0xB4
    Indian -> RGBColor 0xCD 0x5C 0x5C
    Indigo -> RGBColor 0x4B 0x00 0x82
    Ivory -> RGBColor 0xFF 0xFF 0xF0
    Khaki -> RGBColor 0xF0 0xE6 0x8C
    Lavender -> RGBColor 0xE6 0xE6 0xFA
    LavenderBlush -> RGBColor 0xFF 0xF0 0xF5
    LawnGreen -> RGBColor 0x7C 0xFC 0x00
    LemonChiffon -> RGBColor 0xFF 0xFA 0xCD
    LightBlue -> RGBColor 0xAD 0xD8 0xE6
    LightCoral -> RGBColor 0xF0 0x80 0x80
    LightCyan -> RGBColor 0xE0 0xFF 0xFF
    LightGoldenrod -> RGBColor 0xFA 0xFA 0xD2
    LightGray -> RGBColor 0xD3 0xD3 0xD3
    LightGreen -> RGBColor 0x90 0xEE 0x90
    LightPink -> RGBColor 0xFF 0xB6 0xC1
    LightSalmon -> RGBColor 0xFF 0xA0 0x7A
    LightSea -> RGBColor 0x20 0xB2 0xAA
    LightSky -> RGBColor 0x87 0xCE 0xFA
    LightSlate -> RGBColor 0x77 0x88 0x99
    LightSteel -> RGBColor 0xB0 0xC4 0xDE
    LightYellow -> RGBColor 0xFF 0xFF 0xE0
    Lime -> RGBColor 0x00 0xFF 0x00
    LimeGreen -> RGBColor 0x32 0xCD 0x32
    Linen -> RGBColor 0xFA 0xF0 0xE6
    Magenta -> RGBColor 0xFF 0x00 0xFF
    Maroon -> RGBColor 0xB0 0x30 0x60
    Web -> RGBColor 0x80 0x00 0x00
    MediumAquamarine -> RGBColor 0x66 0xCD 0xAA
    MediumBlue -> RGBColor 0x00 0x00 0xCD
    MediumOrchid -> RGBColor 0xBA 0x55 0xD3
    MediumPurple -> RGBColor 0x93 0x70 0xDB
    MediumSeaGreen -> RGBColor 0x3C 0xB3 0x71
    MediumSlateBlue -> RGBColor 0x7B 0x68 0xEE
    MediumSpringGreen -> RGBColor 0x00 0xFA 0x9A
    MediumTurquoise -> RGBColor 0x48 0xD1 0xCC
    MediumVioletRed -> RGBColor 0xC7 0x15 0x85
    MidnightBlue -> RGBColor 0x19 0x19 0x70
    MintCream -> RGBColor 0xF5 0xFF 0xFA
    MistyRose -> RGBColor 0xFF 0xE4 0xE1
    Moccasin -> RGBColor 0xFF 0xE4 0xB5
    NavajoWhite -> RGBColor 0xFF 0xDE 0xAD
    NavyBlue -> RGBColor 0x00 0x00 0x80
    OldLace -> RGBColor 0xFD 0xF5 0xE6
    Olive -> RGBColor 0x80 0x80 0x00
    OliveDrab -> RGBColor 0x6B 0x8E 0x23
    Orange -> RGBColor 0xFF 0xA5 0x00
    OrangeRed -> RGBColor 0xFF 0x45 0x00
    Orchid -> RGBColor 0xDA 0x70 0xD6
    PaleGoldenrod -> RGBColor 0xEE 0xE8 0xAA
    PaleGreen -> RGBColor 0x98 0xFB 0x98
    PaleTurquoise -> RGBColor 0xAF 0xEE 0xEE
    PaleViolet -> RGBColor 0xDB 0x70 0x93
    PapayaWhip -> RGBColor 0xFF 0xEF 0xD5
    PeachPuff -> RGBColor 0xFF 0xDA 0xB9
    Peru -> RGBColor 0xCD 0x85 0x3F
    Pink -> RGBColor 0xFF 0xC0 0xCB
    Plum -> RGBColor 0xDD 0xA0 0xDD
    Powder -> RGBColor 0xB0 0xE0 0xE6
    Purple -> RGBColor 0xA0 0x20 0xF0
    WebPurple -> RGBColor 0x80 0x00 0x80
    RebeccaPurple -> RGBColor 0x66 0x33 0x99
    Red -> RGBColor 0xFF 0x00 0x00
    RosyBrown -> RGBColor 0xBC 0x8F 0x8F
    RoyalBlue -> RGBColor 0x41 0x69 0xE1
    SaddleBrown -> RGBColor 0x8B 0x45 0x13
    Salmon -> RGBColor 0xFA 0x80 0x72
    SandyBrown -> RGBColor 0xF4 0xA4 0x60
    SeaGreen -> RGBColor 0x2E 0x8B 0x57
    Seashell -> RGBColor 0xFF 0xF5 0xEE
    Sienna -> RGBColor 0xA0 0x52 0x2D
    Silver -> RGBColor 0xC0 0xC0 0xC0
    SkyBlue -> RGBColor 0x87 0xCE 0xEB
    SlateBlue -> RGBColor 0x6A 0x5A 0xCD
    SlateGray -> RGBColor 0x70 0x80 0x90
    Snow -> RGBColor 0xFF 0xFA 0xFA
    SpringGreen -> RGBColor 0x00 0xFF 0x7F
    SteelBlue -> RGBColor 0x46 0x82 0xB4
    Tan -> RGBColor 0xD2 0xB4 0x8C
    Teal -> RGBColor 0x00 0x80 0x80
    Thistle -> RGBColor 0xD8 0xBF 0xD8
    Tomato -> RGBColor 0xFF 0x63 0x47
    Turquoise -> RGBColor 0x40 0xE0 0xD0
    Violet -> RGBColor 0xEE 0x82 0xEE
    Wheat -> RGBColor 0xF5 0xDE 0xB3
    White -> RGBColor 0xFF 0xFF 0xFF
    WhiteSmoke -> RGBColor 0xF5 0xF5 0xF5
    Yellow -> RGBColor 0xFF 0xFF 0x00
    YellowGreen -> RGBColor 0x9A 0xCD 0x32
