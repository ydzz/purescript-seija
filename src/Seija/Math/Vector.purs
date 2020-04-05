module Seija.Math.Vector  where

import Data.Typelevel.Num (D2, D3)
import Data.Vec (Vec, vec2, vec3)

type Vector3 a = Vec D3 a

type Vector3f = Vector3 Number

type Vector2 a = Vec D2 a

type Vector2f = Vector2 Number

zeroVec3::Vector3f
zeroVec3 = vec3 0.0 0.0 0.0

oneVec3::Vector3f
oneVec3 = vec3 1.0 1.0 1.0

zeroVec2::Vector2f
zeroVec2 = vec2 0.0 0.0

oneVec2::Vector2f
oneVec2 = vec2 1.0 1.0