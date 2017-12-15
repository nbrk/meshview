{-# LANGUAGE QuasiQuotes #-}
module Meshview.FragmentShader where

import           Text.RawString.QQ


fragmentShaderSourceString = [r|
#version 130

out vec4 outColor;

uniform vec4 ObjectColor;

void
main()
{
//  outColor = vec4(1.0, 0.0, 0.0, 1.0);
  outColor = ObjectColor;
}
|]

