{-# LANGUAGE QuasiQuotes #-}
module Meshview.VertexShader where

import           Text.RawString.QQ

vertexShaderSourceString = [r|
#version 130

in vec3 Position;

uniform mat4 ModelMatrix;
uniform mat4 ViewMatrix;
uniform mat4 ProjectionMatrix;

void
main()
{
  gl_Position = ProjectionMatrix * ViewMatrix * ModelMatrix * vec4(Position, 1);
//  gl_Position = vec4(Position, 1);
}
|]
