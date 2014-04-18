#version 150
#extension GL_ARB_explicit_attrib_location : enable
#extension GL_ARB_explicit_uniform_location : enable

layout(location = 0) in vec3 in_position ;
layout(location = 2) in vec4 in_color ;
layout(location = 1) in vec3 in_normal ;
layout(location = 3) in vec2 in_texcoord ;

uniform mat4 mvMatrix ;
uniform mat4 pjMatrix ;

out vec2 texcoord ;

void main() {
    gl_Position = pjMatrix * mvMatrix * vec4(in_position,1.0);
    texcoord = in_texcoord ;
}
