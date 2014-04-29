#version 150
#extension GL_ARB_explicit_attrib_location : enable
#extension GL_ARB_explicit_uniform_location : enable

layout(location = 0) in vec3 in_position ;
layout(location = 2) in vec3 in_range ;

uniform mat4 pjMatrix ;
uniform mat4 mvMatrix ;

uniform float time ;

void main() {
    float tmp = in_position.y + time ;
    float ipart ;
    float fpart = modf( tmp, ipart ) ;
    float newy = in_range.y - (int(ipart) % int(in_range.y) + fpart) + in_range.x ; 
    vec3 newp = vec3( in_position.x, newy, in_position.z ) ;
    gl_Position = pjMatrix * (mvMatrix * vec4(newp,1.0)) ;
}
