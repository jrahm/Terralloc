#version 150
#extension GL_ARB_explicit_attrib_location : enable
#extension GL_ARB_explicit_uniform_location : enable

layout(location = 0) in vec3 in_position ;
layout(location = 2) in vec4 in_color ;
layout(location = 1) in vec3 in_normal ;
layout(location = 3) in vec2 in_texcoord ;

layout(location = 4) uniform mat4 pjMatrix ;
layout(location = 5) uniform mat4 mvMatrix ;
layout(location = 7) uniform mat3 normalMatrix ;

out vec3 lightPos ;
out vec3 normal ;
out vec4 position ;
out vec2 texpos ;

void main() {
    gl_Position = pjMatrix * (position = mvMatrix * vec4( in_position, 1.0 ));
    normal = normalMatrix * vec3(0,1,0) ;
    texpos = in_position.xz / 20.0;
}
