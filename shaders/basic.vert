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

out vec2 texcoord ;
out vec4 position ;
out vec3 normal ;

out float texture_blend[8] ;
flat out int texidx ;

void main() {
    gl_Position = pjMatrix * (position = mvMatrix * vec4(in_position,1.0)) ;
    texcoord = in_texcoord ;
    normal = normalMatrix * in_normal ;
    for ( int i = 0 ; i < 8 ; ++ i ) 
        texture_blend[i] = 0 ;
    texidx = int(clamp(round(in_color.a),0,8));
    texture_blend[texidx] = 1.0 ;
}
