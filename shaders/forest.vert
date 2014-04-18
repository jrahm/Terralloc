#version 150
#extension GL_ARB_explicit_attrib_location : enable
#extension GL_ARB_explicit_uniform_location : enable

layout(location = 0) in vec3 in_position ;
layout(location = 2) in vec4 in_color ;
layout(location = 1) in vec3 in_normal ;
layout(location = 3) in vec2 in_texcoord ;

layout(location = 4) uniform mat4 pjMatrix ;
layout(location = 5) uniform mat4 mvMatrix ;
layout(location = 8) uniform float time ;
layout(location = 9) uniform mat3 normalMatrix ;

layout(location = 10) in vec3 in_translation ;
layout(location = 11) in vec3 in_scale ;
layout(location = 12) in vec2 in_sincos_rot ;
layout(location = 13) in float noise ;

out vec2 texposition ;
out vec3 normal ;
out vec4 frag_position ;

void main() {
    float s = in_sincos_rot.x ;
    float c = in_sincos_rot.y ;

    mat3 rot = mat3( c,0,s,
                     0,1,0,
                    -s,0,c ) ;
    normal =-rot * normalMatrix * in_normal ;
    texposition = in_texcoord ;

    vec3 real_pos1 = (rot * in_position) * in_scale ;

    float val = sin(( (time+noise) * noise) / 100.0) / 30.0 * pow(real_pos1.y,2) ;
    s = sin( val ) ; c = cos( val ) ;
    rot = mat3( c, -s, 0,
                s,  c, 0,
                0,  0, 1 );

    vec3 real_pos = (rot * real_pos1) + in_translation ;
    gl_Position = pjMatrix * (frag_position = mvMatrix * vec4(real_pos,1.0) );
}
