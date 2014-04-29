#version 150
#extension GL_ARB_explicit_attrib_location : enable
#extension GL_ARB_explicit_uniform_location : enable

layout(location = 0) out vec4 frag_color ;

in float rad ;

uniform vec4 globalAmbient ;

void main() {
    frag_color = vec4( 1.0,1.0,1.0,(9 - rad*rad)/9) * vec4(normalize(globalAmbient.xyz),globalAmbient.a) ;
}
