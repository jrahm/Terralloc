#version 150
#extension GL_ARB_explicit_attrib_location : enable
#extension GL_ARB_explicit_uniform_location : enable

layout(location = 0) out vec4 frag_color ;

void main() {
    frag_color = vec4(0.0,0.3,0.7,0.5) ;
}
