#version 150
#extension GL_ARB_explicit_attrib_location : enable
#extension GL_ARB_explicit_uniform_location : enable

layout(location = 0) out vec4 frag_color ;
layout(location = 1) uniform vec4 globalAmbient ;

uniform sampler2D texture ;
uniform sampler2D night_tex ;
in vec2 texcoord;

void main() {
    vec3 color2 = texture2D(texture,texcoord).xyz ;
    frag_color = 
        mix(texture2D(night_tex,texcoord) * (1-globalAmbient.a),
            texture2D(texture,texcoord) * vec4(normalize(globalAmbient.xyz),1),
            (globalAmbient.a + 1) / 2) ;
}
