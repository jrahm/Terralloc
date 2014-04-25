#version 150
#extension GL_ARB_explicit_attrib_location : enable
#extension GL_ARB_explicit_uniform_location : enable

layout(location = 0) out vec4 frag_color ;
layout(location = 1) uniform vec4 globalAmbient ;

uniform vec4 lightpos ;

uniform sampler2D texture ;
uniform sampler2D night_tex ;

in vec2 texcoord;
in vec4 position ;

uniform float mult ;

float exp1( float x ) {
    return 2 / (1+exp(-x))-1;
}
void main() {  
    // the sun
    
    vec3 lighttofrag = vec3(position*10000000 - lightpos) ;
    vec3 lighttocamera = vec3(lightpos) ;
    float coef = 1 - dot(normalize(lighttocamera), normalize(lighttofrag));
    vec4 mul = vec4(vec3( pow(0.0005 / coef,0.8)), 1) ;
    mul *= vec4(1.0,0.85,0.5,1.0) ;
    frag_color = 
        mix(texture2D(night_tex,texcoord) * (1-globalAmbient.a),
            texture2D(texture,texcoord) * vec4(normalize(globalAmbient.xyz),1),
            (globalAmbient.a + 1) / 2) * 1.8 ;
    frag_color = (frag_color + mul)*mult ;

}
