#version 150
#extension GL_ARB_explicit_attrib_location : enable
#extension GL_ARB_explicit_uniform_location : enable

layout(location = 0) out vec4 frag_color ;

uniform mat4 mvMatrix ;
uniform mat4 pjMatrix ;
uniform sampler2D texture ;
uniform vec4 light ;
uniform vec4 globalAmbient ;

uniform float dX ;
uniform float dY ;

in vec2 texposition ;
in vec3 normal ;
in vec4 frag_position ;
in float shade ;

vec4 sample(float xc,float yc) {
   return texture2D(texture,texposition + vec2(xc,yc));
}

vec3 calNormChange( vec3 norm, vec3 down, vec3 right ) {
    float x00 = 1 - sample(-dX, dY).a ; 
    float x01 = 1 - sample(  0, dY).a ; 
    float x02 = 1 - sample( dX, dY).a ; 

    float x10 = 1 - sample(-dX, 0).a ; 
    float x11 = 1 - sample(  0, 0).a ; 
    float x12 = 1 - sample( dX, 0).a ; 

    float x20 = 1 - sample(-dX,-dY).a ; 
    float x21 = 1 - sample(  0,-dY).a ; 
    float x22 = 1 - sample( dX,-dY).a ; 

    down = ((x11 - x00) + (x11 - x01) + (x11 - x02) - (x11 - x20) - (x11 - x21) - (x11 - x22)) * down ;
    right = ((x11 - x00) + (x11 - x10) + (x11 - x20) - (x11 - x02) - (x11 - x12) - (x11 - x22)) * right ;
    
    return (right*2 + down*2 + norm) / 5.0 ;
}

void main() {
    vec3 down = vec3( 0, -1, 0 ) ;
    vec3 right = normalize(cross( normal, down )) ;
    down = normalize(cross( normal, right ) );
    vec3 newNorm = calNormChange( normal, down, right ) ;

    vec4 col = texture2D(texture,texposition) ;
    float coef = max(dot( normalize(newNorm),
                          normalize(vec3(frag_position - light)) ),0) + (globalAmbient.a/4.0) ;
    frag_color = vec4( shade * col.xyz * coef * globalAmbient.xyz, col.a);
}
