#version 150
#extension GL_ARB_explicit_attrib_location : enable
#extension GL_ARB_explicit_uniform_location : enable

layout(location = 0) out vec4 frag_color ;
layout(location = 8) uniform vec4 lightPos ;
 uniform sampler2D texture ;

in vec3 normal ;
in vec4 position ;
in vec2 texpos ;

float dX = 1 / 512.0 ;
float dY = 1 / 512.0 ;
vec4 sample(float xc,float yc) {
   return texture2D(texture,texpos + vec2(xc,yc));
}

vec3 calNormChange( vec3 norm, vec3 down, vec3 right ) {
    float x00 = 1 - length(sample(-dX, dY)) ; 
    float x01 = 1 - length(sample(  0, dY)) ; 
    float x02 = 1 - length(sample( dX, dY)) ; 

    float x10 = 1 - length(sample(-dX, 0)) ; 
    float x11 = 1 - length(sample(  0, 0)) ; 
    float x12 = 1 - length(sample( dX, 0)) ; 

    float x20 = 1 - length(sample(-dX,-dY)) ; 
    float x21 = 1 - length(sample(  0,-dY)) ; 
    float x22 = 1 - length(sample( dX,-dY)) ; 

    down = ((x11 - x00) + (x11 - x01) + (x11 - x02) - (x11 - x20) - (x11 - x21) - (x11 - x22)) * down ;
    right = ((x11 - x00) + (x11 - x10) + (x11 - x20) - (x11 - x02) - (x11 - x12) - (x11 - x22)) * right ;
    
    return (right*2 + down*2 + norm) / 5.0 ;
}

void main() {
    vec3 down = vec3( 0, -1, 0 ) ;
    vec3 right = normalize(cross( normal, down )) ;
    down = normalize(cross( normal, right ) );
    vec3 newNorm = calNormChange( normal, down, right ) ;

    float coef = dot( normalize(vec3(lightPos) - vec3(position)), normalize(newNorm) ) ;
    vec4 color = texture2D(texture,texpos) ;
    frag_color = vec4(color.xyz * vec3(0.0,0.4,0.7) * coef,0.8);
}
