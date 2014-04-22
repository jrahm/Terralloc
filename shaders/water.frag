#version 150
#extension GL_ARB_explicit_attrib_location : enable
#extension GL_ARB_explicit_uniform_location : enable

layout(location = 0) out vec4 frag_color ;
layout(location = 8) uniform vec4 lightPos ;
layout(location = 9) uniform float time ;
layout(location = 10) uniform vec4 globalAmbient ;
layout(location = 5) uniform mat4 mvMatrix ;

uniform sampler2D texture ;
uniform sampler2D skytex ;
uniform sampler2D skynight ;
uniform vec4 lightpos ;

in vec3 normal ;
in vec4 position ;
in vec2 texpos ;

float dX = 1 / 512.0 ;
float dY = 1 / 512.0 ;
vec4 sample(float xc,float yc) {
   return texture2D(texture,texpos + vec2(xc,yc) - vec2(time/20.0,time/20.0));
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

in vec3 original_x ;
in vec3 original_z ;
in vec3 tmpnormal ;

vec4 specular( vec3 a_normal ) {
    vec3 difpos = -normalize(vec3(0,0,0) - vec3(position)) ;
    difpos = reflect( difpos, a_normal ) ;
    vec3 lightpos2 = reflect( reflect( vec3(lightPos), original_x ), original_z );
    vec3 diflight = normalize(vec3(lightpos2) - vec3(position)) ;
    float d = pow(max(dot( normalize(difpos), -normalize(diflight) ), 0.0 ),100.0);
    return vec4(vec3(d),1) ;
}
void main() {
    vec3 down = vec3( 0, -1, 0 ) ;
    vec3 right = normalize(cross( normal, down )) ;
    down = normalize(cross( normal, right ) );
    vec3 newNorm = calNormChange( normal, down, right ) ;

    vec3 camVector = vec3(position) - vec3(0,0,0);
    vec3 ref = reflect( normalize(camVector), newNorm ) ;

    float tex_x = (abs(dot( ref, original_x )) + 1) / 2;
    float tex_y = (abs(dot( ref, original_z )) + 1) / 2;
    // vec4 refcolor = texture2D(skytex, vec2(tex_x,tex_y));
    vec2 tmpcoord = vec2(tex_x,tex_y) ;
    vec4 refcolor = 
        mix(texture2D(skynight,tmpcoord) * (1-globalAmbient.a),
            texture2D(skytex,tmpcoord) * vec4(normalize(globalAmbient.xyz),1),
            (globalAmbient.a + 1) / 2) ;
    vec3 nlightpos = reflect( vec3(lightPos), vec3(mvMatrix * vec4(0,1,0,1)) ) ;
    refcolor += specular( newNorm ) ;
    float coef = dot( normalize(vec3(lightPos) - vec3(position)), normalize(normal) ) * 0.5 + 0.5 ;


 //   frag_color = vec4( 0,0,1, 1.0 );
    // frag_color = vec4(tex_x,tex_y,0,1.0) ;
    // vec4 color = sample(0,0);
    frag_color = vec4(vec3(refcolor) * vec3(0.6,0.8,1.0),0.95) * vec4(normalize(globalAmbient.xyz),1.0);
//    frag_color = vec4(0,0,1,0.8) ;
}
