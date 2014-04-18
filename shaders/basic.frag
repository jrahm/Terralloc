#version 150
#extension GL_ARB_explicit_attrib_location : enable
#extension GL_ARB_explicit_uniform_location : enable

layout(location = 0) out vec4 frag_color ;
layout(location = 6) uniform vec4 lightPos ;

layout(location = 8) uniform vec4 globalAmbient ;
uniform float dX ;
uniform float dY ;

uniform mat4 mvMatrix ;
uniform mat4 pjMatrix ;

in vec3 normal ;

uniform sampler2D textures[8] ;

in float texture_blend[8] ;
in vec2 texcoord ;
in vec4 position ;

vec3 sample(float xc,float yc) {
    vec3 color = vec3(0);
    for ( int i = 0 ; i < 8 ; ++ i ) {
        vec4 tmp = texture2D(textures[i], texcoord + vec2(xc,yc)) ;
        color += vec3(tmp) * texture_blend[i] ;
    }
    return color ;
}

vec3 sample2(int tex, float xc,float yc) {
    vec3 color = vec3(0);
    vec4 tmp = texture2D(textures[tex], texcoord + vec2(xc,yc)) ;
    color += vec3(tmp) ;
    return color ;
}

int dominentTexture() {
    float m = 0.0 ;
    int ret = 0;
    for( int i = 0 ; i < 8 ; ++ i ) {
        if( texture_blend [i] > m ) {
            m = texture_blend[i] ;
            ret = i ;
        }
    }
    return ret ;
}

vec3 calNormChange( vec3 norm, vec3 down, vec3 right ) {
    int dom = dominentTexture() ;
    float x00 = length(sample2(dom,-dX, dY)); 
    float x01 = length(sample2(dom,  0, dY)); 
    float x02 = length(sample2(dom, dX, dY)); 

    float x10 = length(sample2(dom,-dX, 0)); 
    float x11 = length(sample2(dom,  0, 0)); 
    float x12 = length(sample2(dom, dX, 0)); 

    float x20 = length(sample2(dom,-dX,-dY)); 
    float x21 = length(sample2(dom,  0,-dY)); 
    float x22 = length(sample2(dom, dX,-dY)); 

    down = ((x11 - x00) + (x11 - x01) + (x11 - x02) - (x11 - x20) - (x11 - x21) - (x11 - x22)) * down ;
    right = ((x11 - x00) + (x11 - x10) + (x11 - x20) - (x11 - x02) - (x11 - x12) - (x11 - x22)) * right ;
    
    return (norm + down + right) / 3.0 ;
}

void main() {
    vec3 down = vec3( 0.0, -1.0, 0.0 ) ;
    vec3 right = normalize(cross( normal, down )) ;
    down = normalize(cross( normal, right ) );
    vec3 newNorm = calNormChange(normal,down,right) ;

    vec3 color = sample(0,0) ;
    
    float prod = dot( normalize(-newNorm), normalize(vec3(lightPos - position)));
    vec3 intensity = vec3(prod,prod,max(prod,0.4)) ;

    frag_color = vec4(color * intensity,1) * vec4(normalize(globalAmbient.xyz),1.0);
}
