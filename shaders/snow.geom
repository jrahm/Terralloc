#version 150
layout(points) in;
layout(triangle_strip, max_vertices=28) out;

out float rad ;

void vertex( vec3 pos ) {
    gl_Position = gl_in[0].gl_Position + vec4(pos,0.0) ;
    EmitVertex() ;
}

void main( ) {
    float r = 0.008 ;
    float th = 0.00 ;
    for( ; th < 6.3 ; th += 0.5 ) {
        rad = 3 ;
        vertex( vec3(r*sin(th),r*cos(th),0.0) ) ;
        rad = 0.0 ;
        vertex( vec3(0.0,0.0,0.0) ) ;
    }
    th = 0 ;
    rad = 3 ;
    vertex( vec3(r*sin(th),r*cos(th),0.0) ) ;
    // vertex( vector[0] ) ;
    EndPrimitive();
}
