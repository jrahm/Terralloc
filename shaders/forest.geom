#version 150
#extension GL_ARB_explicit_attrib_location : enable
#extension GL_ARB_explicit_uniform_location : enable

layout(points) in ;
layout(triangle_strip, max_vertices=82) out;

layout(location = 4) uniform mat4 pjMatrix ;
layout(location = 5) uniform mat4 mvMatrix ;

out vec2 texposition;
out vec3 normal ;
out vec4 frag_position ;

void vertex( vec4 pos ) {
    normal = - inverse(transpose(mat3(mvMatrix))) * vec3( pos.x, 0, pos.z ) ;
    gl_Position = pjMatrix * (frag_position = gl_in[0].gl_Position + (mvMatrix * pos)) ;
    EmitVertex() ;
}

void main() {
    float r = 0.045 ;
    float th = 0.00 ;
    float h = 0.4 ;
    for( ; th < 6.4 ; th += 1.0 ) {
        float c = r * cos( th ) ;
        float s = r * sin( th ) ;
        float c2 = r * cos( th + 1.0 ) ;
        float s2 = r * sin( th + 1.0 ) ;

        float tex_x = th / 6.4 / 2.0;
        float tex_x2 = (th+1.0) / 6.4 / 2.0 ;
        texposition = vec2(tex_x, 0);
        vertex( vec4(c, 0.0, s, 0) ) ;
        texposition = vec2(tex_x, 1);
        vertex( vec4(c, h, s, 0) ) ;
        texposition = vec2(tex_x2, 0);
        vertex( vec4(c2, h, s2, 0) ) ;

        texposition = vec2(tex_x, 0);
        vertex( vec4(c, 0.0, s, 0) ) ;
        texposition = vec2(tex_x2, 0);
        vertex( vec4(c2, 0, s2, 0) ) ;
        texposition = vec2(tex_x2, 1);
        vertex( vec4(c2, h, s2, 0) ) ;
    }

    for( th = 0; th < 6.4 ; th += 1.0 ) {
        float c = (r*4) * cos( th ) ;
        float s = (r*4) * sin( th ) ;
        float tex_x = th / 6.4 / 2.0 + 0.5;
        texposition = vec2(tex_x, 1);
        vertex( vec4(0,h*2,0,0) ) ;
        texposition = vec2(tex_x, 0);
        vertex( vec4(s,h/2,c,0) ) ;
    }

    for( th = 0; th < 6.4 ; th += 1.0 ) {
        float c = (r*6) * cos( th ) ;
        float s = (r*6) * sin( th ) ;
        float tex_x = th / 6.4 / 2.0 + 0.5;
        texposition = vec2(tex_x, 1);
        vertex( vec4(0,h,0,0) ) ;
        texposition = vec2(tex_x, 0);
        vertex( vec4(s,h/4,c,0) ) ;
    }

    EndPrimitive();
}
