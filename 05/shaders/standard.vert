#version 330

layout(location = 0) in vec4 position;
layout(location = 1) in vec4 color;

smooth out vec4 frag_color;

uniform vec3 offset;
uniform mat4 perspective_matrix;

void main()
{
        vec4 camera_pos = position + vec4(offset.x, offset.y, offset.z, 0.0);

	gl_Position = perspective_matrix * camera_pos;
        frag_color = color;
}
