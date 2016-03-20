#version 330

layout(location = 0) in vec4 position;
layout(location = 1) in vec4 color;

smooth out vec4 frag_color;

uniform mat4 camera_to_clip_matrix;
uniform mat4 model_to_camera_matrix;

void main()
{
        vec4 camera_pos = model_to_camera_matrix * position;
        gl_Position = camera_to_clip_matrix * camera_pos;

        frag_color = color;
}
