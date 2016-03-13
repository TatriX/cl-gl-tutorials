#version 330

layout(location = 0) in vec4 position;
uniform float loop_duration;
uniform float time;

void main()
{
        float time_scale = 3.14159f * 2.0f / loop_duration;
        float curr_time = mod(time, loop_duration);
        vec4 total_offset = vec4(cos(curr_time * time_scale) * 0.5f,
                                sin(curr_time * time_scale) * 0.5f,
                                0.0f,
                                0.0f);
	gl_Position = position + total_offset;
}
