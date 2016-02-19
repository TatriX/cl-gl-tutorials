#version 330

out vec4 output_color;

uniform float frag_loop_duration;
uniform float time;

const vec4 first_color = vec4(1.0f, 1.0f, 1.0f, 1.0f);
const vec4 second_color = vec4(0.0f, 1.0f, 0.0f, 1.0f);

void main()
{
        float curr_time = mod(time, frag_loop_duration);
        float curr_lerp = curr_time / frag_loop_duration;

        output_color = mix(first_color, second_color, curr_lerp);
}
