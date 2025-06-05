#version 330 core
layout (location = 0) in vec2 aPos;

// If screen-space coordinates are already calculated on CPU,
// and ortho projection is set up via OpenGL (deprecated but might be in use for 2D)
// then this can be simpler: gl_Position = vec4(aPos.x, aPos.y, 0.0, 1.0);
// However, the ortho matrix is usually passed if using core profile.
uniform mat4 projection;

void main()
{
    // Assuming aPos is in world/model coordinates
    gl_Position = projection * vec4(aPos.x, aPos.y, 0.0, 1.0);
}
