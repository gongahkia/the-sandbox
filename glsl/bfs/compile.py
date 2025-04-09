# ----- required imports -----

import pygame
import shaderc
from OpenGL.GL import *

# ----- helper functions -----

def compile_shader_from_file(file_path, shader_type):
    """
    read respective shader source code from file and compile it
    """
    with open(file_path, 'r') as f:
        source_code = f.read()
    compiler = shaderc.Compiler()
    result = compiler.compileGlslToSpv(source_code, shader_type)
    if result.get_compilation_status() != shaderc.compilation_status.success:
        print(f"Compilation failed for {file_path}: ", result.get_error_message())
        return None
    return result.get_bytes()

def compile_shader(source, shader_type):
    """
    compile the shader source code
    """
    shader = glCreateShader(shader_type)
    glShaderSource(shader, source)
    glCompileShader(shader)
    success = glGetShaderiv(shader, GL_COMPILE_STATUS)
    if not success:
        info_log = glGetShaderInfoLog(shader)
        print(f"Error compiling {shader_type}: {info_log.decode()}")
        glDeleteShader(shader)  
        return None
    return shader

def create_shader_program(vertex_shader_source, fragment_shader_source):
    """
    create and link the shader program
    """
    vertex_shader = compile_shader(vertex_shader_source, GL_VERTEX_SHADER)
    fragment_shader = compile_shader(fragment_shader_source, GL_FRAGMENT_SHADER)
    if not vertex_shader or not fragment_shader:
        print("Failed to create shaders.")
        return None
    shader_program = glCreateProgram()
    glAttachShader(shader_program, vertex_shader)
    glAttachShader(shader_program, fragment_shader)
    glLinkProgram(shader_program)
    glDeleteShader(vertex_shader)
    glDeleteShader(fragment_shader)
    return shader_program

# ----- sample execution code -----

if __name__ == "__main__":
    pygame.init()
    pygame.display.set_mode((800, 600), pygame.OPENGL | pygame.DOUBLEBUF)
    vertex_shader_source = compile_shader_from_file("shader.vert", shaderc.shader_kind.vertex)
    fragment_shader_source = compile_shader_from_file("shader.frag", shaderc.shader_kind.fragment)
    shader_program = create_shader_program(vertex_shader_source, fragment_shader_source)
    if not shader_program:
        print("Failed to create the shader program.")
        pygame.quit()
        exit()
    running = True
    while running:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
        glUseProgram(shader_program)
        pygame.display.flip()
    glDeleteProgram(shader_program)
    pygame.quit()