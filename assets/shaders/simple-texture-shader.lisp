(let ((vert-source
	     "
#version 330

//layout (location = 0) out vec4 colorOut;
//layout (location = 1) out vec4 texCoordOut;
//layout (location = 0) out vec4 normalOut;
//layout (location = 1) out vec4 texCoordOut;

uniform mat4 P;
uniform mat4 M;

uniform sampler2D t1;
in vec3 v;
//in vec3 n;
in vec2 tc1;
out vec2 v_tc1;
        void main() {
            gl_Position = P * M * vec4(v, 1);
            v_tc1 = vec2(tc1.x, -tc1.y);
        }")
	    
	    ;; String for the Fragment Shader
	    ;;   t1    is the texture sampler
	    ;;   v_tc1 is the texture coordinates from the fragment shader
	    (frag-source
	     "
#version 330
uniform sampler2D t1;
in vec2 v_tc1;
out vec4 fragColor;
//layout (location = 0) out vec4 colorOut;

        void main() {
            fragColor = texture2D(t1, v_tc1);
        }"))
	
	
	(make-instance 'clinch:shader
		       :name "Shader01"
		       :vertex-shader-text vert-source
		       :fragment-shader-text frag-source
		       :uniforms '(("P" :matrix)
				   ("M" :matrix)
				   ("t1" :int))
		       :attributes '(("tc1" :float)
				     ("v" :float)
				     )))





