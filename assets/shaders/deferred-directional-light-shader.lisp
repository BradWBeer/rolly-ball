(let ((vert-source
	     "
#version 330

uniform mat4 P;
uniform mat4 M;

uniform sampler2D raw;
uniform sampler2D normals;

in vec3 v;
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

uniform sampler2D raw;
uniform sampler2D normals;

uniform vec3 lightDirection;
uniform vec3 lightIntensity;
uniform vec3 lightAmbient;

in vec2 v_tc1;
out vec4 fragColor;
        void main() {

            vec3 dir   = lightDirection;
            float light = max(dot
                              (dir,
                                  vec3(texture2D(normals, v_tc1))), 0);

            fragColor = vec4(texture2D(raw, v_tc1).xyz * ((lightIntensity * light) + lightAmbient), 1);
        }"))
	
	
	(make-instance 'clinch:shader
		       :name "Shader01"
		       :vertex-shader-text vert-source
		       :fragment-shader-text frag-source
		       :uniforms '(("P" :matrix)
				   ("M" :matrix)
				   ("t1" :int)
				   ("raw" :int)
				   ("normals" :int)
				   ("lightDirection" :float)
				   ("lightIntensity" :float)
				   ("lightAmbient" :float))
		       :attributes '(("tc1" :float)
				     ("v" :float)
				     )))




