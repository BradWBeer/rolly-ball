(let ((vertex-shader-source
       "
#version 330

uniform mat4 P;
uniform mat4 M;

in vec3 v;
in vec3 n;
in vec2 tc;

uniform   sampler2D t1;

out   vec3      oV;
out   vec3      oN;
out   vec2      vTextureCoord;
out   mat3      normalMatrix;

void main() {
            gl_Position = P * M * vec4(v, 1);
            normalMatrix = transpose(inverse(mat3(M)));

            oV = vec3(gl_Position);
            vTextureCoord = tc;
            oN = normalMatrix * n;
}")
      
      (fragment-shader-source
       "
#version 330

uniform   sampler2D t1;

in   vec3      oN;
in   vec2      vTextureCoord;
in   vec3      oV;
//in   mat3      normalMatrix;

layout(location = 1) out vec4 pos;
layout(location = 0) out vec4 fragColor;
layout(location = 2) out vec4 normal;
layout(location = 3) out vec4 tc;

void main() {
             fragColor = texture2D(t1, vTextureCoord);
             //pos = vec4( vec3(1, 1, 1) * (1 -  oV.z), 1);
             pos = vec4( oV, 1);
             normal = normalize(vec4(oN, 1));
             tc = vec4(vTextureCoord, 0, 1);
        }"))


  (make-instance 'clinch:shader 
		 :name "shader"
		 :vertex-shader-text vertex-shader-source
		 :fragment-shader-text fragment-shader-source
		 :attributes '(("tc" :float)
			       ("v"  :float)
			       ("n"  :float))
		 :uniforms   '(("t1" :int)
			       ("P"  :float)
			       ("M"  :float)))))

