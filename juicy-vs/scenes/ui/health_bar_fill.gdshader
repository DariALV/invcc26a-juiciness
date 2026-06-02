shader_type canvas_item;

// Rellena horizontalmente hasta 'progress' (0..1) con un borde anti-aliased,
// para que el avance se vea sub-pixel suave (como el filtrado de un sprite) en
// vez de saltar de pixel entero en pixel entero como el relleno del ProgressBar.

uniform float progress : hint_range(0.0, 1.0) = 1.0;
uniform vec4 fill_color : source_color = vec4(1.0, 0.342, 0.286, 1.0);

void fragment() {
	// fwidth(UV.x) ~= ancho de 1 pixel en espacio UV -> borde AA de ~1px.
	float edge = fwidth(UV.x);
	float a = 1.0 - smoothstep(progress - edge, progress + edge, UV.x);
	COLOR = vec4(fill_color.rgb, fill_color.a * a);
}
