AudioBufferSource abs { url assets/wav/pinknoise.wav, loop true}  [ compressor ] 
DynamicsComressor compressor { threshold
    [ setValueAtTime -1.0  t + 0.1, 
      linearRampToValueAtTime 1.0 t +10 
    ] } [ gain ] 
Gain gain { gain 0.5 } [ output ] 
End