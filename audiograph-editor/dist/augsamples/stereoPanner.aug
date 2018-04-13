AudioBufferSource abs { url assets/wav/pinknoise.wav, loop true}  [ panner ] 
StereoPanner panner { pan 
    [ setValueAtTime -1.0  t + 0.1, 
      linearRampToValueAtTime 1.0 t +10 
    ] } [ gain ] 
Gain gain { gain 0.5 } [ output ] 
End