AudioBufferSource abs { url assets/wav/pinknoise.wav, loop true}  [ compressor ] 
DynamicsCompressor compressor { 
  threshold  [ setValueAtTime -50  t + 2  ] , 
  knee  [ setValueAtTime 40  t + 2  ] , 
  ratio  [ setValueAtTime 12  t + 2  ] , 
  attack  [ setValueAtTime 0  t + 2  ] , 
  release  [ setValueAtTime 0.25  t + 2  ] 
} [ gain ] 
Gain gain { gain 0.5 } [ output ] 
End