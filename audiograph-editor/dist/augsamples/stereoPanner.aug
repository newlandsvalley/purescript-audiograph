AudioBufferSource abs { url assets/wav/pinknoise.wav, loop true}  [ panner ]
StereoPanner panner { pan -0.2 } [ gain ]
Gain gain { gain 0.8 } [ output ] 
End