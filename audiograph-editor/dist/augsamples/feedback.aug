AudioBufferSource abs { url assets/ogg/chop.ogg, loop true}  [ delay, output ] 
Delay delay { delayTime 0.2 } [ feedback, output ] 
Gain feedback { gain 0.8 } [ delay ] 
End