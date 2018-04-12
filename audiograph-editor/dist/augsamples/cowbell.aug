Oscillator osc2 { type square, frequency 800 } [ gain1 ]
Oscillator osc1 { type square, frequency 540 } [ gain1 ]
Gain gain1 { gain [ setValue 0.5, setValueAtTime 0.5 t + 0, exponentialRampToValueAtTime 0.01 t + 1.0 ] } [ filter ] 
BiquadFilter filter { type bandpass, frequency 800 } [ output ]
End
