purescript-audiograph
=====================

Highly experimental work in progress.  The idea is to build a simple proof of concept to see whether it is realistic to provide a declarative interface into web-audio in purescript - something roughly along the lines of [visual-audio-graph](https://github.com/benji6/virtual-audio-graph).

Example
-------

Currently, nodes must be listed in reverse order, starting with the destination and moving to the sound sources.  This restriction will be relaxed very soon, allowing them to be listed in any order.

The number of supported nodes is growing - five at the moment - an implicit __destination__ (output), __oscillator__, __audioBufferSource__, __biquadFilter__ and __gain__.  For example - 

simple oscillator:

```   
  Gain id1 { gain 2.0 } [ output ] 
  Oscillator id2 { type square frequency 440 } [ id1 ]
  End
```

cowbell:

```
  BiquadFilter filter1 { type bandpass frequency 800 } [ output ]
  Gain gain1 { gain [ setValue 0.5, setValueAtTime 0.5 0, exponentialRampToValueAtTime 0.01 1.0 ] } [ filter1 ] 
  Oscillator osc1 { type square frequency 540 } [ gain1 ] 
  Oscillator osc2 { type square frequency 800 } [ gain1 ]
  End
```

buffer source which loads its audio buffer from a URL:

```
  Gain id1 { gain 2 } [ output ]
  AudioBufferSource id2 { url wav/techno.wav loop true}  [ id1 ]
  End
```

Each line defines a new Audio node in the graph starting with the node type and id.  The curly braces define attributes for the node which are either simple scalar values or else complex Audio Params (contained within square braces). The final square braces hold the connections from that node to any other node(s). An implicit Destination node, named output, is always present by default. 

Contributing
------------

The POC seems promising.  Contributions would be more than welcome to help nurture it a little.  [purescript-webaudio](https://github.com/waterson/purescript-webaudio) (on which this project is based) only covers a relatively small proportion of the Web-Audio API and PRs which attempt to extend its reach are not being actively merged. And of course, audiograph, at the moment, only uses a trivially small part even of this.  __visual-audio-graph__ has some nice approaches to modifying an existing graph and in specifiying custom nodes which we could perhaps steal.
