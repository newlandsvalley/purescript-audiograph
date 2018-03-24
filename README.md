purescript-audiograph
=====================

The idea is to build a simple proof of concept to provide a declarative interface into web-audio in purescript - something roughly along the lines of [visual-audio-graph](https://github.com/benji6/virtual-audio-graph).

Example
-------

Nodes may be listed in any order, ending with the key word _End_. The number of supported nodes is growing - six at the moment - an implicit __destination__ (output), __Oscillator__, __AudioBufferSource__, __BiquadFilter__, __Delay__ and __Gain__.  For example - 

simple oscillator:

```   
  Gain id1 { gain 2.0 } [ output ] 
  Oscillator id2 { type square, frequency 440 } [ id1 ]
  End
```

cowbell:

```
  Oscillator osc2 { type square, frequency 800 } [ gain1 ]
  Oscillator osc1 { type square, frequency 540 } [ gain1 ] 
  Gain gain1 { gain [ setValue 0.5, setValueAtTime 0.5 0, exponentialRampToValueAtTime 0.01 1.0 ] } [ filter1 ] 
  BiquadFilter filter1 { type bandpass, frequency 800 } [ output ]
  End
```

buffer source which loads its audio buffer from a URL:

```
  AudioBufferSource id2 { url wav/techno.wav, loop true}  [ id1 ]
  Gain id1 { gain 2 } [ output ]
  End,
```

feedback:

```
  AudioBufferSource abs { url ogg/chop.ogg, loop true}  [ delay, output ]
  Delay delay { delayTime 0.5 } [ feedback, output ]
  Gain feedback { gain 0.8 } [ delay ]
  End
```

Each line defines a new Audio node in the graph starting with the node type and id.  The curly braces define attributes for the node which are either simple scalar values or else complex Audio Params (contained within square braces). The final square braces hold the connections from that node to any other node(s). An implicit Destination node, named output, is always present by default. 

Building
--------

from the current directory:

    $ bower install
    $ ./build.sh

Then host dist/index.html on your web server of choice.

Contributing
------------

The POC seems promising.  Contributions would be more than welcome to help nurture it a little.  [purescript-webaudio](https://github.com/waterson/purescript-webaudio) (on which this project is based) only covers a relatively small proportion of the Web-Audio API and PRs which attempt to extend its reach are not being actively merged. And of course, audiograph, at the moment, only uses a trivially small part even of this.  __visual-audio-graph__ has some nice approaches to modifying an existing graph and in specifiying custom nodes which we could perhaps steal.
