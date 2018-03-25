purescript-audiograph
=====================

The idea is to build a simple proof of concept to provide a declarative interface into web-audio in purescript - something roughly along the lines of [visual-audio-graph](https://github.com/benji6/virtual-audio-graph).

Level of support for Web-Audio
------------------------------

Six nodes are supported at the moment - an implicit __destination__ (output), __Oscillator__, __AudioBufferSource__, __BiquadFilter__, __Delay__ and __Gain__. This includes all the nodes completed in Chris Watersons original [purescript-webaudio](https://github.com/waterson/purescript-webaudio) library together with a couple of extra ones from my port of that library.


Examples
--------

Nodes may be listed in any order, ending with the key word _End_.

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

frequency modulation:

```
  Oscillator modulator { frequency 0.8 } [ gain1 ]
  Oscillator carrier { frequency 300.0 } [ output ]
  Gain gain1 { gain 30.0 } [ carrier.frequency ]
  End
```

Each line defines a new Audio node in the graph starting with the node type and id.  The curly braces define attributes for the node which are either simple scalar values or else complex Audio Params (contained within square braces). The final square braces hold the connections from that node to any other node(s) or to audio parameters on those nodes. An implicit Destination node, named output, is always present by default.

Updates
-------

Updates to a running graph can be defined in a similar manner.  The update graph defines a node (or nodes) with attributes that are to be updated.  The syntax is identical to that for the initial graph definition but without specifiying connections.  For example, you can increase the frequency of the simple oscillator to 880Hz as follows:

simple oscillator update:

```
  Oscillator id2 { frequency 880 }
  End
```

Building
--------

from the current directory:

    $ bower install
    $ ./build.sh

Then host dist/index.html on your web server of choice.

Contributing
------------

The POC seems promising.  Contributions would be more than welcome to help nurture it a little. purescript-webaudio only covers a relatively small proportion of the Web-Audio API and PRs which attempt to extend its reach are not being actively merged. __visual-audio-graph__ has some nice approaches to specifying custom nodes which we could perhaps steal.
