purescript-audiograph
=====================


[![Latest release](http://img.shields.io/github/release/newlandsvalley/purescript-audiograph.svg)](https://github.com/newlandsvalley/purescript-audiograph/releases)
[![Build Status](https://travis-ci.org/newlandsvalley/purescript-audiograph.svg?branch=master)](https://travis-ci.org/newlandsvalley/purescript-audiograph)


This is a declarative interface into web-audio in purescript - something roughly along the lines of [visual-audio-graph](https://github.com/benji6/virtual-audio-graph).

Try it out [here](http://www.tradtunedb.org.uk:8602/).

Level of support for Web-Audio
------------------------------

The following nodes are supported at the moment - an implicit __destination__ (output), __Oscillator__, __AudioBufferSource__, __BiquadFilter__, __Delay__, __Gain__, __StereoPanner__, __Panner__, __Convolver__ and __DynamicsCompressor__. This includes all the nodes currently [purescript-webaudio](hhttps://pursuit.purescript.org/packages/purescript-webaudio/0.1.2) other than Analyser and MediaElementSource. Full details are given [here](https://github.com/newlandsvalley/purescript-audiograph/blob/master/NODES.md).


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
  Gain gain1 { gain [ setValue 0.5,
                      setValueAtTime 0.5 t + 0,
                      exponentialRampToValueAtTime 0.01 t + 1.0
                    ] } [ filter1 ]
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

Nodes
-----

Each line defines a new Audio node in the graph starting with the node type and id.  The curly braces define attributes for the node which are either simple scalar values or else complex Audio Params (contained within square braces). The final square braces hold the connections from that node to any other node(s) or to audio parameters on those nodes. An implicit Destination node, named output, is always present by default.

Audio Listener
--------------

An Audio Listener is an optional node with no explicit connections to other nodes in the graph. If present, it must be placed last in the node list, just before the __End__ marker. It does have implicit connections however - it listens to the sound created by the overall graph and tends to be used in conjunction with Panner nodes.

Audio Params
------------

These may be _setValue_, _setValueAtTime_, _setTargetAtTime_, _linearRampToValueAtTime_ or  _exponentialRampToValueAtTime_. The params which mention time use a parameter to represent the time (in seconds) which is either a simple number (absolute javascript time) or which takes the form _t + n_ (time relative to the AudioContext start time).

Updates
-------

Updates to a running graph can be defined in a similar manner.  The update graph defines a node (or nodes) with attributes that are to be updated.  The syntax is identical to that for the initial graph definition but without specifiying connections.  For example, you can increase the frequency of the simple oscillator to 880Hz as follows:

simple oscillator update:

```
  Oscillator id2 { frequency 880 }
  End
```

Building the library
--------------------

from the current directory:

    $ bower install
    $ pulp build

User Interface
--------------

__audiograph_editor__ is an editor (written in Halogen) for audiograph scripts that allows you to 'play' the script after each keystroke (assuming that it still conforms to the DSL).

to build:

cd to audiograph-editor

    $ bower install
    $ npm run build

Then host audiograph-editor/dist/index.html on your web server of choice.    

Building the Simple Examples
----------------------------

cd to simple-examples:

    $ bower install
    $ npm run build

Then host simple-examples/dist/index.html on your web server of choice.

Contributing
------------

Contributions would be more than welcome to help nurture it a little. purescript-webaudio only covers a relatively small proportion of the Web-Audio API at the moment. __visual-audio-graph__ has some nice approaches to specifying custom nodes which we could perhaps steal.

Module documentation
--------------------

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-audiograph).
