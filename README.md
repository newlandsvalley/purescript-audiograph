purescript-audiograph
=====================

Highly experimental work in progress.  The idea is to build a very simple proof of concept to see whether it is realistic to provide a declarative interface into web-audio in purescript - something roughly along the lines of [visual-audio-graph](https://github.com/benji6/virtual-audio-graph).

Example
-------

Nodes are built in reverse order, starting with the destination and moving to the sound sources.  The reason for this is that the audiograph parser enforces a rule that a new node cannot connect to a node that has not yet been defined.  If this proves to be unrealistic (for example if we need feedback loops) then I will relax this condition later on.

At the moment, only three nodes are supported - an implicit __destination__ (output), __oscillator__ and __gain__.  For example:

```   
  Gain id1 { gain 2.0 } [ output ] 
  Oscillator id2 { type square frequency 440 } [ id1 ]
  End
```

or:

```
  Gain id1 { gain [ setValue 0.1, linearRampToValueAtTime 2 2.0 ] } [ output ] 
  Oscillator id2 { type sawtooth frequency 220 } [ id1 ] 
  End
```

Contributing
------------

The POC seems promising.  Contributions would be more than welcome to help nurture it along.  [purescript-webaudio](https://github.com/waterson/purescript-webaudio) (on which this project is based) only covers a relatively small proportion of the Web-Audio API and PRs which attempt to extend its reach are not being actively merged. And of course, audiograph, at the moment, only uses a trivially small part even of this.  __visual-audio-graph__ has some nice approaches to modifying an existing graph and in specifiying custom nodes which we could perhaps steal.
