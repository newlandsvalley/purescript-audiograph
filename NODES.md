Supported Audio Nodes
=====================

This describes the varous audio nodes that are supported together with their attributes and types.

Oscillator
----------

An oscillator sound source.


| Attribute | Type |
| --------- | ---- |
| frequency | Audio Param |
| detune    | Audio Param |
| type      | sine, square, sawtooth, triangle or custom |

AudioBufferSource
-----------------

A sound source built by loading a sound resource (wav, mp3 etc.) into a buffer.


| Attribute | Type |
| --------- | ---- |
| url | String |
| loop    | Boolean |

Gain
----

A volume controller


| Attribute | Type |
| ----------- | -- |
| gain | Audio Param |

Delay
-----

A note that introduces a time delay.


| Attribute | Type |
| --------- | ---- |
| gain | Audio Param |


BiquadFilter
------------

A node that filters selected frequencies

| Attribute | Type |
| --------- | ---- |
| frequency | Audio Param |
| quality   | Audio Param |
| type      | lowpass, highpass, bandpass, lowshelf, highshelf, peaking, notch or allpass |
