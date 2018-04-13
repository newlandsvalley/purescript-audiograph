Supported Audio Nodes
=====================

This describes the various audio nodes that are supported together with their attributes and types. See the [web-audio](https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API) specification for more details.

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
| setLoopStart | Number |
| setLoopEnd | Number |

Gain
----

A volume controller


| Attribute | Type |
| ----------- | -- |
| gain | Audio Param |

Delay
-----

A node that introduces a time delay.


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

StereoPanner
------------

A node that pans between the left and right stereo channels.


| Attribute | Type |
| --------- | ---- |
| pan | Audio Param (-1 <= p <= +1)|


DynamicsCompressor
------------------

A node that which lowers the volume of the loudest parts of the signal in order to help prevent clipping and distortion.


| Attribute | Type |
| --------- | ---- |
| threshold | Audio Param (-100 <= p <= 0)|
| knee | Audio Param (0 <= p <= 40)|
| ratio | Audio Param (1 <= p <= 20)|
| attack | Audio Param (0 <= p <= 1)|
| release | Audio Param (0 <= p <= 1)|

