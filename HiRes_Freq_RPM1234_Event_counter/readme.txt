This is another improvement of my previously modified firmware for the 
"Frequency Counter with a PIC and minimum hardware" created by Wolfgang 
"Wolf" Büscher, DL4YHF http://www.qsl.net/dl4yhf/freq_counter/freq_counter.html

The PIC in question is a PIC16F628A. The kit is widely available on eBay or places like Banggood  

This firmware versionincorporates all changes from the previous versions, plus it now includes a new mode to just 
count events, for example to count turns when winding coils for transformers. If fed with a
1 Khz signal, you can use it as a stopwatch with 1ms resolution.

The current mode is shown at start-up. If you hold the button at startup, it cycles through all modes,
just let go when the desired mode is shown. The new mode is then used as default.

Modes are: 

Freq  = frequency (autoranging)
RP=1  = RPM 1 pulse  per revolution up to 92100 RPM
RP=2  = RPM 2 pulses per revolution up to 92130 RPM
RP=3  = RPM 3 pulses per revolution up to 92140 RPM
RP=4  = RPM 4 pulses per revolution up to 92145 RPM
count = event counting up to 99999

Note: in event counting mode, pressing the button at any time resets the counter back to zero

The latest version are the files with the _V4 suffix which contain the fix for the pulse width problem and a fix in the timing loop plus that version can do over 100MHz in my tests.
