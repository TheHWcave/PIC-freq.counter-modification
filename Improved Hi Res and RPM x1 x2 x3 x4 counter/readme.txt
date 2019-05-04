This is an improvement of my previously modified firmware for the 
"Frequency Counter with a PIC and minimum hardware" created by Wolfgang 
"Wolf" Büscher, DL4YHF http://www.qsl.net/dl4yhf/freq_counter/freq_counter.html

The most significant change is that the update rate is now much faster than before.
The frequency display in period measuring mode is not 200.00 Hz from 100.00 Hz before

This version allows the selection of 4 different RPM modes to allow for 1, 2, 3, or 4 
pulses per revolution, thus allowing measuring 2-bladed, 3-bladed, 4-bladed fans or props.

The display shows the selected mode for 0.5 seconds after power up. 
If the button is pressed during that time, the display will continously cycle through
the modes, displaying each for 0.5 seconds. If the button is released during that 
time, the last displayed mode is stored in EEPROM and used as default at the 
next power-up. 

The effect of the selection for a pulse frequency 100 Hz is

Mode: | Display:
------|-----------
Freq :| 100.00    (Hz)  
RP 1 :| 6000      (RPM)
RP 2 :| 3000      (RPM)
RP 3 :| 2000      (RPM)
RP 4 :| 1500	  (RPM)

The second modification is that the range for RPM is now extended. Up to 255 Hz, RPM (1, 2, 3 or 4) 
use period measuring as before but for frequencies > 255 Hz they use the frequency as basis
for the conversion instead. 

The ranges are now:

Mode   |input pulses/s (Hz) |  display  
RP 1:  | 1 .. 1535          | 60 .. 92100 RPM
RP 2:  | 1 .. 3071          | 30 .. 92130 RPM
RP 3:  | 1 .. 4607          | 20 .. 92140 RPM
RP 4:  | 1 .. 6143	    | 15 .. 92145 RPM

