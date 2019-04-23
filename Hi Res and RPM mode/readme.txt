This is a further modification of my previously modified firmware for the 
"Frequency Counter with a PIC and minimum hardware" created by Wolfgang 
"Wolf" Büscher, DL4YHF http://www.qsl.net/dl4yhf/freq_counter/freq_counter.html

This version removes the programming and frequency-offset capabilities but adds
a hi-resolution mode in the frequency range of 2 to 100 Hz and can be switched 
to show RPM instead of frequency in the range of 120 to 15300 RPM 

To toggle between frequency or RPM mode, hold the button during power-up. The counter 
remembers the last setting in its EEPROM. 

Note that there are no more build options: It assumes a 20 MHz clock frequency and a 
5 digit display using common cathode (this variant was previously known as DISPLAY_VARIANT_2)

In frequency mode, for frequencies <= 100 Hz the display has now 2 digits behind the decimal
point, i.e.  100Hz is shown as 100.00  12.34Hz is shown as 012.34 

In RPM mode, the display shows full RPM, i.e. 100 Hz is shown as 6000 RPM 
and 12.34 Hz as 740 RPM
