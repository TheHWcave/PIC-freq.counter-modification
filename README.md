# PIC-freq.counter-modification
firmware change to improve resolution of a popular "1Hz-50MHz Digital LED DIY Kits Crystal Oscillator Frequency Counter Tester" sold on Ebay and other sites

This is a modification of the original firmware for the "Frequency Counter with
a PIC and minimum hardware" created by Wolfgang "Wolf" Büscher, DL4YHF
http://www.qsl.net/dl4yhf/freq_counter/freq_counter.html

Derivatives of Wolf's design are sold on Ebay and other sides in kit form, 
usually adding a crystal test circuit but otherwise using Wolf's exact design and 
firmware for the counter part (Wolf is aware of these "clones" but doesn't mind
as long as "those kits are offered for a fair price"). 

Wolf's firmware is writen in assembler and has 3 options, one of which you must select 
when building it. The clone kit I have needs to be built with option DISPLAY_VARIANT_2 
set (Common Cathode). I presume that is true for most if not all clones out there, but 
it might be good to check your circuit diagram before replacing the program that was 
loaded in the PIC. 

The software is based on the latest version from Wolf's website and contains the
following modifications (if you set DISPLAY_VARIANT_2 or DISPLAY_VARIANT_3) that are all
clearly marked in the comments.

1. The range#2 from 11520 Hz to 105600 Hz is now using 1 second gate time (same as 
   the range#1 (1..11519 Hz) and therefor the counter now displays frequencies from 
   1 to 99999 in 1Hz resolution on the 5 digit display. Range#2 in the original version 
   uses 0.5s gate time and multiply the result by 2 before displaying which leads to 
   2Hz dsiplay resolution.

2. The underflow and overflow display now uses the 5th digit instead of the 4th. 

3. I changed the formatting of the display so that the range from 0..99999 Hz is always 
   using KHz format and leading zeros if necessary. Examples below:
    Freq          old       new
    <= 0Hz      "___0_"     "____0"
       1Hz      "___1_"     "00001"
    1234Hz      "1234_"     "01234"
   12000Hz      "12000"     "12000"   

Other than these changes, the software and the comments are unchanged from Wolf's version
The hex file is for DISPLAY_VARIANT_2 and can be directly loaded into the PIC

NOTE:
The latest firmware is now available in HiRes_Freq_RPM1234_Event_counter
