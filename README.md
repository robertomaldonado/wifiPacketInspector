This project inspects a wifi packet to get the header field, in which the length is specified.

To perform the packet analysis we use R:
    Rscript PacketInspector.R
run PacketInspector.R to create a middle step file named: "LTSOutput.txt"

This is the chain of convolutional encoding, processed later by python.

"LTSOutput.txt" needs to have all the string values removed, then run python.
The convolutional code is decoded by python running:
    python inspect.py
The final output is the packet size found inside the header of the PLCP.
