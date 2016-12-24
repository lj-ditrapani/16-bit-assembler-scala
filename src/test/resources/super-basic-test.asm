
# Author:  Lyall Jonathan Di Trapani

# A simple test program
# Another comment

.symbols
.end-symbols
.program-rom
# Adds two number togeth
# RA (register 10) is used for all addresses
# A is stored in ram[$0100]
# B is stored in ram[$0101]
# Add A and B and store in ram[$0102]
# Put A in R1
# Put B in R2
# Add A + B and put in R3
# Store R3 into ram[0102]

# Set RA to $100
HBY $01 10
LBY $00 10

LOD 10 1     # Load value at ram[RA] => R1
LBY $01 10   # Set RA to $101
LOD 10 2     # Load value at ram[RA] => R2
ADD 1 2 3
LBY $02 10   # Set RA to $102
STR 10 3     # Store value in R3 into ram[RA]
END

.end-program-rom
.video-rom
.end-video-rom
.data-ram
.end-data-ram
