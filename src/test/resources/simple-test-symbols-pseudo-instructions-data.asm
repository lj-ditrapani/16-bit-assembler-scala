
# Author:  Lyall Jonathan Di Trapani

# A simple test program with
#   symbols (user & pre-defined),
#   pseudo-instructions, &
#   data entries

.symbols
    offset 12
.end-symbols
.program-rom
    WRD val1 R1
    WRD val2 R2
    WRD out_addr RF
    ADD R1 R2 R3
    ADI R3 offset R3     # add x to R3
    STR RF R3
.end-program-rom
.video-rom
.end-video-rom
.data-ram
    word val1 3
    word val2 5
    word out_addr 0
.end-data-ram
