- Implement labels
- Implement fill symbols
- Implement intructions can have symbol or number operands
- Implement 2 data commands: word & move; with fill symbols
- WRD pseudo instruction (with symbol or number)
- Move instructions out of program-section since they cross package boundaries
    - they know about symbol tables & binary generation; they are created by the parser
- Move Number4,8,16 classes from asm.parser.number package up to asm.number package
    - They are used everywhere
- pseudo instructions
- program_section is mandatory
- Make symbols_section, video_section, & data_section optional
- Update spec to have optional sections
- symbols section
    - disallow defining predefined symbols (later pass)
- Update spec to be more precise (esp when symbols can and can't be used)
- Instructions & pseudo instructions can be written in upper or lower case? (update spec)
- rest of data section
- video rom section


- 3 passes
    - 1) parse string into domain objects. Outputs for each section:
        - Symbols:      Seq of symbol entries
        - Program ROM:  Seq of program.Command
        - Video ROM:    (Option[Seq[NamedColor]], Option[Seq[Bytes]])
        - Data RAM:     Seq[TaggedCommand]
    - 2) Computer addresses & fill out symbol table
        - symbol entries + pre-defined symbols
          ===[create table]===>
          symbol_table1
        - Seq[Either[Instruction, label]] + symbol_table1
          ===[Enter labels into symbol table]===>
          Seq[Instruction], symbol_table2
        - Option[Seq[NamedColor]] + symbol_table2
          ===[Enter color names into symbol table]===>
          Option[Seq[Color]], symbol_table3
        - Seq[TaggedCommand]
          ===[enter tags into symbol_table]===>
          Seq[Command], symbol_table4
    - 3) Fill in symbols & generate binary
