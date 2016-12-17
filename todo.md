- 3 passes
    - 1) parse string into domain objects. Outputs for each section:
        - Symbols:      Seq of symbol entries
        - Program ROM:  Seq of Either[Instruction, Label]
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
- 3 packages (match up with passes)
    - 1) parser (converts text to domain objects)
    - 2) symbol table
    - 3) binary generator
- symbols section
    - symbols table
    - dissalow defining predefined symbols (later pass)
- main assembler parser
    - Can flatMap over classes that inject state and return new parsers
      to allow immutable symbol map & line number propagation.
