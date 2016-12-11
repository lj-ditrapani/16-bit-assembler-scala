- symbols section
    - predefined symbols
    - symbols table
    - dissalow defining predefined symbols
    - Entry value:  Not Either[String, Number16]; just Number16?
- main assembler parser
    - Can flatMap over classes that inject state and return new parsers
      to allow immutable symbol map & line number propagation.
