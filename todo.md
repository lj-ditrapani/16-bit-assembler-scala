- main assembler parser
    - dissallow all control chars except '\n'
    - optional .?, negation !()
    - Can flat_map over classes that inject state and return new parsers
      to allow immutable symbol map & line number propagation.
- scene parser (uses bg-cell parser)
- bg-cell parser
