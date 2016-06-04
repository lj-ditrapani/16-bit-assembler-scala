Usage
-----

    java -jar asm16 [-t | --tiles] file

If only a file is provided as a command line argument, assemble the file into
a binary executable to run on the [ljd 16-bit computer][ljd 16-bit computer].
The assembly language is defined [here][asmlang].

The program can also be used to generate [binary tile sets][video] from
[text tile format][text tile format] files.

    -t --tiles  Parse the text tile format file and produce a binary format
                tile file.


Examples
--------

    java -jar asm16 file.asm > bin.file

    java -jar asm16 -t main.tiles > tiles.bin

    java -jar asm16 --tiles main.tiles > tiles.bin



[ljd 16-bit computer]: https://github.com/lj-ditrapani/16-bit-computer-specification
[asmlang]: https://github.com/lj-ditrapani/16-bit-computer-specification/blob/master/assembler/assembly-language.md
[video]: https://github.com/lj-ditrapani/16-bit-computer-specification/blob/master/video.md
[text tile format]: https://github.com/lj-ditrapani/16-bit-computer-specification/blob/master/assembler/tile-file-format.md
