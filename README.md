# tichy\_oe\_generator

Old English (Anglo-Saxon) morphological generator based on the work of Ondřej Tichý's thesis, [Morphological Analyser of Old English](https://www.researchgate.net/publication/318926182_Morphological_analyser_of_old_english) (2017).

The upstream code and data, (c) Ondřej Tichý, is released under the [CC BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0/) license. Changes made in this repository by Madeleine Thompson are released to the public domain.

To run the generator and output the TSV file `output.txt` with one form per line, run:
```
perl create_dict31.pl
```
Pass the `--help` flag for usage.

This repository mainly exists to generate source data for the [Bosworth–Toller/Charles Anglo-Saxon Dictionary](https://play.google.com/store/apps/details?id=net.mdln.englisc) Android app.
