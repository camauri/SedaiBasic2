# Rosetta Code — FreeBASIC example programs

These are **third-party** FreeBASIC programs taken **verbatim** from
[Rosetta Code](https://rosettacode.org). They are **not part of SedaiBasic** and are
not compiled into, linked with, or required by it: they are standalone example programs
used to check that SedaiBasic's MODERN (FreeBASIC) dialect runs real-world FB code
correctly. They are included here as a *mere aggregation* (in the GPL FAQ sense) of
separately-licensed works — see the license note below.

## License and attribution

Rosetta Code content is licensed under the
**GNU Free Documentation License, version 1.2 (GFDL 1.2)**:
<https://www.gnu.org/licenses/old-licenses/fdl-1.2.html>
(see also <https://rosettacode.org/wiki/Rosetta_Code:Copyrights>).

Each file below is © its respective Rosetta Code contributors and remains under the
GFDL 1.2. The full history and list of authors for each program is available on its
source page (linked below). The GFDL is **not** the license of SedaiBasic itself
(SedaiBasic is GNU GPL v3); these documents are only aggregated alongside it and keep
their own license. The GFDL 1.2 and the GPL are not compatible for *combining* into a
single work, but distributing separate works together (aggregation) is permitted.

## Sources

| File | Rosetta Code task |
|------|-------------------|
| `pascals_triangle.bas`        | <https://rosettacode.org/wiki/Pascal%27s_triangle> |
| `perfect_numbers.bas`         | <https://rosettacode.org/wiki/Perfect_numbers> |
| `100_doors.bas`               | <https://rosettacode.org/wiki/100_doors> |
| `least_common_multiple.bas`   | <https://rosettacode.org/wiki/Least_common_multiple> |
| `selection_sort.bas`          | <https://rosettacode.org/wiki/Sorting_algorithms/Selection_sort> |
| `gnome_sort.bas`              | <https://rosettacode.org/wiki/Sorting_algorithms/Gnome_sort> |
| `dot_product.bas`             | <https://rosettacode.org/wiki/Dot_product> |
| `insertion_sort.bas`          | <https://rosettacode.org/wiki/Sorting_algorithms/Insertion_sort> |

## Running

```
bin/sb.exe bas/rosetta/perfect_numbers.bas
```

`pascals_triangle.bas` prompts for the number of rows; the sort demos and a few others
end with a "press any key" pause (FreeBASIC convention). All run under the headless
`sb` build.
