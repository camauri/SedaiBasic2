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
| `abc_problem.bas`             | <https://rosettacode.org/wiki/ABC_problem> |
| `amb.bas`                     | <https://rosettacode.org/wiki/Amb> |
| `array_concatenation.bas`     | <https://rosettacode.org/wiki/Array_concatenation> |
| `bell_numbers.bas`            | <https://rosettacode.org/wiki/Bell_numbers> |
| `caesar_cipher.bas`           | <https://rosettacode.org/wiki/Caesar_cipher> |
| `cholesky_decomposition.bas`  | <https://rosettacode.org/wiki/Cholesky_decomposition> |
| `counting_sort.bas`           | <https://rosettacode.org/wiki/Sorting_algorithms/Counting_sort> |
| `flatten_a_list.bas`          | <https://rosettacode.org/wiki/Flatten_a_list> |
| `four_bit_adder.bas`          | <https://rosettacode.org/wiki/Four_bit_adder> |
| `gapful_numbers.bas`          | <https://rosettacode.org/wiki/Gapful_numbers> |
| `gray_code.bas`               | <https://rosettacode.org/wiki/Gray_code> |
| `hailstone_sequence.bas`      | <https://rosettacode.org/wiki/Hailstone_sequence> |
| `hofstadter_q_sequence.bas`   | <https://rosettacode.org/wiki/Hofstadter_Q_sequence> |
| `iterated_digits_squaring.bas`| <https://rosettacode.org/wiki/Iterated_digits_squaring> |
| `jaro_similarity.bas`         | <https://rosettacode.org/wiki/Jaro_similarity> |
| `kronecker_product.bas`       | <https://rosettacode.org/wiki/Kronecker_product> |
| `levenshtein_distance.bas`    | <https://rosettacode.org/wiki/Levenshtein_distance> |
| `map_range.bas`               | <https://rosettacode.org/wiki/Map_range> |
| `matrix_multiplication.bas`   | <https://rosettacode.org/wiki/Matrix_multiplication> |
| `matrix_transposition.bas`    | <https://rosettacode.org/wiki/Matrix_transposition> |
| `pascal_matrix_generation.bas`| <https://rosettacode.org/wiki/Pascal_matrix_generation> |
| `pernicious_numbers.bas`      | <https://rosettacode.org/wiki/Pernicious_numbers> |
| `recamans_sequence.bas`       | <https://rosettacode.org/wiki/Recaman%27s_sequence> |
| `roman_numerals_encode.bas`   | <https://rosettacode.org/wiki/Roman_numerals/Encode> |
| `sum_digits_of_an_integer.bas`| <https://rosettacode.org/wiki/Sum_digits_of_an_integer> |
| `sum_of_a_series.bas`         | <https://rosettacode.org/wiki/Sum_of_a_series> |
| `tokenize_a_string.bas`       | <https://rosettacode.org/wiki/Tokenize_a_string> |
| `towers_of_hanoi.bas`         | <https://rosettacode.org/wiki/Towers_of_Hanoi> |
| `primorial_numbers.bas`       | <https://rosettacode.org/wiki/Primorial_numbers> |
| `permutations.bas`            | <https://rosettacode.org/wiki/Permutations> |
| `middle_three_digits.bas`     | <https://rosettacode.org/wiki/Middle_three_digits> |
| `leap_year.bas`               | <https://rosettacode.org/wiki/Leap_year> |
| `rot_13.bas`                  | <https://rosettacode.org/wiki/Rot-13> |

## Running

```
bin/sb.exe bas/rosetta/perfect_numbers.bas
```

`pascals_triangle.bas` prompts for the number of rows; the sort demos and a few others
end with a "press any key" pause (FreeBASIC convention). All run under the headless
`sb` build.
