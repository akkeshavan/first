# Backlog

Specifications for features to be implemented later. Each spec is self-contained but may depend on others (e.g. Binary file I/O depends on ByteArray).

| Spec | Description | Dependencies |
|------|-------------|--------------|
| [SPEC-ByteArray](SPEC-ByteArray.md) | Built-in `ByteArray` type (byte buffer) | — |
| [SPEC-BitwiseOperators](SPEC-BitwiseOperators.md) | Bitwise operators `&`, `\|`, `^`, `~`, `<<`, `>>` on Int | — |
| [SPEC-BinaryFileIO](SPEC-BinaryFileIO.md) | `readFileBytes` / `writeFileBytes` for binary files | ByteArray |
| [SPEC-HexBinaryPrint](SPEC-HexBinaryPrint.md) | Print in hex/binary: `intToHex`, `intToBinary`, optional parsing and ArrayBuf dump | — (ArrayBuf optional) |
| [SPEC-HigherKindedTypes](SPEC-HigherKindedTypes.md) | Higher-kinded type params: `F : * -> *`, e.g. `interface Functor<F : * -> *>` | — |

Implement in any order for ByteArray and BitwiseOperators; implement Binary file I/O after ByteArray. Hex/binary print can follow BitwiseOperators. Higher-kinded types are implemented (Option B: explicit `* -> *` annotation).
