# cs557-naomi-header-parser

Final project for CS557 - Functional Programming - Summer 2020 (Pandemic Days)

Simple header parser, using Cereal to parse bytes

# Research

Thanks go to [DragonMinded](https://github.com/DragonMinded/netboot/blob/trunk/docs/naomi.md) for documenting the Naomi header format so well.

# Build
```
cabal build
```

# Play
```
cabal repl
```

# Test

This one's the weird one. The tests are only demonstrating that the library functions work, so they are side-effect-only/output-only.

Run with the following flag to make the test runner show its execution output.

```
cabal new-test --test-show-details=streaming
```

# Thanks

Thanks to Katie Casamento for being an excellent instructor.
