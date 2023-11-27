# SpinalHDL Base Project

This repository contains the hardware description of the AES accelerator as a PLIM module.

## Test-Bench
```sh
sbt "runMain aes.AESSim"
```

## Obtain the HDL

To obtain the equivalent Verilog:
```sh
sbt "runMain aes.AESVerilog"
```

To obtain the equivalent VHDL:
```sh
sbt "runMain aes.AESVhdl"
```

## Dependencies

Make sure to have `java` (or `openjdk`), `scala`, and `sbt` install on your system.

## Repository organization

 - The hardware description is located at `hw/spinal/aes/AES.scala`
 - The test-bench is located at `hw/spinal/aes/AESSim.scala`
