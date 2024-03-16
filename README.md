# SpinalHDL Base Project

This repository contains the hardware description of the AES accelerator as a PLIM module.

## Test-Bench
```sh
sbt "runMain aes.AESEncryptionBlockSim"
```

## Obtain the HDL

To obtain the equivalent Verilog:
```sh
sbt "runMain aes.AESEncryptionBlockVerilog"
```

To obtain the equivalent VHDL:
```sh
sbt "runMain aes.AESEncryptionBlockVhdl"
```

### Troubleshooting

If the simulation (as well as the build) fails with an error looking like
```
[info] [Progress] Verilator compilation started
[info] ../verilator/VAES__spinalWrapper.cpp:5:10: fatal error: jni.h: No such file or directory
[info]     5 | #include <jni.h>
[info]       |          ^~~~~~~
[info] compilation terminated.
```

It means that the java/open-jdk version used cannot be leveraged. Instead, you may want to use older version such as, for example, open-jdk 8.


```sh
sbt "runMain aes.AESEncryptionBlockSim" --java-home /usr/lib/jvm/java-8-openjdk-amd64/
```

## Dependencies

Make sure to have `java` (or `openjdk`), `scala`, and `sbt` install on your system.

## Repository organization

 - The hardware description is located at `hw/spinal/aes/AESEncryptionBlock.scala`
 - The test-bench is located at `hw/spinal/aes/AESEncryptionBlockSim.scala`
