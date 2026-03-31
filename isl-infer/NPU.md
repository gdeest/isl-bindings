# AMD XDNA2 NPU Integration

## Hardware present

```
PCI ID:    1022:17F0
Device:    AMD Strix/Krackan/Strix Halo Neural Processing Unit
Driver:    amdxdna (mainline kernel 6.17+)
Firmware:  /lib/firmware/amdnpu/17f0_10/npu.sbin
Dev node:  /dev/accel/accel0 (owner root:render, mode 0660)
```

## NPU specs (XDNA2 / Strix Point)

| Spec | Value |
|------|-------|
| Architecture | AIE-ML (AI Engine, ML variant) |
| Peak INT8 | **50 TOPS** |
| Peak INT4 | **100 TOPS** |
| AIE columns | ~4-8 (query via ioctl to confirm) |
| AIE rows | ~4 core + 1 mem + 1 shim per column |
| On-chip SRAM | ~32-64 KB per core tile, ~512 KB per mem tile |
| Total on-chip | ~2-4 MB across all tiles |
| Memory interface | Shared DDR5 (same 50 GB/s as CPU) |
| Not cache-coherent | Must flush/invalidate CPU cache before/after NPU ops |

## What 50 TOPS means for inference

For Q8_0 matvec (the hot path):
- Inner loop: 32 × INT8 multiply-accumulate per block
- Per-token compute: ~7.2 billion INT8 MACs
- At 50 TOPS: **0.14 ms per token** (compute only)
- BUT: still 151 ms for weight reads at 50 GB/s DDR5
- **NPU helps when compute-bound (batch M ≥ 5)**

| Batch M | CPU time (GEMM) | NPU time (GEMM) | Speedup |
|---------|----------------|-----------------|---------|
| 1 | 151 ms (BW) | 151 ms (BW) | 1× |
| 8 | 227 ms (compute) | 151 ms (BW) | **1.5×** |
| 16 | 454 ms (compute) | 151 ms (BW) | **3×** |
| 64 | 1816 ms (compute) | 184 ms | **10×** |

The NPU's 50 TOPS INT8 vs CPU's ~100 GOPS INT8 = **500× more INT8 compute**.
For large enough batches, the NPU keeps the DRAM pipe full while the CPU can't.

## Software stack

```
Haskell (isl-infer)
  → C FFI → ioctl on /dev/accel/accel0
  → amdxdna kernel driver
  → NPU firmware
  → AIE tiles execute pre-compiled microcode
```

### What's installed ✓
- Kernel driver: `amdxdna.ko` loaded
- Firmware: `npu.sbin` present
- Kernel header: `/usr/include/drm/amdxdna_accel.h`

### What's NOT installed ✗
- XRT (Xilinx Runtime) userspace library
- AIE compiler toolchain (for compiling compute kernels to tile microcode)
- `xclbin` format tools (for packaging compiled kernels)

## Programming model

The NPU doesn't execute arbitrary C code. It runs **pre-compiled AIE tile programs** packaged as `.xclbin` files. The flow:

```
Matrix multiply kernel (C/C++ for AIE)
  → AIE compiler (aiecc / xchesscc) → tile ELF
  → xclbin packager → .xclbin
  → XRT runtime → ioctl → NPU executes
```

### Three paths to get compute on the NPU:

**Path A: ONNX Runtime + Vitis AI EP** (highest level)
- Export the GEMM as an ONNX model
- ONNX Runtime dispatches to NPU via Vitis AI Execution Provider
- Requires: `pip install onnxruntime-vitisai`
- Pro: easiest. Con: opaque, no polyhedral control.

**Path B: MLIR-AIE compiler** (most aligned with polyhedral vision)
- Repository: github.com/Xilinx/mlir-aie
- Compiles MLIR (affine dialect) → AIE tile code
- The connection: **ISL polyhedra → MLIR affine maps → MLIR-AIE → NPU microcode**
- This IS polyhedral compilation targeting the NPU
- Pro: full control, polyhedral. Con: complex toolchain, research-grade.

**Path C: Direct ioctls with pre-built xclbin** (fastest prototype)
- Use kernel header directly from Haskell FFI
- Load a pre-compiled matmul `.xclbin` from AMD examples
- Submit via `DRM_IOCTL_AMDXDNA_EXEC_CMD`
- Pro: minimal dependencies. Con: need pre-compiled kernels.

## Ioctl interface summary

```c
// From /usr/include/drm/amdxdna_accel.h

// 1. Create hardware context (allocate AIE tiles)
DRM_IOCTL_AMDXDNA_CREATE_HWCTX   // → handle, syncobj

// 2. Allocate buffers
DRM_IOCTL_AMDXDNA_CREATE_BO      // type: SHMEM (host), CMD (pinned), DEV (device)

// 3. Get buffer info for mmap
DRM_IOCTL_AMDXDNA_GET_BO_INFO    // → map_offset, xdna_addr

// 4. Sync caches (NPU is NOT cache-coherent!)
DRM_IOCTL_AMDXDNA_SYNC_BO        // SYNC_DIRECT_TO_DEVICE or FROM_DEVICE

// 5. Submit compute command
DRM_IOCTL_AMDXDNA_EXEC_CMD       // → sequence number

// 6. Wait via DRM syncobj (standard DRM mechanism)

// 7. Query device info
DRM_IOCTL_AMDXDNA_GET_INFO       // metadata, clocks, firmware version, etc.
```

## Polyhedral connection

The tile size for the polyhedral schedule should match NPU hardware:
- **SRAM per core tile**: the L1 tile size (inner loop blocking)
- **SRAM per mem tile**: the L2 tile size (weight staging)
- **Number of columns**: the parallelism factor (outer loop unrolling)

The polyhedral domain for NPU-targeted GEMM:

```
{ [col, kb_tile, j_tile, j, kb] :
    0 <= col < N_COLUMNS,
    0 <= kb_tile < KB/SRAM_BLOCKS,
    0 <= j_tile < N/(N_COLUMNS * TILE_J),
    col*TILE_J*j_tiles <= j < min(...),
    kb_tile*SRAM_BLOCKS <= kb < min(...) }
```

Where:
- `col` maps to AIE column (hardware parallelism)
- `kb_tile` maps to weight DMA transfers (staged to SRAM)
- `j_tile` maps to output tiling within a column
- `j, kb` are the point loops inside the tile

The schedule determines when weights are DMA'd to each column's SRAM.
This is the classic double-buffering pipeline:
- Tile T: compute using SRAM buffer A, DMA weights for T+1 into buffer B
- Tile T+1: compute using buffer B, DMA for T+2 into buffer A

ISL can model this as a multi-statement schedule with producer (DMA) and
consumer (compute) statements and a distance-1 pipeline dependency.

## Prerequisites to proceed

1. Add user to render group: `sudo usermod -aG render huginn` + re-login
2. Install XRT or use direct ioctls (see Path C above)
3. Get or compile a matmul `.xclbin` for Strix Point XDNA2
4. Run `app/NpuProbe.hs` to query actual tile counts and clock speeds

## Files

- `app/NpuProbe.hs` — Direct ioctl probe of NPU metadata (ready to run after group fix)
- `/usr/include/drm/amdxdna_accel.h` — Complete ioctl interface definition
- `/lib/firmware/amdnpu/17f0_10/npu.sbin` — NPU firmware blob

## References

- [AMD NPU kernel docs](https://docs.kernel.org/accel/amdxdna/amdnpu.html)
- [AMD XDNA driver](https://github.com/amd/xdna-driver) — kernel + XRT shim
- [Xilinx XRT](https://github.com/Xilinx/XRT) — runtime library
- [MLIR-AIE](https://github.com/Xilinx/mlir-aie) — MLIR compiler for AIE tiles
- [Ryzen AI SDK](https://ryzenai.docs.amd.com/en/latest/) — AMD's official SDK
- [TINA project](https://www.hackster.io/tina/) — non-NN algorithms on AMD NPU
