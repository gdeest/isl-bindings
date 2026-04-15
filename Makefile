# Makefile for isl-infer — quick targets for common model runs
#
# Best known B=1 settings (2026-04-03):
#   Strategy: original (scalar fused layer, no panel overhead)
#   KV cache: q8 (allows larger batch/context without OOM)
#   Context:  4096

# Model paths (override with: make mistral MODELS=~/my/models)
MODELS ?= ~/models
MISTRAL ?= $(MODELS)/Mistral-7B-Instruct-v0.3-Q8_0.gguf
LLAMA   ?= $(MODELS)/llama-3.1-8b-q8.gguf
DRAFT   ?= $(MODELS)/llama-3.2-1b-q8.gguf
QWEN_MOE ?= $(MODELS)/Qwen3-Coder-30B-A3B-Instruct-Q4_K_M.gguf

# Best B=1 defaults
STRATEGY ?= original
KVCACHE  ?= float32  # Q8 KV pending SARE subsystem (Phase 7D)
CONTEXT  ?= 4096
EXTRA    ?=

CABAL_RUN = nix develop --command cabal run exe:isl-infer --
BEST_FLAGS = --strategy $(STRATEGY) --kvcache $(KVCACHE) --context $(CONTEXT) $(EXTRA)

.PHONY: mistral llama qwen-moe mistral-f32 llama-f32 mistral-draft build bench vs-llama vs-llama-mistral vs-llama-llama vs-llama-qwen download-qwen

# Best B=1: Mistral 7B
mistral:
	taskset -c 0-9 $(CABAL_RUN) $(MISTRAL) $(BEST_FLAGS)

# Best B=1: Llama 3.1 8B
llama:
	taskset -c 0-9 $(CABAL_RUN) $(LLAMA) $(BEST_FLAGS)

# MoE: Qwen3-Coder-30B-A3B (128 experts, top-8, 3.3B active, Q4_K_M)
qwen-moe:
	taskset -c 0-9 $(CABAL_RUN) $(QWEN_MOE) $(BEST_FLAGS)

# Float32 KV (baseline comparison)
mistral-f32:
	taskset -c 0-9 $(CABAL_RUN) $(MISTRAL) --strategy original --kvcache float32 --context $(CONTEXT)

llama-f32:
	taskset -c 0-9 $(CABAL_RUN) $(LLAMA) --strategy original --kvcache float32 --context $(CONTEXT)

# Speculative decoding with Llama 3.2 1B draft
mistral-draft:
	taskset -c 0-9 $(CABAL_RUN) $(MISTRAL) $(BEST_FLAGS) --draft $(DRAFT)

llama-draft:
	taskset -c 0-9 $(CABAL_RUN) $(LLAMA) $(BEST_FLAGS) --draft $(DRAFT)

# Build only (no run)
build:
	nix develop --command cabal build isl-infer

# GEMM benchmark
bench:
	taskset -c 0-9 nix develop --command cabal run exe:gemm-bench

# Head-to-head: isl-infer vs llama.cpp (requires nix develop shell)
vs-llama-mistral:
	nix develop --command bash ./bench-vs-llama.sh $(MISTRAL) --context $(CONTEXT)

vs-llama-llama:
	nix develop --command bash ./bench-vs-llama.sh $(LLAMA) --context $(CONTEXT)

vs-llama-qwen:
	nix develop --command bash ./bench-vs-llama.sh $(QWEN_MOE) --context $(CONTEXT)

vs-llama: vs-llama-mistral

# Download Qwen3-Coder MoE model (~18.6 GB)
download-qwen:
	wget -c -O $(QWEN_MOE) "https://huggingface.co/unsloth/Qwen3-Coder-30B-A3B-Instruct-GGUF/resolve/main/Qwen3-Coder-30B-A3B-Instruct-Q4_K_M.gguf"
