# Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors
# SPDX-License-Identifier: LGPL-3.0-or-later
FROM ubuntu:24.04

# Keep native repositories restricted to the host architecture. RISC-V
# packages live in Ubuntu Ports even when the Docker host is x86-64.
RUN set -eu; \
    native="$(dpkg --print-architecture)"; \
    sed -i "/^Types:/a Architectures: $native" /etc/apt/sources.list.d/ubuntu.sources; \
    dpkg --add-architecture riscv64; \
    printf '%s\n' \
      'deb [arch=riscv64 signed-by=/usr/share/keyrings/ubuntu-archive-keyring.gpg] http://ports.ubuntu.com/ubuntu-ports noble main universe' \
      'deb [arch=riscv64 signed-by=/usr/share/keyrings/ubuntu-archive-keyring.gpg] http://ports.ubuntu.com/ubuntu-ports noble-updates main universe' \
      'deb [arch=riscv64 signed-by=/usr/share/keyrings/ubuntu-archive-keyring.gpg] http://ports.ubuntu.com/ubuntu-ports noble-security main universe' \
      > /etc/apt/sources.list.d/riscv64.list; \
    apt-get update; \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      gcc-riscv64-linux-gnu libc6-dev-riscv64-cross qemu-user \
      pkg-config libffi-dev:riscv64; \
    rm -rf /var/lib/apt/lists/*
