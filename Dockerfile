# Lean Crafter - Railway Dockerfile
# Multi-stage build: Lean + Rust → minimal runtime

FROM ubuntu:22.04 AS builder

# Install system dependencies
RUN apt-get update && apt-get install -y \
    curl \
    git \
    build-essential \
    libssl-dev \
    pkg-config \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Install elan (Lean version manager)
RUN curl -sSfL https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh | sh -s -- -y --default-toolchain none
ENV PATH="/root/.elan/bin:${PATH}"

# Install Rust
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

WORKDIR /build

# Clone lithe framework (sibling directory expected by build.rs)
RUN git clone --depth 1 https://github.com/joshpurtell/lithe.git /build/lithe

# Copy lean-crafter source
COPY . /build/lean-crafter

# Install Lean toolchain from lean-toolchain file
WORKDIR /build/lean-crafter/lithe
RUN elan default $(cat lean-toolchain)

# Build Lean code (CrafterWeb + dependencies)
RUN lake build

# Build Rust shim (release mode)
WORKDIR /build/lean-crafter/lithe/shim
ENV LITHE_SKIP_LAKE_BUILD=1
RUN cargo build --release

# Copy Lean shared library to known location for runtime stage
RUN mkdir -p /build/runtime-libs && \
    find /root/.elan/toolchains -name "libleanshared.so" -exec cp {} /build/runtime-libs/ \;

# Runtime stage
FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \
    ca-certificates \
    libssl3 \
    && rm -rf /var/lib/apt/lists/*

# Copy Lean shared library
COPY --from=builder /build/runtime-libs/libleanshared.so /usr/local/lib/
ENV LD_LIBRARY_PATH=/usr/local/lib

# Copy the built binary
COPY --from=builder /build/lean-crafter/lithe/shim/target/release/crafter-web-shim /usr/local/bin/

# Run ldconfig to update library cache
RUN ldconfig

EXPOSE 3000

# Railway sets PORT env var
CMD ["sh", "-c", "LITHE_BIND=0.0.0.0:${PORT:-3000} /usr/local/bin/crafter-web-shim"]
