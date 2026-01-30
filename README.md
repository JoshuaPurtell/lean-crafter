# Lean Crafter

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Lean 4](https://img.shields.io/badge/Lean-4.27.0-orange)](https://lean-lang.org/)

A web-playable Crafter game built entirely in **[Lean 4](https://lean-lang.org/)**, powered by the [Lithe](https://github.com/JoshuaPurtell/lithe) web framework.

> **What is Lean 4?** [Lean](https://lean-lang.org/) is a functional programming language and theorem prover developed by Microsoft Research. It combines a powerful type system with the ability to write mathematical proofs—making it possible to build software with verified correctness guarantees.

## What is this?

Crafter is a 2D survival game (inspired by Minecraft) implemented as a pure functional game engine in Lean 4. This repo packages it as a web application you can play in your browser.

**Tech stack:**
- **Game engine**: Pure Lean 4 (no FFI for game logic)
- **Web framework**: [Lithe](https://github.com/JoshuaPurtell/lithe)
- **HTTP server**: Rust/Axum shim

## Quick Start

### Prerequisites

- [elan](https://github.com/leanprover/elan) (Lean version manager)
- Rust toolchain
- [Lithe](https://github.com/JoshuaPurtell/lithe) cloned as sibling directory

### Build & Run

```bash
# Clone repos
git clone https://github.com/JoshuaPurtell/lithe.git
git clone https://github.com/JoshuaPurtell/lean-crafter.git

# Build
cd lean-crafter/lithe
lake build

# Run server
cd shim
cargo run
```

Open **http://127.0.0.1:3000** in your browser.

### Controls

| Key | Action |
|-----|--------|
| Arrow keys / WASD | Move |
| Space | Interact |

## Deploy to Railway

```bash
# Via Railway CLI
railway login
railway init
railway up
```

Or connect via GitHub in the Railway dashboard.

## API

### Create Session

```http
POST /api/sessions
Content-Type: application/json

{ "seed": 12345 }
```

### WebSocket Stream

```
WS /api/sessions/:id/stream

→ { "type": "action", "action": "MoveUp" }
← { "type": "frame", "frame": "..." }
```

## Project Structure

```
lean-crafter/
├── src_lean4/           # Game engine (Lean 4)
│   ├── CrafterLean/     # World, entities, actions
│   └── lakefile.toml
├── lithe/               # Web application
│   ├── CrafterWeb.lean  # Routes, handlers
│   └── shim/            # Rust HTTP server
├── Dockerfile           # Railway deployment
└── railway.toml
```

## License

MIT
